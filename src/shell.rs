use super::{
    BLANK_COLOR, BRACKET_COLORS, CLASS_COLOR, COMMENT_COLOR, FUNCTION_COLOR, INFORM_ERR_COLOR,
    INIT_CMDS, KEY1_COLOR, KEY2_COLOR, MSG, PROMPT1, PROMPT1_ERR, PROMPT1_OK, PROMPT2, PROMPT2_OK,
    STRING_COLOR, SYMBOL_COLOR, TERMINATE_N, UNKNOWN_COLOR, args,
};
use alloc::ffi::CString;
use anstyle::{AnsiColor, Style};
use core::{fmt, iter::once, marker::PhantomData, ops::Range};
use pyo3::{
    exceptions::{PyBaseException, PySystemExit},
    prelude::*,
    types::IntoPyDict,
};
use ruff_python_ast::{Mod, Stmt};
use ruff_python_parser::{
    LexicalErrorType, Mode, ParseError, ParseErrorType, Parsed, TokenKind, parse_unchecked,
};
use ruff_text_size::{Ranged, TextRange};
use rustyline::{
    Cmd, Editor, EventHandler, Helper, KeyCode, KeyEvent, Modifiers, Movement, Parser,
    completion::Completer,
    error::ReadlineError,
    highlight::{DisplayOnce, Highlighter, Style as _, StyledBlocks},
    hint::Hinter,
    history::DefaultHistory,
    validate::{ValidationContext, ValidationResult, Validator},
};
use std::{fs::File, io::Read, path::PathBuf};
use thiserror::Error;

#[inline]
pub(super) fn run(py: Python<'_>, args: args::Args) -> ExitMsg {
    match args.mode {
        args::Mode::InteractiveShell => {
            println!("{MSG}");
            ExitMsg {
                inner: run_shell(py, INIT_CMDS),
                path: None,
            }
        }
        args::Mode::ExecFile(file_path) => ExitMsg {
            inner: if args.flag.quiet {
                quiet_exec_file(py, &file_path)
            } else {
                exec_file(py, &file_path)
            },
            path: Some(file_path.into()),
        },
        args::Mode::ExecModule(modele_name) => ExitMsg {
            inner: run_module(py, &modele_name),
            path: None,
        },
        args::Mode::Command(cmd) => ExitMsg {
            inner: run_command(py, &cmd),
            path: None,
        },
    }
}

#[derive(Debug)]
pub(super) struct ExitMsg {
    pub(super) inner: Result<(), ExecErr>,
    pub(super) path: Option<PathBuf>,
}

#[derive(Error, Debug)]
pub(super) struct ExitCode(i32);
impl fmt::Display for ExitCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Error, Debug)]
#[expect(variant_size_differences)]
pub(super) enum ExecErr {
    #[error("Exit {0}")]
    Exit(#[from] ExitCode),
    #[error("python error {0}")]
    PyResult(#[from] PyErr),
    #[error("readline error {0}")]
    Readline(#[from] ReadlineError),
    #[error("io error {0}")]
    IO(#[from] std::io::Error),
    #[error("fmt error {0}")]
    Fmt(#[from] fmt::Error),
    #[error("SyntaxError:")]
    ParseError,
}

impl ExitMsg {
    #[inline]
    pub(super) fn report(self) -> i32 {
        match self.inner {
            Ok(()) => 0,
            Err(ExecErr::Exit(code)) => code.0,
            Err(ExecErr::PyResult(err)) => {
                if let Err(code) = Python::with_gil(|py| ErrDisplay { py, err }.report()) {
                    code.0
                } else {
                    1
                }
            }
            Err(ExecErr::Readline(e)) => {
                println!("ShellError: {}", e);
                1
            }
            Err(ExecErr::Fmt(e)) => {
                println!("{}", e);
                1
            }
            Err(ExecErr::ParseError) => 1,
            Err(ExecErr::IO(e)) => {
                if let Some(path) = self.path {
                    println!("{}: {}", path.display(), e);
                } else {
                    println!("{}", e);
                }
                1
            }
        }
    }
}

struct MyHelper {
    segments: Vec<(bool, Range<usize>)>,
    parsed: Parsed<Mod>,
    bracket_level_diff: i32,
    need_render: bool,
    exec_error: bool,
    parse_error: Option<ParseError>,
}

impl MyHelper {
    #[inline]
    fn new() -> Self {
        Self {
            segments: Vec::new(),
            parsed: parse_unchecked("", Mode::Module.into()),
            exec_error: false,
            need_render: true,
            bracket_level_diff: 0,
            parse_error: None,
        }
    }
}
#[inline]
fn segments(parsed: &Parsed<Mod>) -> Vec<(bool, Range<usize>)> {
    if parsed.tokens().is_empty() {
        return vec![(false, 0..0)];
    }
    match parsed.syntax() {
        Mod::Module(module) => {
            let mut sat_ranges = module.body.iter().map(|body| {
                (
                    matches!(body, Stmt::Expr(_)),
                    <_ as Into<Range<usize>>>::into(body.range()),
                )
            });
            let mut sat_range = sat_ranges.next();
            let mut block_start = 0;
            parsed
                .tokens()
                .iter()
                .filter_map(|token| match token.kind() {
                    TokenKind::Newline | TokenKind::NonLogicalNewline => {
                        Some(<_ as Into<Range<usize>>>::into(token.range()))
                    }
                    _ => None,
                })
                .chain(parsed.tokens().last().and_then(|last| {
                    if last.kind().eq(&TokenKind::Comment) {
                        let last_idx = last.end().to_usize();
                        Some(last_idx..last_idx)
                    } else {
                        None
                    }
                }))
                .filter_map(|newline_range| {
                    if let Some((is_expr, _sat_range)) = &sat_range {
                        if newline_range.start >= _sat_range.end {
                            let is_expr = *is_expr;
                            let range = block_start..newline_range.start;
                            sat_range = sat_ranges.next();
                            block_start = newline_range.end;
                            Some((is_expr, range))
                        } else if newline_range.end <= _sat_range.start {
                            let range = block_start..newline_range.start;
                            block_start = newline_range.end;
                            Some((false, range))
                        } else {
                            None
                        }
                    } else {
                        let range = block_start..newline_range.start;
                        block_start = newline_range.end;
                        Some((false, range))
                    }
                })
                .collect()
        }
        Mod::Expression(expr) => vec![(true, expr.range.into())],
    }
}
impl Parser for MyHelper {
    fn segments(&mut self) -> &mut Vec<(bool, Range<usize>)> {
        &mut self.segments
    }
}
impl Helper for MyHelper {
    #[inline]
    fn update_after_edit(&mut self, lines: &str, _pos: usize, _forced_refresh: bool) {
        self.need_render = true;
        let parsed = parse_unchecked(lines, Mode::Module.into());
        self.bracket_level_diff =
            parsed
                .tokens()
                .iter()
                .fold(0, |level, token| match token.kind() {
                    TokenKind::Lpar | TokenKind::Lsqb | TokenKind::Lbrace => level + 1,
                    TokenKind::Rpar | TokenKind::Rsqb | TokenKind::Rbrace => level - 1,
                    _ => level,
                });
        self.parse_error = None;
        for error in parsed.errors() {
            match &error.error {
                ParseErrorType::OtherError(s) => {
                    if s.starts_with("Expected an indented") {
                    } else {
                        self.parse_error = Some(error.clone());
                        break;
                    }
                }
                ParseErrorType::Lexical(
                    LexicalErrorType::Eof | LexicalErrorType::LineContinuationError,
                ) => {}
                _ => {
                    self.parse_error = Some(error.clone());
                    break;
                }
            }
        }
        let seg = self.segments();
        seg.clear();
        let segments = segments(&parsed);
        let multi_input = segments.len() > 1;
        seg.extend(segments.into_iter().rev());
        if multi_input {
            seg.push((false, 0..0));
        }
        self.parsed = parsed;
    }
    #[inline]
    fn continuation_prompt_width<'b, 's: 'b, 'p: 'b>(&'s self, _prompt: &'p str) -> usize {
        PROMPT1.len()
    }
}

impl Validator for MyHelper {
    #[inline]
    fn validate(
        &mut self,
        _ctx: &mut ValidationContext<'_>,
    ) -> rustyline::Result<ValidationResult> {
        let mut indent = self.bracket_level_diff.try_into().unwrap_or(0);
        let mut incomplete = false;
        let mut tokens_rev = self.parsed.tokens().iter().rev();
        while let Some(token) = tokens_rev.next() {
            let (kind, range) = token.as_tuple();
            match kind {
                TokenKind::Dedent => {
                    indent += 1;
                    incomplete = true;
                }
                TokenKind::NonLogicalNewline | TokenKind::Newline => {
                    if incomplete {
                        incomplete = range.len().to_u32() == 0
                    }
                    break;
                }
                _ => break,
            }
        }
        for error in self.parsed.errors() {
            match &error.error {
                ParseErrorType::OtherError(s) => {
                    if s.starts_with("Expected an indented") {
                        incomplete = true;
                        indent += 1;
                        break;
                    }
                }
                ParseErrorType::Lexical(
                    LexicalErrorType::Eof | LexicalErrorType::LineContinuationError,
                ) => {
                    incomplete = true;
                    break;
                }
                _ => {}
            }
        }
        if incomplete {
            Ok(ValidationResult::Incomplete(indent * 2))
        } else {
            if let Some(parse_error) = &self.parse_error {
                Ok(ValidationResult::Invalid(Some(format!(
                    "\nSyntaxError: {}",
                    parse_error.error,
                ))))
            } else {
                Ok(ValidationResult::Valid(None))
            }
        }
    }
}
impl Completer for MyHelper {
    type Candidate = String;
}
impl Hinter for MyHelper {
    type Hint = String;
    #[inline]
    fn hint(&mut self, line: &str, pos: usize, ctx: &rustyline::Context<'_>) -> Option<String> {
        use rustyline::history::SearchDirection;
        if line.is_empty() || pos < line.len() {
            return None;
        }
        let start = if ctx.history_index() == ctx.history().len() {
            ctx.history_index().saturating_sub(1)
        } else {
            ctx.history_index()
        };
        if let Some(sr) = ctx
            .history()
            .starts_with(line, start, SearchDirection::Reverse)
            .unwrap_or(None)
        {
            if sr.entry == line {
                return None;
            }
            return Some(sr.entry[pos..].to_owned());
        }
        None
    }
}

impl Highlighter for MyHelper {
    #[inline]
    fn highlight_char(&mut self, _line: &str, _pos: usize, _forced: bool) -> bool {
        self.need_render
    }
    #[inline]
    fn highlight<'b, 's: 'b, 'l: 'b>(
        &'s mut self,
        line: &'l str,
        _pos: usize,
    ) -> impl 'b + DisplayOnce {
        let segment = self.segments().last().unwrap().1.clone();
        let after_segment = &line[(segment.end - segment.start)..];
        if segment.end == line.len() {
            self.need_render = false;
        }
        let tokens = self.parsed.tokens();
        let bracket_level_diff = self.bracket_level_diff;
        let error_location = self.parse_error.as_ref().map(|e| e.location);
        let segment = TextRange::new((segment.start as u32).into(), (segment.end as u32).into());

        let mut last_end = 0;
        let mut bracket_level: i32 = 0;
        let mut last_kind = TokenKind::Name;
        let iter = tokens
            .iter()
            .enumerate()
            .filter_map(move |(idx, token)| {
                let (kind, range) = token.as_tuple();
                if range.len().to_u32() == 0 || !segment.contains_range(range) {
                    None
                } else {
                    Some((
                        idx,
                        kind,
                        range.sub_start(segment.start()).sub_end(segment.start()),
                    ))
                }
            })
            .flat_map(move |(idx, kind, range)| {
                let term = match kind {
                    TokenKind::Newline | TokenKind::NonLogicalNewline => PROMPT2_OK,
                    _ => &line[range],
                };
                let mut style = match kind {
                    TokenKind::Name => match last_kind {
                        TokenKind::Def => Style::new().fg_color(Some(FUNCTION_COLOR)),
                        TokenKind::Class => Style::new().fg_color(Some(CLASS_COLOR)),
                        _ => match term {
                            "self" | "super" => Style::new().fg_color(Some(KEY1_COLOR)),
                            _ => {
                                if term.chars().all(|c| c.is_ascii_uppercase()) {
                                    Style::new().fg_color(Some(KEY1_COLOR))
                                } else {
                                    if let Some(next_token) = tokens.get(idx + 1) {
                                        match next_token.kind() {
                                            TokenKind::Lpar => {
                                                Style::new().fg_color(Some(FUNCTION_COLOR))
                                            }
                                            _ => Style::new().fg_color(Some(BLANK_COLOR)),
                                        }
                                    } else {
                                        Style::new().fg_color(Some(BLANK_COLOR))
                                    }
                                }
                            }
                        },
                    },
                    TokenKind::Lpar | TokenKind::Lsqb | TokenKind::Lbrace => {
                        let style = Style::new().fg_color(Some(
                            if bracket_level_diff <= bracket_level + 1 {
                                TryInto::<usize>::try_into(bracket_level)
                                    .map_or(UNKNOWN_COLOR, |level| {
                                        BRACKET_COLORS[level % BRACKET_COLORS.len()]
                                    })
                            } else {
                                UNKNOWN_COLOR
                            },
                        ));
                        bracket_level += 1;
                        style
                    }
                    TokenKind::Rpar | TokenKind::Rsqb | TokenKind::Rbrace => {
                        bracket_level -= 1;
                        let style = Style::new().fg_color(Some(
                            TryInto::<usize>::try_into(bracket_level)
                                .map_or(UNKNOWN_COLOR, |level| {
                                    BRACKET_COLORS[level % BRACKET_COLORS.len()]
                                }),
                        ));
                        style
                    }
                    TokenKind::From
                    | TokenKind::Import
                    | TokenKind::Def
                    | TokenKind::Class
                    | TokenKind::Equal
                    | TokenKind::EqEqual
                    | TokenKind::NotEqual
                    | TokenKind::LessEqual
                    | TokenKind::GreaterEqual
                    | TokenKind::DoubleStarEqual
                    | TokenKind::PlusEqual
                    | TokenKind::MinusEqual
                    | TokenKind::StarEqual
                    | TokenKind::SlashEqual
                    | TokenKind::PercentEqual
                    | TokenKind::AmperEqual
                    | TokenKind::VbarEqual
                    | TokenKind::CircumflexEqual
                    | TokenKind::LeftShiftEqual
                    | TokenKind::RightShiftEqual
                    | TokenKind::DoubleSlash
                    | TokenKind::DoubleSlashEqual
                    | TokenKind::ColonEqual
                    | TokenKind::At
                    | TokenKind::AtEqual
                    | TokenKind::Elif
                    | TokenKind::Else
                    | TokenKind::For
                    | TokenKind::If
                    | TokenKind::In
                    | TokenKind::Plus
                    | TokenKind::Minus
                    | TokenKind::Star
                    | TokenKind::Slash
                    | TokenKind::Vbar
                    | TokenKind::Amper
                    | TokenKind::Less
                    | TokenKind::Greater
                    | TokenKind::Percent
                    | TokenKind::Tilde
                    | TokenKind::CircumFlex
                    | TokenKind::LeftShift
                    | TokenKind::RightShift
                    | TokenKind::Dot
                    | TokenKind::DoubleStar
                    | TokenKind::As
                    | TokenKind::Assert
                    | TokenKind::Async
                    | TokenKind::Await
                    | TokenKind::Break
                    | TokenKind::Continue
                    | TokenKind::Del
                    | TokenKind::Except
                    | TokenKind::Global
                    | TokenKind::Is
                    | TokenKind::Lambda
                    | TokenKind::Finally
                    | TokenKind::Nonlocal
                    | TokenKind::Not
                    | TokenKind::Pass
                    | TokenKind::Raise
                    | TokenKind::Return
                    | TokenKind::Try
                    | TokenKind::While
                    | TokenKind::With
                    | TokenKind::Yield
                    | TokenKind::Case
                    | TokenKind::And
                    | TokenKind::Or
                    | TokenKind::Match => Style::new().fg_color(Some(KEY2_COLOR)),
                    TokenKind::String
                    | TokenKind::FStringStart
                    | TokenKind::FStringMiddle
                    | TokenKind::FStringEnd => Style::new().fg_color(Some(STRING_COLOR)),
                    TokenKind::Int
                    | TokenKind::Float
                    | TokenKind::Complex
                    | TokenKind::Ellipsis
                    | TokenKind::True
                    | TokenKind::False
                    | TokenKind::None
                    | TokenKind::Type => Style::new().fg_color(Some(KEY1_COLOR)),
                    TokenKind::Comment => Style::new().fg_color(Some(COMMENT_COLOR)).italic(),
                    TokenKind::Comma
                    | TokenKind::Unknown
                    | TokenKind::IpyEscapeCommand
                    | TokenKind::Exclamation
                    | TokenKind::Colon => Style::new().fg_color(Some(BLANK_COLOR)),
                    TokenKind::Indent
                    | TokenKind::Dedent
                    | TokenKind::Newline
                    | TokenKind::NonLogicalNewline
                    | TokenKind::EndOfFile => Style::new(),
                    TokenKind::Semi | TokenKind::Question | TokenKind::Rarrow => {
                        Style::new().fg_color(Some(SYMBOL_COLOR)).italic()
                    }
                };
                if let Some(error_location) = error_location {
                    if error_location.contains_range(range) {
                        style = style
                            .underline()
                            .underline_color(Some(AnsiColor::BrightRed.into()));
                    }
                }
                last_kind = kind;
                let out = once((style, &line[last_end..range.start().to_usize()]))
                    .chain(once((style, term)));
                last_end = range.end().to_usize();
                out
            })
            .chain({
                let mut v: Vec<_> = after_segment
                    .split('\n')
                    .flat_map(|s| [(Style::new(), s), (Style::new(), PROMPT2)])
                    .collect();
                _ = v.pop();
                v
            })
            .filter(|(_, term)| !term.is_empty());
        StyledBlocks::new(iter)
    }
    #[inline]
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s mut self,
        prompt: &'p str,
        default: bool,
    ) -> impl 'b + DisplayOnce {
        if default {
            if self.exec_error {
                PROMPT1_ERR
            } else {
                PROMPT1_OK
            }
        } else {
            prompt
        }
    }
    #[inline]
    fn highlight_hint<'b, 's: 'b, 'h: 'b>(&'s mut self, hint: &'h str) -> impl 'b + DisplayOnce {
        struct Lines<'l, 'e> {
            s: &'l str,
            style: Style,
            parse_error: &'e Option<ParseError>,
            _marker: PhantomData<&'l ()>,
        }
        impl<'l, 'e> DisplayOnce for Lines<'l, 'e> {
            fn fmt<W: fmt::Write>(self, f: &mut W) -> fmt::Result {
                if let Some(parse_error) = self.parse_error.as_ref() {
                    if self.s.starts_with("\nSyntaxError") {
                        return write!(f, "\n{}", ParseErrorDisplay { err: parse_error });
                    }
                }
                let mut iter = self.s.split('\n');
                if let Some(first_line) = iter.next() {
                    write!(f, "{}", self.style.start())?;
                    write!(f, "{}", first_line)?;
                    iter.map(|line| write!(f, "{}{}", PROMPT2, line))
                        .collect::<fmt::Result>()?;
                    write!(f, "{}", self.style.end())
                } else {
                    write!(f, "{}", self.style.end())
                }
            }
        }
        Lines {
            s: hint,
            style: Style::new().fg_color(Some(AnsiColor::BrightBlack.into())),
            _marker: PhantomData,
            parse_error: &self.parse_error,
        }
    }
}

struct ParseErrorDisplay<'a> {
    err: &'a ParseError,
}
impl<'a> fmt::Display for ParseErrorDisplay<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let style = Style::new().fg_color(Some(INFORM_ERR_COLOR)).bold();
        let msg_style = Style::new().fg_color(Some(INFORM_ERR_COLOR));
        return write!(
            f,
            "{}SyntaxError{}: {}{}{}",
            style.start(),
            style.end(),
            msg_style.start(),
            self.err.error,
            msg_style.end(),
        );
    }
}

#[inline]
pub(super) fn run_shell(py: Python<'_>, init_cmds: &str) -> Result<(), ExecErr> {
    let mut rl = Editor::<MyHelper, DefaultHistory>::new(MyHelper::new())?;
    _ = rl.bind_sequence(
        KeyEvent(KeyCode::Tab, Modifiers::NONE),
        EventHandler::Simple(Cmd::Indent(Movement::ForwardChar(4))),
    );
    _ = rl.bind_sequence(
        KeyEvent(KeyCode::BackTab, Modifiers::NONE),
        EventHandler::Simple(Cmd::Dedent(Movement::BackwardChar(4))),
    );
    _ = rl.bind_sequence(
        KeyEvent(KeyCode::Char('s'), Modifiers::CTRL),
        EventHandler::Simple(Cmd::Newline),
    );
    let mut terminate_count: u8 = 0;
    let mut is_init = true;
    loop {
        let (is_expr, input) = {
            match if is_init {
                is_init = false;
                rl.readline_with_initial(PROMPT1, (init_cmds, ""))
            } else {
                rl.readline(PROMPT1)
            } {
                Ok(input) => input,
                Err(ReadlineError::Interrupted | ReadlineError::Eof) => {
                    if terminate_count >= TERMINATE_N {
                        return Ok(());
                    }
                    println!(
                        "\nNeed {} interrupt to exit..",
                        TERMINATE_N - terminate_count
                    );
                    terminate_count += 1;
                    continue;
                }
                Err(err) => {
                    println!("Error: {:?}", err);
                    return Err(ExecErr::Readline(err));
                }
            }
        };
        match input.as_str() {
            "clear" | "clear()" => {
                rl.clear_screen()?;
                rl.helper_mut().exec_error = false;
                continue;
            }
            "exit" | "quit" | "exit()" | "quit()" => {
                std::process::exit(0);
            }
            _ => {}
        }
        terminate_count = 0;
        rl.helper_mut().exec_error = false;
        let code = CString::new(input.as_str()).unwrap();
        if is_expr {
            match py.eval(&code, None, None) {
                Ok(res) => {
                    if !res.is_none() {
                        println!("{res}");
                    }
                }
                Err(err) => {
                    rl.need_input();
                    ErrDisplay { py, err }.report()?;
                    rl.helper_mut().exec_error = true;
                }
            }
        } else {
            if let Err(err) = py.run(&code, None, None) {
                rl.need_input();
                ErrDisplay { py, err }.report()?;
                rl.helper_mut().exec_error = true;
            }
        }
        _ = rl.add_history_entry(input)?;
    }
}

struct ErrDisplay<'py> {
    py: Python<'py>,
    err: PyErr,
}
impl<'py> ErrDisplay<'py> {
    fn report(&self) -> Result<(), ExitCode> {
        let value = self.err.value(self.py);
        struct ErrDisplayInner<'py, 'a> {
            value: &'a Bound<'py, PyBaseException>,
        }
        impl<'py, 'a> fmt::Display for ErrDisplayInner<'py, 'a> {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                let style = Style::new().fg_color(Some(INFORM_ERR_COLOR)).bold();
                let msg_style = Style::new().fg_color(Some(INFORM_ERR_COLOR));
                let type_name = self
                    .value
                    .get_type()
                    .qualname()
                    .map_err(|_| std::fmt::Error)?;
                write!(
                    f,
                    "{}{type_name}{}: {}",
                    style.start(),
                    style.end(),
                    msg_style.start()
                )?;
                if let Ok(s) = self.value.str() {
                    write!(f, "{}", &s.to_string_lossy())?;
                } else {
                    write!(f, "<exception str() failed>")?;
                }
                write!(f, "{}", msg_style.end())?;
                Ok(())
            }
        }
        match value.downcast_exact::<PySystemExit>() {
            Ok(exit) => Err(ExitCode(exit.getattr("code").map_or(0, |code| {
                if code.is_none() {
                    0
                } else {
                    code.extract::<i32>().unwrap_or(0)
                }
            }))),
            Err(_) => {
                println!("{}", ErrDisplayInner { value });
                Ok(())
            }
        }
    }
}

#[inline]
fn run_module(py: Python<'_>, modele_name: &str) -> Result<(), ExecErr> {
    let runpy = PyModule::import(py, "runpy")?;
    match runpy.call_method(
        "run_module",
        (modele_name,),
        Some(&[("run_name", "__main__"), ("alter_sys", "true")].into_py_dict(py)?),
    ) {
        Ok(_) => Ok(()),
        Err(e) => {
            if "SystemExit: 0" == &(e.to_string()) {
                Ok(())
            } else {
                Err(e.into())
            }
        }
    }
}

#[inline]
fn run_command(py: Python<'_>, cmd: &str) -> Result<(), ExecErr> {
    let code = CString::new(cmd).unwrap();
    py.run(&code, None, None).map_err(Into::into)
}

#[inline]
fn exec_file(py: Python<'_>, file_path: &str) -> Result<(), ExecErr> {
    struct Lines<'s> {
        input: &'s str,
    }
    impl<'s> fmt::Display for Lines<'s> {
        #[inline]
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let mut iter = self.input.split('\n');
            if let Some(first_line) = iter.next() {
                write!(f, "{PROMPT1}{}", first_line)?;
                iter.map(|line| write!(f, "{PROMPT2}{line}"))
                    .collect::<fmt::Result>()?;
            }
            Ok(())
        }
    }
    let mut file = File::open(file_path)?;
    let mut buf = String::new();
    _ = file.read_to_string(&mut buf)?;
    let parsed = parse_unchecked(&buf, Mode::Module.into());
    if let Some(error) = parsed.errors().first() {
        print_error_lines(&buf, file_path, error.location);
        println!("{}", ParseErrorDisplay { err: error });
        return Err(ExecErr::ParseError);
    }
    for (is_expr, range) in segments(&parsed) {
        let input = &buf[range];
        let code = CString::new(input).unwrap();
        println!("{}", Lines { input });
        if is_expr {
            let res = py.eval(&code, None, None)?;
            if !res.is_none() {
                println!("{res}");
            }
        } else {
            _ = py.run(&code, None, None)?;
        }
    }
    Ok(())
}

#[inline]
fn quiet_exec_file(py: Python<'_>, file_path: &str) -> Result<(), ExecErr> {
    let bytes = std::fs::read(file_path)?;
    let code = CString::new(bytes).unwrap();
    py.run(&code, None, None)?;
    Ok(())
}

fn print_error_lines(input: &str, file_path: &str, error_location: TextRange) {
    struct LineInfo<'a> {
        content: &'a str,
        start_offset: usize,
        end_offset: usize,
    }

    fn collect_lines_with_offsets(text: &str) -> Vec<LineInfo<'_>> {
        let mut lines = Vec::new();
        let mut current_start = 0;
        for line in text.split('\n') {
            let line_len = line.len();
            let start_offset = current_start;
            let end_offset = current_start + line_len;
            lines.push(LineInfo {
                content: line,
                start_offset,
                end_offset,
            });
            current_start = end_offset + 1;
        }
        lines
    }

    fn find_affected_line_range(
        lines: &[LineInfo<'_>],
        start: usize,
        end: usize,
    ) -> (usize, usize) {
        let mut first_line = None;
        let mut last_line = None;

        for (i, line) in lines.iter().enumerate() {
            if line.end_offset > start && first_line.is_none() {
                first_line = Some(i);
            }
            if line.start_offset < end {
                last_line = Some(i);
            }
        }

        let first_line = first_line.unwrap_or(0);
        let last_line = last_line.unwrap_or(0);
        (first_line, last_line)
    }

    fn print_highlighted_lines(
        file_path: &str,
        lines: &[LineInfo<'_>],
        first_line_idx: usize,
        last_line_idx: usize,
        start: usize,
        end: usize,
        shown_line_number: usize,
    ) {
        let msg_style = Style::new().fg_color(Some(INFORM_ERR_COLOR));
        for i in first_line_idx..=last_line_idx {
            let line_info = &lines[i];
            let line_num = shown_line_number + (i - first_line_idx);
            println!(
                "File {}\"{file_path}\"{}, line {}{line_num}{}",
                msg_style.start(),
                msg_style.end(),
                msg_style.start(),
                msg_style.end(),
            );
            println!("    {}", line_info.content);
            let line_len = line_info.content.len();
            let mut highlight = vec![' '; line_len];
            let intersection_start = start.max(line_info.start_offset);
            let intersection_end = end.min(line_info.end_offset);
            if intersection_end > intersection_start {
                let rel_start = intersection_start - line_info.start_offset;
                let rel_end = intersection_end - line_info.start_offset;
                for j in rel_start..rel_end {
                    if j < line_len {
                        highlight[j] = '^';
                    }
                }
            }
            if intersection_end > intersection_start {
                let highlight_str: String = highlight.into_iter().collect();
                let style = Style::new()
                    .fg_color(Some(AnsiColor::BrightRed.into()))
                    .bold();
                println!("    {}{highlight_str}{}", style.start(), style.end());
            }
        }
    }

    let start = error_location.start().to_usize();
    let end = error_location.end().to_usize();
    let lines = collect_lines_with_offsets(input);
    let (first_line, last_line) = find_affected_line_range(&lines, start, end);
    print_highlighted_lines(
        file_path,
        &lines,
        first_line,
        last_line,
        start,
        end,
        first_line + 1,
    );
}
