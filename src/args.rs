use core::fmt;
use pyo3::prelude::*;
use thiserror::Error;
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Mode {
    /// Start in interactive shell mode
    #[default]
    InteractiveShell,
    /// Execute a file with arguments
    ExecFile(String),
    /// Execute a module with arguments
    ExecModule(String),
    /// Execute a command
    Command(String),
}

#[derive(Error, Debug)]
// #[derive(PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum ArgsError {
    #[error("Unknow Flag '-{0}'")]
    UnknowShort(char),
    #[error("Unknow Flag '--{0}'")]
    UnknowLong(String),
    #[error("ExpectValue {0}")]
    ExpectValue(Arg),
    #[error("PyErr {0:?}")]
    PyErr(#[from] PyErr),
}

#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Flag {
    /// execute in quiet mode (effect in file mode)
    // -q
    pub(crate) quiet: bool,
    /// [PYTHON] isolate Python from the user's environment (implies -E and -s)
    // -I
    pub(crate) isolate: bool,
    /// [PYTHON] don't add user site directory to sys.path; also PYTHONNOUSERSITE
    // -s
    pub(crate) ignore_site: bool,
    /// [PYTHON] ignore PYTHON* environment variables (such as PYTHONPATH)
    // -E
    pub(crate) ignore_env: bool,
}
#[derive(Debug, Default, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Args {
    pub(crate) mode: Mode,
    pub(crate) flag: Flag,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum Arg {
    // -m
    Module,
    // -c
    Command,
}

impl fmt::Display for Arg {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Arg::Module => f.write_str("-m"),
            Arg::Command => f.write_str("-c"),
        }
    }
}

impl Args {
    fn version() -> ! {
        println!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"),);
        std::process::exit(0)
    }
    fn help(e: Option<ArgsError>) -> ! {
        let code = if let Some(e) = e {
            println!("Error: {e}\n");
            2
        } else {
            println!("ptpyrs {} by Junzhuo\n", env!("CARGO_PKG_VERSION"));
            0
        };
        println!(
            "Usage: {} [option] ... [-c cmd | -m mod | file | -] [arg] ...

Arguments:
    [file]    [PYTHON] program read from script file
    [ - ]     [PYTHON] program read from stdin (default; interactive mode if a tty)
    [arg] ... [PYTHON] arguments passed to program in sys.argv[1:]

Options:
    -q, --quiet    execute in quiet mode (effect in file mode)
    -I             [PYTHON] [TODO] isolate Python from the user's environment (implies -E and -s)
    -s             [PYTHON] [TODO] don't add user site directory to sys.path; also PYTHONNOUSERSITE
    -E             [PYTHON] [TODO] ignore PYTHON* environment variables (such as PYTHONPATH)
    -m <mod>       [PYTHON] run library module as a script (terminates option list)
    -c <cmd>       [PYTHON] program passed in as string (terminates option list)
    -h, --help     Print help
    -V, --version  Print version",
            env!("CARGO_PKG_NAME")
        );
        std::process::exit(code)
    }
    fn match_long(s: &str, flag: &mut Flag, last_arg: &mut Option<Arg>) -> Result<(), ArgsError> {
        if let Some(last) = last_arg {
            Err(ArgsError::ExpectValue(*last))
        } else {
            match s {
                "quiet" => {
                    flag.quiet = true;
                    *last_arg = None;
                    Ok(())
                }
                "help" => {
                    Self::help(None);
                }
                "version" => {
                    Self::version();
                }
                _ => Err(ArgsError::UnknowLong(s.to_owned())),
            }
        }
    }

    fn match_short(c: char, flag: &mut Flag, last_arg: &mut Option<Arg>) -> Result<(), ArgsError> {
        if let Some(last) = last_arg {
            Err(ArgsError::ExpectValue(*last))
        } else {
            match c {
                'm' => {
                    *last_arg = Some(Arg::Module);
                    Ok(())
                }
                'c' => {
                    *last_arg = Some(Arg::Command);
                    Ok(())
                }
                'q' => {
                    flag.quiet = true;
                    *last_arg = None;
                    Ok(())
                }
                'I' => {
                    flag.isolate = true;
                    *last_arg = None;
                    Ok(())
                }
                's' => {
                    flag.ignore_site = true;
                    *last_arg = None;
                    Ok(())
                }
                'E' => {
                    flag.ignore_env = true;
                    *last_arg = None;
                    Ok(())
                }
                'h' => {
                    Self::help(None);
                }
                'V' => {
                    Self::version();
                }
                _ => Err(ArgsError::UnknowShort(c)),
            }
        }
    }
    fn get_argv(py: Python<'_>) -> Result<Vec<String>, ArgsError> {
        Ok(PyModule::import(py, "sys")?
            .getattr("argv")?
            .extract::<Vec<String>>()?)
    }
    pub(crate) fn parse(py: Python<'_>) -> Self {
        Self::get_argv(py)
            .and_then(|argv| Self::parse_from(py, argv))
            .unwrap_or_else(|e| Self::help(Some(e)))
    }
    fn parse_from<I>(py: Python<'_>, itr: I) -> Result<Self, ArgsError>
    where
        I: IntoIterator<Item = String>,
    {
        let mut last_arg = None;
        let mut out = Args::default();
        let mut iter = itr.into_iter().skip(1);
        for arg_str in iter.by_ref() {
            let mut chars = arg_str.chars();
            match chars.next() {
                None => continue,
                Some('-') => match chars.next() {
                    None => {
                        return Err(ArgsError::UnknowShort('\0'));
                    }
                    Some('-') => {
                        Self::match_long(&arg_str[2..], &mut out.flag, &mut last_arg)?;
                    }
                    Some(c) => {
                        Self::match_short(c, &mut out.flag, &mut last_arg)?;
                        for c in chars {
                            Self::match_short(c, &mut out.flag, &mut last_arg)?;
                        }
                    }
                },
                _ => match last_arg {
                    None => {
                        out.mode = Mode::ExecFile(arg_str);
                        break;
                    }
                    Some(Arg::Module) => {
                        out.mode = Mode::ExecModule(arg_str);
                        break;
                    }
                    Some(Arg::Command) => {
                        out.mode = Mode::Command(arg_str);
                        break;
                    }
                },
            }
        }
        match &mut out.mode {
            Mode::InteractiveShell => {
                if let Some(last) = last_arg {
                    Err(ArgsError::ExpectValue(last))
                } else {
                    Ok(out)
                }
            }
            Mode::ExecFile(s) | Mode::ExecModule(s) | Mode::Command(s) => {
                let args: Vec<String> = std::iter::once(s.clone()).chain(iter).collect();
                PyModule::import(py, "sys")?.setattr("argv", args)?;
                Ok(out)
            }
        }
    }
}

// #[cfg(test)]
// mod test {
//   use super::*;
//   #[test]
//   fn short() {
//     assert_eq!(
//       Args::parse_from({
//         let args: &[&str] = &[];
//         args
//       }),
//       Ok(Args {
//         mode: Mode::InteractiveShell,
//         flag: Flag::default()
//       })
//     );
//     assert_eq!(
//       Args::parse_from(&["-qI"]),
//       Ok(Args {
//         mode: Mode::InteractiveShell,
//         flag: {
//           let mut f = Flag::default();
//           f.quiet = true;
//           f.isolate = true;
//           f
//         }
//       })
//     );
//     assert_eq!(
//       Args::parse_from(&["-qc", "print(1);", "arg1", "arg2"]),
//       Ok(Args {
//         mode: Mode::Command(
//           "print(1);".into(),
//           vec!["-c".into(), "arg1".into(), "arg2".into()]
//         ),
//         flag: {
//           let mut f = Flag::default();
//           f.quiet = true;
//           f
//         }
//       })
//     );
//     assert_eq!(
//       Args::parse_from(&["-Iqs", "run.py", "arg1", "arg2"]),
//       Ok(Args {
//         mode: Mode::ExecFile(vec!["run.py".into(), "arg1".into(), "arg2".into()]),
//         flag: {
//           let mut f = Flag::default();
//           f.quiet = true;
//           f.isolate = true;
//           f.ignore_site = true;
//           f
//         }
//       })
//     );
//     assert_eq!(Args::parse_from(&["-c"]), Err(ArgsError::ExpectValue(Arg::Command)));
//     assert_eq!(Args::parse_from(&["-m"]), Err(ArgsError::ExpectValue(Arg::Module)));
//     assert_eq!(
//       Args::parse_from(&["-cq", "arg1", "arg2"]),
//       Err(ArgsError::ExpectValue(Arg::Command))
//     );
//   }
// }
