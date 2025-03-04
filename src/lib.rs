mod args;
mod shell;

use anstyle::{AnsiColor, Color, RgbColor};
use pyo3::prelude::*;
const TERMINATE_N: u8 = 2;
const PROMPT1: &str = "pypy > ";
const PROMPT2: &str = " ... > ";
const PROMPT1_OK: &str = "\x1b[1m\x1b[38;5;39mpypy\x1b[1;32m > \x1b[m";
const PROMPT1_ERR: &str = "\x1b[1m\x1b[38;5;39mpypy\x1b[1;91m > \x1b[m";
const PROMPT2_OK: &str = "\n\x1b[1m\x1b[38;5;39m ...\x1b[1;32m > \x1b[m";
const FUNCTION_COLOR: Color = Color::Rgb(RgbColor(0xDC, 0xBD, 0xFB));
const BLANK_COLOR: Color = Color::Rgb(RgbColor(0xAD, 0xBA, 0xC7));
const CLASS_COLOR: Color = Color::Rgb(RgbColor(0xF6, 0x9D, 0x50));
const KEY1_COLOR: Color = Color::Rgb(RgbColor(0x6C, 0xB6, 0xFF));
const KEY2_COLOR: Color = Color::Rgb(RgbColor(0xF4, 0x70, 0x67));
const SYMBOL_COLOR: Color = Color::Rgb(RgbColor(0xFF, 0x93, 0x8A));
const COMMENT_COLOR: Color = Color::Rgb(RgbColor(0x76, 0x83, 0x90));
const STRING_COLOR: Color = Color::Rgb(RgbColor(0xA5, 0xD6, 0xFF));
const UNKNOWN_COLOR: Color = Color::Rgb(RgbColor(0xFF, 0x00, 0x00));
const INFORM_ERR_COLOR: Color = Color::Ansi(AnsiColor::BrightMagenta);
const BRACKET_COLORS: [Color; 3] = [
    Color::Rgb(RgbColor(0xFF, 0xFF, 0x00)),
    Color::Rgb(RgbColor(0xFF, 0x00, 0xFF)),
    Color::Rgb(RgbColor(0x00, 0xFF, 0xFF)),
];

/// App command interface, juse a prue Python shell.
#[pyfunction]
fn main() {
    let msg = Python::with_gil(|py| shell::run(py, args::Args::parse(py)));
    std::process::exit(msg.report())
}

#[pymodule]
fn ptpyrs(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(main, m)?)?;
    Ok(())
}
