use std::env::args;
use std::io::{self, Write as _};
use std::process::ExitCode;

use cods::{Asts, Checker, Context, Stack, Val, Warning};
use cods_derive::EnumFromStr;

use display::*;
pub use style::*;

mod display;
mod json;
mod style;

#[derive(Default)]
struct State {
    ctx: Context,
    checker: Checker,
    stack: Stack,
}

impl State {
    fn clear(&mut self) {
        self.ctx.clear();
        self.checker.clear();
        self.stack.clear();
    }
}

#[derive(Clone, Copy, Default, PartialEq, Eq, EnumFromStr)]
#[cods(rename_all = "snake_case")]
enum OutputFormat {
    #[default]
    Pretty,
    Json,
}

#[derive(Default)]
struct Args {
    format: OutputFormat,
    skip_unused_warnings: bool,
}

enum Action {
    Run(String),
    Check(String),
    Interactive,
    Help,
    Version,
}

macro_rules! error {
    ($pat:expr $(,$args:expr),*) => {{
        bprintln!(LRed, $pat $(,$args)*);
        println!();
        help();
        return ExitCode::FAILURE;
    }}
}

fn main() -> ExitCode {
    let mut action = None;
    let mut user_args = Args::default();

    let mut args = args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "i" | "interactive" => action = Some(Action::Interactive),
            "r" | "run" => {
                let Some(path) = args.next() else {
                    error!("Path not specified");
                };
                action = Some(Action::Run(path));
            }
            "c" | "check" => {
                let Some(path) = args.next() else {
                    bprintln!(LRed, "Path not specified");
                    return ExitCode::FAILURE;
                };
                action = Some(Action::Check(path));
            }
            "-h" | "--help" => action = Some(Action::Help),
            "-v" | "--version" => action = Some(Action::Version),
            "-f" | "--format" => match args.next() {
                Some(f) => match f.parse::<OutputFormat>() {
                    Ok(f) => user_args.format = f,
                    Err(_) => {
                        error!("Invalid --format: `{f}`, possible values are [pretty, json]");
                    }
                },
                None => {
                    error!("Missing --format, possible values are [pretty, json]");
                }
            },
            "--" => {
                let items = args.collect::<Vec<_>>();
                return eval_args(&user_args, &items);
            }
            a => {
                error!("Invalid argument: `{a}`");
            }
        }
    }

    match action {
        Some(Action::Run(path)) => eval_path(&user_args, &path),
        Some(Action::Check(path)) => check_path(&user_args, &path),
        Some(Action::Interactive) => repl(&mut user_args),
        Some(Action::Help) => {
            help();
            ExitCode::SUCCESS
        }
        Some(Action::Version) => {
            version();
            ExitCode::SUCCESS
        }
        None => {
            bprintln!(LRed, "Missing arguments\n");
            help();
            ExitCode::FAILURE
        }
    }
}

fn repl(args: &mut Args) -> ExitCode {
    bprintln!(LBlue, "Started interactive repl");

    args.skip_unused_warnings = true;

    let mut output = io::stdout();
    let input = io::stdin();
    let mut buf = String::new();
    let mut state = State::default();
    loop {
        buf.clear();
        state.ctx.clear_errors();

        bprint!(LBlue, " >> ");
        let _ = output.flush();
        if input.read_line(&mut buf).is_err() {
            bprintln!(LRed, "Error reading line");
            continue;
        }

        match buf.lines().next().map(str::trim) {
            Some("exit") => break,
            Some("clear") => {
                print!("\x1b[1;1H\x1B[2J");
                let _ = output.flush();
            }
            Some("reset") => {
                state.clear();
                print!("\x1b[1;1H\x1B[2J");
                let _ = output.flush();
            }
            _ => {
                print_eval(&mut state, &buf, args);
            }
        }
    }

    ExitCode::SUCCESS
}

fn eval_path(args: &Args, path: &str) -> ExitCode {
    match std::fs::read_to_string(path) {
        Ok(input) => {
            let mut state = State::default();
            match print_eval(&mut state, &input, args) {
                Some(_) => ExitCode::SUCCESS,
                None => ExitCode::FAILURE,
            }
        }
        Err(_) => {
            bprintln!(LRed, "Error reading file: {path}");
            ExitCode::FAILURE
        }
    }
}

fn check_path(args: &Args, path: &str) -> ExitCode {
    match std::fs::read_to_string(path) {
        Ok(input) => {
            let mut state = State::default();
            match print_check(&mut state, &input, args) {
                Some(_) => ExitCode::SUCCESS,
                None => ExitCode::FAILURE,
            }
        }
        Err(_) => {
            bprintln!(LRed, "Error reading file: {path}");
            ExitCode::FAILURE
        }
    }
}

fn eval_args(args: &Args, items: &[String]) -> ExitCode {
    let input = items.join(" ");
    let mut state = State::default();
    match print_eval(&mut state, &input, args) {
        Some(_) => ExitCode::SUCCESS,
        None => ExitCode::FAILURE,
    }
}

fn print_eval(state: &mut State, input: &str, args: &Args) -> Option<Val> {
    if let Some(asts) = print_check(state, input, args) {
        match cods::eval_with(&mut state.stack, &state.checker.funs, &asts) {
            Ok(v) => {
                if v != Val::Unit {
                    println!("{v}");
                }
                return Some(v);
            }
            Err(e) => {
                println!("{}", e.display(input));
            }
        }
    }

    None
}

fn print_check(state: &mut State, input: &str, args: &Args) -> Option<Asts> {
    let asts = match check(state, input) {
        Ok(asts) => Some(asts),
        Err(e) => {
            state.ctx.errors.push(e);
            None
        }
    };

    match args.format {
        OutputFormat::Pretty => {
            if state.ctx.errors.is_empty() {
                for w in state.ctx.warnings.iter().rev() {
                    let skip = args.skip_unused_warnings
                        && matches!(
                            w,
                            Warning::UnusedVar(_, _)
                                | Warning::UnreadVar(_, _)
                                | Warning::UnusedFun(_, _)
                        );

                    if !skip {
                        println!("{}\n", w.display(input));
                    }
                }
            } else {
                for w in state.ctx.errors.iter().rev() {
                    println!("{}\n", w.display(input));
                }
            }
        }
        OutputFormat::Json => {
            let mut buf = String::new();
            if state.ctx.errors.is_empty() {
                let _ = json::write_diagnostics(&mut buf, &[], &state.ctx.warnings);
            } else {
                let _ = json::write_diagnostics(&mut buf, &state.ctx.errors, &[]);
            }
            println!("{buf}");
        }
    }

    asts
}

fn check(state: &mut State, input: &str) -> cods::Result<Asts> {
    let tokens = state.ctx.lex(input.as_ref())?;
    let items = state.ctx.group(tokens)?;
    let csts = state.ctx.parse(items)?;
    let asts = state.ctx.check_with(&mut state.checker, csts)?;
    if !state.ctx.errors.is_empty() {
        return Err(state.ctx.errors.remove(0));
    }
    Ok(asts)
}

fn help() {
    println!(
        "\
{green}comeondosomething{esc} {vers}
{authors}
{desc}

{yellow}USAGE:{esc}
    cods [COMMAND][OPTIONS] [-- EXPRESSION]

{yellow}EXPRESSION:{esc}
    An expression that will be evaluated

{yellow}COMMANDS:{esc}
    {green}r{esc}, {green}run   <file>{esc}         Run a file
    {green}c{esc}, {green}check <file>{esc}         Check a file
    {green}i{esc}, {green}interactive{esc}          Start an interactive repl

{yellow}OPTIONS:{esc}
    {green}-h{esc}, {green}--help{esc}              Show this help message
    {green}-v{esc}, {green}--version{esc}           Print the version
    {green}-f{esc}, {green}--format <format>{esc}   The output format [default: \"pretty\"] [possible values: \"pretty\", \"json\"]
",
        vers = env!("CARGO_PKG_VERSION"),
        authors = env!("CARGO_PKG_AUTHORS"),
        desc = env!("CARGO_PKG_DESCRIPTION"),
        green = DGreen::NORMAL,
        yellow = DYellow::NORMAL,
        esc = ANSI_ESC,
    );
}

fn version() {
    println!(env!("CARGO_PKG_VERSION"));
}
