use std::env::args;
use std::io::{self, Write};
use std::process::{exit, ExitCode};

use cods::{Context, Scopes, Stack, Val};

use display::*;
use style::*;

mod display;
mod style;

#[derive(Default)]
struct State {
    ctx: Context,
    scopes: Scopes,
    stack: Stack,
}

fn main() -> ExitCode {
    let mut args = args().skip(1);
    if let Some(first) = args.next() {
        match first.as_str() {
            "-i" | "--interactive" => repl(),
            "-p" | "--path" => calc_file(args.next()),
            "-h" | "--help" => {
                help();
                ExitCode::SUCCESS
            }
            "-v" | "--version" => {
                version();
                ExitCode::SUCCESS
            }
            _ => calc_args(first, args),
        }
    } else {
        bprintln!(LRed, "Error: missing arguments\n");
        help();
        ExitCode::FAILURE
    }
}

fn repl() -> ExitCode {
    bprintln!(LBlue, "Started interactive repl");

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

        match buf.lines().next().map(|s| s.trim()) {
            Some("exit") => break,
            Some("clear") => {
                print!("\x1b[1;1H\x1B[2J");
                let _ = output.flush();
            }
            _ => {
                print_calc(&mut state, &buf);
            }
        }
    }

    ExitCode::SUCCESS
}

fn calc_file(path: Option<String>) -> ExitCode {
    let p = match path {
        Some(p) => p,
        None => {
            bprintln!(LRed, "Path not specified");
            exit(1);
        }
    };

    match std::fs::read_to_string(&p) {
        Ok(input) => {
            let mut state = State::default();
            print_calc(&mut state, &input)
        }
        Err(_) => {
            bprintln!(LRed, "Error reading file: {p}");
            ExitCode::FAILURE
        }
    }
}

fn calc_args(first: String, args: impl Iterator<Item = String>) -> ExitCode {
    let input = args.fold(first, |a, b| a + " " + &b);
    let mut state = State::default();
    print_calc(&mut state, &input)
}

fn print_calc(state: &mut State, input: &str) -> ExitCode {
    match calc(state, input) {
        Ok(v) => {
            for w in state.ctx.warnings.iter().rev() {
                println!("{}\n", w.display(input));
            }
            if v != Val::Unit {
                println!("{v}")
            }
            ExitCode::SUCCESS
        }
        Err(e) => {
            for w in state.ctx.warnings.iter().rev() {
                println!("{}\n", w.display(input));
            }
            for e in state.ctx.errors.iter().rev() {
                println!("{}\n", e.display(input));
            }
            println!("{}\n", e.display(input));
            ExitCode::FAILURE
        }
    }
}

fn calc(state: &mut State, input: &str) -> cods::Result<Val> {
    let tokens = state.ctx.lex(input.as_ref())?;
    let items = state.ctx.group(tokens)?;
    let csts = state.ctx.parse(items)?;
    let asts = state.ctx.check_with(&mut state.scopes, csts)?;
    if !state.ctx.errors.is_empty() {
        return Err(state.ctx.errors.remove(0));
    }
    cods::eval_with(&mut state.stack, &asts)
}

fn help() {
    println!(
        "\
{green}comeondosomething{esc} {vers}
{authors}
{desc}

{yellow}USAGE:{esc}
    cods [OPTIONS] [EXPRESSION]

{yellow}EXPRESSION:{esc}
    An expression that will be evaluated

{yellow}OPTIONS:{esc}
    {green}-i{esc}, {green}--interactive{esc}   Start an interactive repl (read evaluate print loop)
    {green}-h{esc}, {green}--help{esc}          Show this help message
    {green}-v{esc}, {green}--version{esc}       Print the version
    {green}-p{esc}, {green}--path <file>{esc}   Evaluate a file
",
        vers = env!("CARGO_PKG_VERSION"),
        authors = env!("CARGO_PKG_AUTHORS"),
        desc = env!("CARGO_PKG_DESCRIPTION"),
        green = DGreen::normal(),
        yellow = DYellow::normal(),
        esc = ANSI_ESC,
    );
}

fn version() {
    println!(env!("CARGO_PKG_VERSION"));
}
