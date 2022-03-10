use std::env::args;
use std::io::{self, Write};
use std::process::exit;

use cods::Context;

use display::*;
use style::*;

mod display;
mod style;

fn main() {
    let mut args = args().skip(1);
    if let Some(first) = args.next() {
        match first.as_str() {
            "-i" | "--interactive" => repl(),
            "-p" | "--path" => calc_file(args.next()),
            "-h" | "--help" => help(),
            "-v" | "--version" => version(),
            _ => calc_args(first, args),
        }
    } else {
        bprintln!(LRed, "Error: missing arguments\n");
        help();
        exit(1);
    }
}

fn repl() {
    bprintln!(LBlue, "Started interactive repl");

    let mut output = io::stdout();
    let input = io::stdin();
    let mut buf = String::new();
    let mut ctx = Context::default();
    loop {
        buf.clear();
        ctx.clear_errors();

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
            _ => print_calc(&mut ctx, &buf),
        }
    }
}

fn calc_file(path: Option<String>) {
    let p = match path {
        Some(p) => p,
        None => {
            bprintln!(LRed, "Path not specified");
            exit(1);
        }
    };

    match std::fs::read_to_string(&p) {
        Ok(input) => {
            let mut ctx = Context::default();
            print_calc(&mut ctx, &input);
            println!();
        }
        Err(_) => {
            bprintln!(LRed, "Error reading file: {p}");
            exit(1);
        }
    }
}

fn calc_args(first: String, args: impl Iterator<Item = String>) {
    let input = args.fold(first, |a, b| a + " " + &b);
    let mut ctx = Context::default();
    print_calc(&mut ctx, &input);
}

fn print_calc(ctx: &mut Context, input: &str) {
    match ctx.parse_and_eval(input) {
        Ok(v) => {
            for w in ctx.warnings.iter().rev() {
                println!("{}\n", w.display(input));
            }
            for e in ctx.errors.iter().rev() {
                println!("{}\n", e.display(input));
            }
            if let Some(v) = v {
                println!("{v}")
            }
        }
        Err(e) => {
            for w in ctx.warnings.iter().rev() {
                println!("{}\n", w.display(input));
            }
            for e in ctx.errors.iter().rev() {
                println!("{}\n", e.display(input));
            }
            println!("{}\n", e.display(input));
        }
    }
}

fn help() {
    println!(
        "{green}comeondosomething{esc} {vers}
{authors}
{desc}

{yellow}USAGE:{esc}
    cods [FLAGS] [OPTIONS] [EXPRESSION]

{yellow}EXPRESSION:{esc}
    A mathmatical expression that will be evaluated

{yellow}OPTIONS:{esc}
    {green}-i{esc}, {green}--interactive{esc}   Start an interactive repl (read evaluate print loop)
    {green}-h{esc}, {green}--help{esc}          Show this help message
    {green}-v{esc}, {green}--version{esc}       Print the version
    {green}-p{esc}, {green}--path <file>{esc}   A file that contains an expression
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
