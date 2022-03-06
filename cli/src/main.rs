use std::env::args;
use std::io;
use std::process::exit;

use cods::{bprintln, Color, Context, DGreen, DYellow, LBlue, LRed, UserFacing, ANSI_ESC};

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
    bprintln!(
        LBlue,
        "Started interactive repl (read interpret print loop)"
    );

    let input = io::stdin();
    let mut buf = String::new();
    loop {
        buf.clear();
        if input.read_line(&mut buf).is_err() {
            bprintln!(LRed, "Error reading line");
            continue;
        }

        if buf.lines().next().map(|s| s.trim()) == Some("exit") {
            break;
        }

        print_calc(&buf);
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
            let calcs = separate_lines(&input);
            let mut iter = calcs.iter().enumerate().peekable();
            while let Some((i, c)) = iter.next() {
                println!("# {}", i + 1);
                println!("------------------");
                println!("{c}");
                print_calc(c);
                println!();

                if iter.peek().is_some() {}
            }
        }
        Err(_) => {
            bprintln!(LRed, "Error reading file: {p}");
            exit(1);
        }
    }
}

fn separate_lines(string: &str) -> Vec<&str> {
    let mut calcs = Vec::new();
    let mut line_start = 0;
    let mut last_line_end = 0;
    let mut last_line_start = 0;
    let mut pos = 0;
    let mut pushed_str = false;

    let mut chars = string.chars();
    while let Some(c) = chars.next() {
        match c {
            '\r' => {
                let line = &string[(last_line_end + 1)..pos];

                if line == "---" {
                    let calc = &string[line_start..last_line_end];
                    calcs.push(calc);

                    line_start = pos + 1;
                    pushed_str = true;
                }

                last_line_end = pos;
            }
            '\n' => {
                if !pushed_str {
                    let line = &string[(last_line_start)..pos];

                    if line == "---" {
                        let calc = &string[line_start..last_line_end];
                        calcs.push(calc);

                        line_start = pos + 1;
                        pushed_str = true;
                    }

                    last_line_end = pos;
                }

                last_line_start = pos + 1;
            }
            _ => pushed_str = false,
        }

        pos = string.len() - chars.as_str().len();
    }

    if !pushed_str {
        let line = &string[(last_line_end + 1)..pos];

        let calc = if line == "---" {
            &string[line_start..last_line_end]
        } else {
            &string[line_start..pos]
        };
        calcs.push(calc);
    }

    calcs
}

fn calc_args(first: String, args: impl Iterator<Item = String>) {
    let input = args.fold(first, |a, b| a + " " + &b);

    print_calc(&input);
}

fn print_calc(input: &str) {
    let mut ctx = Context::default();
    match ctx.calc(input) {
        Ok(v) => {
            for w in ctx.warnings.iter().rev() {
                println!("{}\n", w.display(input));
            }
            for e in ctx.errors.iter().rev() {
                println!("{}\n", e.display(input));
            }
            println!("= {v}");
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
    A mathmatical expression that will be calculated

{yellow}FLAGS:{esc}
    {green}-i{esc}, {green}--interactive{esc}   Start an interactive repl (read evaluate print loop)
    {green}-h{esc}, {green}--help{esc}          Show this help message
    {green}-v{esc}, {green}--version{esc}       Print the version

{yellow}OPTIONS:{esc}
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
