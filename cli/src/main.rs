use std::env::args;
use std::io;
use std::process::exit;

use cods::{bprintln, Color, DGreen, DYellow, LBlue, LRed, UserFacing, ANSI_ESC};

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
    if let Some(p) = path {
        match std::fs::read_to_string(&p) {
            Ok(input) => return print_calc(&input),
            Err(_) => {
                bprintln!(LRed, "Error reading file: {}", p);
                exit(1);
            }
        }
    }
    bprintln!(LRed, "Path not specified");
    exit(1);
}

fn calc_args(first: String, args: impl Iterator<Item = String>) {
    let input = args.fold(first, |a, b| a + " " + &b);

    print_calc(&input);
}

fn print_calc(input: &str) {
    match cods::calc(input) {
        Ok((v, warnings)) => {
            for w in warnings.iter() {
                println!("{}", w.display(input));
            }
            println!("\n{}", v);
        }
        Err(errors) => {
            for e in errors.iter() {
                println!("{}", e.display(input));
            }
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
    println!("{}", env!("CARGO_PKG_VERSION"));
}
