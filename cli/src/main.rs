use std::env::args;
use std::process::exit;

use cods::{Color, Green, Red, UserFacing, Yellow};

fn main() {
    let mut args = args().skip(1);
    if let Some(first) = args.next() {
        match first.as_str() {
            "-h" | "--help" => help(),
            "-i" | "--interactive" => repl(),
            "-p" | "--path" => calc_file(args.next()),
            _ => calc_args(first, args),
        }
    } else {
        help();
        exit(1);
    }
}

fn help() {
    println!(
        "
{green}comeondosomething{esc} {vers}
{authors}
{desc}

{yellow}USAGE:{esc}
    cods [FLAGS] [OPTIONS] [EXPRESSION]

{yellow}EXPRESSION:{esc}
    A mathmatical expression that will be calculated

{yellow}FLAGS:{esc}
    {green}-i{esc}, {green}--interactive{esc}   Start a interactive repl (read evaluate print loop)

{yellow}OPTIONS:{esc}
    {green}-p{esc}, {green}--path <file>{esc}   A file that contains an expression
",
        vers = env!("CARGO_PKG_VERSION"),
        authors = env!("CARGO_PKG_AUTHORS"),
        desc = env!("CARGO_PKG_DESCRIPTION"),
        green = Green::ansi_start(),
        yellow = Yellow::ansi_start(),
        esc = Red::ansi_end(),
    );
}

fn repl() {
    todo!("A repl is not yet implemented")
}

fn calc_file(path: Option<String>) {
    if let Some(p) = path {
        match std::fs::read_to_string(&p) {
            Ok(input) => return print_calc(&input),
            Err(_) => {
                println!("\x1B[;31mError reading file: {}\x1B[0m", p);
                exit(1);
            }
        }
    }
    println!("\x1B[;31mPath not specified\x1B[0m");
    exit(1);
}

fn calc_args(first: String, args: impl Iterator<Item = String>) {
    let input = args.fold(first, |mut a, b| {
        a.push(' ');
        a.push_str(&b);
        a
    });

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
