use std::env::args;
use std::process::exit;

use comeondosomething::calc;

fn main() {
    let input = read_file().or_else(read_args).unwrap_or_else(String::new);
    print_calc(&input);
}

fn read_file() -> Option<String> {
    let mut args = args().skip(1);
    let name = args.next()?;
    if &name == "--path" || &name == "-p" {
        let path = args.next()?;

        match std::fs::read_to_string(&path) {
            Ok(input) => return Some(input),
            Err(_) => {
                println!("\x1B[;31mError reading file: {}\x1B[0m", path);
                exit(1);
            }
        }
    }

    None
}

fn read_args() -> Option<String> {
    args().skip(1).reduce(|mut a, b| {
        a.push(' ');
        a.push_str(&b);
        a
    })
}

fn print_calc(input: &str) {
    match calc(input) {
        Ok(n) => println!("{}", n),
        Err(e) => println!("\n{}", e.display(input)),
    }
}
