use std::env::args;

use comeondosomething::calc;

fn main() {
    let input = args()
        .skip(1)
        .reduce(|mut a, b| {
            a.push(' ');
            a.push_str(&b);
            a
        })
        .unwrap_or_else(String::new);

    match calc(&input) {
        Ok(n) => println!("{}", n),
        Err(e) => println!("{}\n{}", input, e.show(&input)),
    }
}
