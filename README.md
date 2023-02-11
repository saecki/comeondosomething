# comeondosomething
[![CI](https://github.com/Saecki/comeondosomething/actions/workflows/main.yml/badge.svg)](https://github.com/Saecki/comeondosomething/actions/workflows/main.yml)
![LOC](https://tokei.rs/b1/github/saecki/comeondosomething?category=code)

A statically typed scripting language

## CLI
```
comeondosomething 0.2.0
Saecki <tobiasschmitz2001@gmail.com>
A statically typed scripting language

USAGE:
    cods [COMMAND][OPTIONS] [-- EXPRESSION]

EXPRESSION:
    An expression that will be evaluated

COMMANDS:
    r, run   <file>         Run a file
    c, check <file>         Check a file
    i, interactive          Start an interactive repl

OPTIONS:
    -h, --help              Show this help message
    -v, --version           Print the version
    -f, --format <format>   The output format [default: "pretty"] [possible values: "pretty", "json"]
```

## TODO
- structs `struct A { b: int, c: float }`
- Generics for functions `fun a<T>(i: T) -> T { i }`
- Warnings
    - Unnecessary semicolons (if a newline is following)
