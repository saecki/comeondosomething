# comeondosomething
Experimental parser and interpreter

## CLI
```
comeondosomething 0.2.0
Saecki <tobiasschmitz2001@gmail.com>
A cli calculator

USAGE:
    cods [OPTIONS] [EXPRESSION]

EXPRESSION:
    An expression that will be evaluated

OPTIONS:
    -i, --interactive   Start an interactive repl (read evaluate print loop)
    -h, --help          Show this help message
    -v, --version       Print the version
    -p, --path <file>   Evaluate a file
```

## TODO
- Early returns
- Static Type checking
- Required parameter types and return type for functions (`fun a(b: str, c: bool) -> int { ... }`)
- Optional type hints for variables (`var x: int = 5`)
- Variable definition without assignment (`val x` or `val y: str`)
- Warnings
    - Unnecessary semicolons (if a newline is following)
    - Unused variables
