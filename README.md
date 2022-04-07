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
- Variable definition without assignment (`val x` or `val y: str`)
- Casting from float to int
- Comments
- Warnings
    - Unnecessary semicolons (if a newline is following)
    - Unused variables
