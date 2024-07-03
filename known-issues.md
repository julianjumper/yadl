# Known Issues
We will note features/bug fixes there that part of the core functionality of the language, but won't be implemented any time soon.


## Builtin functions returning functions
Converting InterpreterData functions to AST Functions is not possible. To fix this we would need to rewrite large parts of the interpreter. (Which would result in better data separation between AST and interpreter and improve performance significantly, but it's nonetheless a significant task.)

## Optional Arguments and Arglists

