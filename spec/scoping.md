# Scoping
Variables can have either local or global scope. Variables with global scope are accessible in any Block in the programm and have to be defined in the main Block of the Programm.
Local scope variables are defined inside of a Block and are only accessible inside that block
## Example
```
glob = 3 // global variable

if(true){
    loc = 4 // local variable
    print(glob) // This prints 3
}

print(loc) // The variable is not in scope anymore, so this causes an OutOfScope Error
```
