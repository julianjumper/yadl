# Functions
A function may be declared on one of the following ways:

## Inline
It as the form: `(arguments) => stmt`. The result of the function will be the statement in case the statement is an expression. Otherwise it will return none.

### Example

```
() => print("hi")
(x) => x + 1
() => return 1 // Error 
```

## Block
It has the form `(arguments) => { stmt* }`. It will return none if not explicitly returned inside the function body.

### Example

```
() => {
    x
} // Returns none

() => {
    return 1
} // Returns 1

() => {} // Returns none
```

## Assignments & Function Calls
Functions are considered objects inside the language. You may call functions the following way:

```
inc = (x) => x + 1
y = inc(1) // y == 2
```