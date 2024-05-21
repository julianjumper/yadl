# Functions
A function may be declared on one of the following ways:

## Inline
It as the form: `(arguments) => stmt`. The result of the function will be the statement in case the statement is an expression. Otherwise it will return none.

### Example

```
() => print("hi")
(x) => x + 1
() => return 1 // Error 

eight = ((x, y) => x + y)(3, 5)

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

## Scope
The function body is inside a new scope.

### Example
```
x = (x) => x
print(x(3)) // prints 3 not the function.
```

## Optional Arguments

You can assign default values for an argument in a function definition. This argument is then optional and doesn't
require a value to be passed when calling the function. Optional arguments can only be followed by another optional 
argument or an arglist

### Example
```
x = (x=2) => x + 1
print(x()) // prints 3
print(x(5)) // prints 6

y = (u=2, w) => u + w //causes an error because an optional argument is followd by a nonoptional argument
```

## arglist

You can have a function expect multiple arguments by following an argument with `...`

There can only be one such argument in a function definition and it needs to be the last argument in the definition

The arguments are passed as an array

### Example
```
x = (y, args...) => {
    while (y < len(args)): 
        {
        print(args[y])
        y += 1
        }
    }

x(0, 1, 2, 3, 4)

z = (args..., y) => y // This causes an error

z = (args..., args2...) => y // This causes an error
```