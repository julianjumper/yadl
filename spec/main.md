


# Operator Precedence



# Terminals

Note everything in brackets is regex.


`DIGIT ::= [0-9]`

`IDENT ::= [a-zA-Z] [a-zA-Z0-9_]*` // Variable/Function Identifier
# Types



`sting, bool, number, array, dictionary, none`

types can be converted with for example bool(3) == true

Check the type of an object with `typeof obj == 'bool'`.

# Variables
`IDENT ::= EXPR`

## Example
```
x = 3
x = x * 6 + func(69) | compilerBau
 
```
## Comment
Sets a value of an identifier. Reassigning identifiers is allowed.

# Functions

`FUNC ::= '(' ARG_LIST ')' '=>' FUNC_BODY`

`ARG_LIST ::= IDENT ARG_REP | eps`

`ARG_REP ::= ',' IDENT ARG_REP | eps`

`FUNC_BODY ::= STMT | '{' STMT* '}'`

## Example 
```
() => print('gg')
(x, y) => x + y


u = 0
u_succ = () => u = u + 1 // u_succ() == none ; u == 1
enpty = () => {
    // do nothing
}

upp = (u) => u + 1 // upp(4) == 5
```

## Comment
Functions may not return at all. If not they automagically return type none.

Functions may access variables that are out of scope.

## TODO(0)
Add ability to access variables with same name as local function in outer space.


## TODO(0)
Functions with single arguments may omit the parens.
I.e. `x => x + 1` instead of `(x) => x + 1`.


# Loops

`WHILE_LOOP ::= 'while' '(' EXPR ')' '{' STMT* '}'`

# Control flow

## Example

```
if (cond) {

} elif (cond) {

} else {

}
```


## Example
```
i = 0
while (i < 10) {
    i = i + 1
}
```

# Piping
`PIPE_OP ::= EXPR | EXPR`


## Example
```
data = ... // [1,6]
result = data | function1 arg arg arg | function2 arg (x) => x + 1
```

## Comment
`a | fn arg2 arg3 ...` is equivalent to `fn(a, arg2, arg3, ...)`.


# Array accessing

## Example

```
arr = [1,2,3]
first = arr[0]
last = arr[-1]
first = arr[start_inclusive:end_exclusive]
```
Note: For further details see python.

# Dictionary accessing

## Example

```
dict = {'a': 1, b: 2}

a_value = dict['a']
a_value = dict.a
```

Note: Dictionaries are ordered acording to insertion order.

# I/O
Default serialization format is always json.

## save as (keyword)
`save EXPR to 'path' (as EXPR)?` - saves the result of the expr to disk.

## load as (keyword)
`load EXPR (as EXPR)?` - loads the file.

## printnal (build in)
`printnal(EXPR, EXPR)` - prints the result of the first epxr to the console. May convert objects to a string. print not a line (nal). Takes the file format as optional second parameter.


# Build function

`keys(dict)` - Given a dictionary return all its keys in an array.
`copy(dict/arr)` - Makes a deep copy of an arr/dict and returns it.

`len(arr/dict/str)` - returns its length


# EXPR
```
EXPR ::= IDENT | FUNC | PIPE_OP | 
```

# Arithmetic
Standart Arithmeitk
`+`: Addition
`-`: Subtraction / negative numbers
`*`: Multiplication
`/`: Division
`^`: Power -> `2^2 = 4`

# Boolean Operations
Standart bool OPs
# String Operations
strings eiher with '' or "".

str + str = strstr
when u do str + notstr, its equal to str + string(notstr).


## TODO(-1) python fstring formatting

# Scoping and function scope

# Significant newlines