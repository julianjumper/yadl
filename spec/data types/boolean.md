# Bool
`bool_lit`

boolean is the truth value of an expression. It can either be `true` or`false`
It is a 1-bit value

```
true 
false
```

# Boolean Arithmetic

### Operators
`or, and, not`
`or` takes two expressions and returns `true` if either of the expressions evaluates as `true`, else `false`
`and` takes two expressions and returns `true` if both expressions evaluate as `true`, else `false`
`not` takes one expression and inverts the boolean value it is evaluated as

Examples:
```
true or false == true
false or false == false
true and false == false
true and true == true
not true == false
not false == true
```

### Evaluating Expressions
A Number is evaluated as `false` if it is equal to 0, else it is evaluated as `true`

A String is evaluated as `true` if it is nonempty, while an empty String is evaluated as `false`

An Array/Dictionary is evaluated as `true` if it is nonempty, an empty Array/Dictionary is evaluated as `false`

`none` is always evaluated as `false`

Examples:
```
0 == false
23 == true
'hallo' == true
'' == false
[] == false
{} == false
[1, 2, 3, 4] == true
{'a': 1, 'b': 2} == true
none == false 
```
