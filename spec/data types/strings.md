# Strings

Strings are enclosed by `" "`/`' '` or `f" "`/`f' '`. If they are opened with `f" "`/`f' '` they are a formatted string which allows you to insert variables with `{ }` which works like the `+` operator.

Note: strings are immutable.

### Example
```
"Hallo Welt"
'Hello World!'
f"Moin {name}!"
```


# String Operators
`+`: takes two strings and returns the concationation of both strings.

`*`: takes a string and a number (must be a non negative whole number) x, returning a new string that the concatination of the original string to '' x times.

`[]`: You can access the i-th element in the string.

### Example
```
"Hallo " + "Welt!" // "Hallo Welt!"
"Moin" * 3 // "MoinMoinMoin"
"Hello"[1] // "H"
len("Hello") // 5
```

## Multiline Strings
You can also define multipline strings in the following way:

```
'''
this is a multiline string!!
ok
'''
```
You can also use `"` instead of `'`. 

Note: Whe using indented multiline strings, the indentation is also considered part of the string. Same goes for newlines.

## Implicit type conversions
`string(any)`

### Bool
returns `'true'` if `true` otherwise return `'false'`.

### Number
retruns the number as a string.

#### Example
```
string(0xf_f) // '255'
```
### Arrays
Prints all elements of the array enclosed in brackets and seperated by commas. Calls the string cast recursivly for all elements.

#### Example
```
string([[[], none]]) // '[[[],none]]'
```

### Dictionaries
Prints all key value pairs seperated by commas. The pairs it self are seperated by colons. Calls the string conversion recursivly for all keys and values. This construct will be enclosed by curly braces.

```
d = {}
d[[]] = none
string(d) // '{[]:none}' 
```

### String
returns the string.

### none
returns `'none'`
