# Strings

Strings are enclosed by `" "`/`' '` or `f" "`/`f' '`. If they are opened with `f" "`/`f' '` they are a formatted string which allows you to insert variables with `{ }` which works like the `+` operator (and potentionally other things but that's by far the most used f-string property in python).


### Example
```
"Hallo Welt"
'Hello World!'
f"Moin {name}!"
```


# String Operators
`+`: takes two strings and appends the second string to the first. If used with a non-string, that gets converted to a string.

`*`: takes a string and a int x, returning the string x times

`[]`: works like array accessing

## Builtins
`len`: returns the length of a string

`split`: takes a string and returns an array of substrings split on a delimeter


### Example
```
"Hallo " + "Welt!" // "Hallo Welt!"
"Moin" * 3 // "MoinMoinMoin"
"Hello"[1] // "H"
len("Hello") // 5
split("Hello World!", " ") // ["Hello", "World!"]
```