# Dictionaries:

Values in dictionaries an be accessed via `dic[key]` or via `dic.key` if the key would be a valid variable name. Keys and values may be any datatype.

Note: There is no sigificant whitespace inside dictionaries analog to parentheses.


## Example

```
x = {
    , a: 1,
    "b"; "3",
    3: false,
}

x.b = none
x[3] = 1
x['3'] = 2 // x['3'] != x[3]

```


## Returning an empty dictionary with an inline function
Given this function `() => {}` it may not be clear if it's returning an empty dictionary of none. We declare that it returns none (an empty statement block). If one wishes to return an empty dictionary you would do `() => ({})`.