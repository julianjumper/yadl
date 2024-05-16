# Dictionaries:

Values in dictionaries an be accessed via `dic[key]`. Keys and values may be any datatype.

Note: There is no sigificant whitespace inside dictionaries analog to parentheses.

Key-values are seperated by a colon. Key-value-pairs are seperated by a comma.
Leading and trailing commas are optional.

## Example

```
x = {
    , a: 1,
    "b": "3",
    3: false,
}

x[3] = 1
x['3'] = 2 // x['3'] != x[3]

{
    1: 1,
    2: 2
}

{
    1: 1,
    2: 2,
}

{
    ,1: 1
    ,2: 2
}

{
    1: 1 // Error: comma expected
    2: 2
}

{
    1: 1         ,
    ,2:
     2
}

```


## Returning an empty dictionary with an inline function
Given this function `() => {}` it may not be clear if it's returning an empty dictionary of none. We declare that it returns none (an empty statement block). If one wishes to return an empty dictionary you would do `() => ({})`.