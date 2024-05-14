# Arrays
Arrays are Collections of values. These values can have the same value and can be the of different types

Defining an Array is done using square brackes (Example: `arr = [1, 'a']`)

Accessing an Element in an Array is done using square Brackets (Example: `arr[0]`)

Indexing of arrays starts at `0`


## Examples

```
arr = [1, 2, 3]
arr[0] == 0

arr_nested = [1, 2, [1, 2]] //Nested Array
arr_nested[2] == [1, 2]

arr_mixed = [1, 'Hello', {'first': 1, 'second': 2}]
arr_mixed[1] == 'Hello'
arr_mixed[2]['second'] == 2 
```


## Build-ins
`append(array, new1, new2, ...)` - Adds values to the existing array and returns the existing array.
`len(array)` - Returns the length of the array. 
