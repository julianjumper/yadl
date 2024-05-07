# Type Convserions

The following operations are defined on operations across different types:

`string + any`. This will convert any type to its string representation and append it to the string.

`any + string`. Analogous but it will prepend to the string.


`string * number`. If number is a none-negative integer, the result will be the original string repeated `number` times. For example `'hi' * 2 == 'hihi'`.

`array * number`. Analogous to the previous conversion it will copy all elements inside the array `number` of times and returns a new array.

`bool num_op number` it will convert the boolean to a number.




# Conversion Methods