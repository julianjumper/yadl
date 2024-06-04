# Errors

Whenever an error occurs, the programm terminates with exit code 1

## Types of Errors

`CastingError` -> Happens when trying to execute a cast operation with an invaild argument, i.E.:

```
3 + {}
CastingError (line 1, column 5): cannot convert type dictionary to type number
```

This causes implicit type conversion and results in an error. This happens at Runtime

`VariableNotDefinedError` -> Happens when trying to execute an operation that contains an undefined Variable, i.E.:

```
x = 2 + y
VariableNotDefinedError (line 1, column 9): no variable with identifier y found
```

Here, `y` is not defined, so an error occurs. This happens at Runtime currently but should happen at before Execution in the future

`IndexOutOfRangeError` -> Happens when trying to access an element outside the range of an array
```
x = [1, 2, 3]
print(x[4])
IndexOutOfRangeError (line 2, column 9): Index 4 is out of range for array of length 3
```
Happens at Runtime

`KeyNotFoundError` -> Happens when trying to access a key in a dict that doesn't exist. 
```
x = {'a': 3, 'b': 4}
print(x['c'])
KeyNotFoundError(line2, column 9): No key named 'c' in dictionary 
```
Happens at Runtime


`ÌnvalidFunctionCallError`-> Happens when a function call doesn't follow the specifications in 

`data types/functions.md`. Happens at Runtime

`SyntaxError` -> Happens wheń the program isn't syntactically correct. Happens before Execution

# TODO

StackTrace
