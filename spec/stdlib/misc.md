Note: contains some random util functions

# Serialize
`serialize(obj: object, format="json|xml|toml") -> string`:

Tries to convert the given object (may be any type) to a string.
Note: this is a gernalization of the toString method for dictionaries and lists currently impelmented in the stdlib. `serialize(obj, format="json")` should replace the toString method parts that handle lists and dictionaries when fully implemented.
Recursive references must be handled. This will result in an error:

```
a = {}
b = {'a': a}
a['b'] = b
serialize(a)
```

The resulting string should be valid json,xml,toml,... if the input is a dictionary or list if supported by the language. Passing anything but dictionaries or lists should return an error for now (whe can change this later on).


