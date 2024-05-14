# Implicid Type Conversions
We denote here when types will be cast implicitly and on which operators. When writing `Type1, Type2`, the ordering does not matter. We denote with `Type1->Type2` that `Type1` will be converted to `Type2`.

Note: When casting we use the default conversion methods defined in the respective data types.

## + Operator
### String, Bool
`bool -> string`

### Number, Bool
`bool -> number`

### Number, String
`number -> string`

### Array, String
`array -> string`

### Dictionary, String
`dictionary -> string`

## - * / % ^ Operator
### Number, Bool
`bool -> number`

