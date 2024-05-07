
# Number
`number_lit`

A number is a 64bit signed floting point value.
It may be prefixed to notate its base. Available bases are Binary, Octal and Hexadecimal. Given a fractional number, the leading zero may be obmitted. You may not use a dot w
ithout following digits. You may add an arbitary amount of leading zeros. You may add one underscores inbetween the individual digits the make the number more readable.

## Example
```
6.5
-6
0o10 // Ocatal -> 8
0b10 // Binary -> 2
0x10 // Hexadecimal -> 16
.3 // -> 0.3
.0 // -> 0
0. // -> Error
00000 // -> 0
0_0.0_0 // -> 0
0_.0_0 // -> Error
1_000 // -> 1000
```

# Number Operations
The following operations are defined on numbers: 

`+` Addition

`-` Subraction

`*` Multiplicaton

`/` Division

`^` Power

`mod` Modulo (todo)