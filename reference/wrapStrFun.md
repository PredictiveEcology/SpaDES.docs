# Wrap/break strings by size

Unlike `strwrap` and `strsplit`, `wrapStrFun` "breaks" a string based on
length creating chunks of a specified size that are collapsed into a
single string but separated using a special character

## Usage

``` r
wrapStrFun(x, size, breakChar = "\n")
```

## Arguments

- x:

  the string, or a vector or list of strings to break

- size:

  desired size of string chunks. Strings whose original size is less
  than or equal to `size` will not be cut.

- breakChar:

  break symbol/character used to separate chunks

## Value

the "broken" and collapsed strings, in the same class as `x`
