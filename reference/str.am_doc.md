# Display the structure of an Automerge document

S3 method for [`utils::str()`](https://rdrr.io/r/utils/str.html) that
displays the structure of an Automerge document in a human-readable
format.

## Usage

``` r
# S3 method for class 'am_doc'
str(object, max.level = 2, ...)
```

## Arguments

- object:

  An automerge document object.

- max.level:

  Maximum depth to recurse into nested structures. Default 2.

- ...:

  Additional arguments (ignored).

## Value

Invisibly returns `NULL`.

## Examples

``` r
if (FALSE) { # \dontrun{
doc <- amsync_fetch("wss://sync.automerge.org/", "4F63WJPDzbHkkfKa66h1Qrr1sC5U")
str(doc)
str(doc, max.level = 4)
} # }
```
