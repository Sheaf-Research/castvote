# Read a cast vote record

Dispatches to the reader for the detected or declared format.

## Usage

``` r
read_cvr(path, format = c("auto", "wineds", "dominion"), lookup = NULL, ...)
```

## Arguments

- path:

  A path to the cast vote record.

- format:

  One of \`"auto"\`, \`"wineds"\`, or \`"dominion"\`.

- lookup:

  The master lookup, required for WinEDS input.

- ...:

  Passed to the reader.

## Value

Canonical marks as a data table.
