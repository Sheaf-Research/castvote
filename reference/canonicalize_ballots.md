# Canonicalize mark data

Validates marks, coerces every column to the \[castvote_schema()\] type,
and orders rows by ballot, rank and candidate.

## Usage

``` r
canonicalize_ballots(marks)
```

## Arguments

- marks:

  A data frame of marks.

## Value

The canonical marks as a data table.
