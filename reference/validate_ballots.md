# Validate normalized mark data

Checks the \[castvote_schema()\] contract: required columns present,
ballot identifiers usable, ranks positive whole numbers, and the
overvote, skipped and write-in flags logical. A missing write-in flag
defaults to \`FALSE\`. The input is not modified; the validated copy is
returned.

## Usage

``` r
validate_ballots(marks)
```

## Arguments

- marks:

  A data frame of marks.

## Value

The validated marks as a \`cvr_marks\` data table.
