# Read a WinEDS ballot image and master lookup

WinEDS is the fixed-width format San Francisco used before 2019. The
ballot image holds one row per mark; the master lookup names the
contests, candidates, precincts and tally types. Both are read together
and resolved into the \[castvote_schema()\] long format.

## Usage

``` r
read_wineds(ballot, lookup, b_header = FALSE, l_header = FALSE)
```

## Arguments

- ballot:

  A path to the ballot image, a character vector of lines, or a
  single-column data frame.

- lookup:

  The master lookup, in the same forms as \`ballot\`.

- b_header, l_header:

  Whether the ballot image and lookup carry a header line.

## Value

Canonical marks as a data table.
