# Canonical cast-vote-mark schema

The long-format contract that every reader produces: one row per mark on
a ballot, with the contest, precinct, rank and candidate resolved to
labels.

## Usage

``` r
castvote_schema()
```

## Value

A \[data.table::data.table()\] with one row per schema column and the
fields \`column\`, \`type\`, and \`required\`.
