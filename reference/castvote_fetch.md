# Fetch and read a registered election

Resolves an election's files from the local data directory or cache
(downloading them when needed), reads them, and returns canonical marks.

## Usage

``` r
castvote_fetch(id, contest = NULL, download = TRUE, ...)
```

## Arguments

- id:

  An election id from \[castvote_elections()\].

- contest:

  Optional contest label to keep.

- download:

  Whether to download missing files.

- ...:

  Passed to the reader.

## Value

Canonical \`cvr_marks\`.
