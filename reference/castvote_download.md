# Download an election's source files

Downloads every file for an election into the cache and verifies its
checksum.

## Usage

``` r
castvote_download(id, overwrite = FALSE)
```

## Arguments

- id:

  An election id from \[castvote_elections()\].

- overwrite:

  Whether to re-download files that already exist.

## Value

A named character vector of file paths, named by role.
