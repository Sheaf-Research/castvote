# Read a Dominion CVR export

A Dominion export is a zip of \`CvrExport\_\*.json\` files plus manifest
files that name every internal id. Each session and card is one ballot
sheet. Marks are flattened and joined to the manifests, then reduced to
the \[castvote_schema()\] long format for recorded votes. Contest-level
overvote and undervote counts are carried through as extra columns;
mapping them to per-rank flags is not yet defined.

## Usage

``` r
read_dominion(zip_path, contests = NULL, files = NULL, cores = 1L)
```

## Arguments

- zip_path:

  Path to the CVR zip, read in place.

- contests:

  Contest ids to keep. Defaults to the ranked contests (\`NumOfRanks \>
  0\` or \`VoteFor \> 1\`).

- files:

  Optional subset of zip entries, for testing.

- cores:

  Files read in parallel on unix.

## Value

Canonical marks as a data table.
