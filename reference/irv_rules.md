# Instant-runoff tabulation rules

Instant-runoff tabulation rules

## Usage

``` r
irv_rules(
  year,
  rank_cap,
  overvote,
  skipped_rank,
  duplicate_candidate,
  batch_elimination = FALSE,
  tie,
  initial_majority = "continuing_ballots",
  subsequent_majority = "continuing_ballots"
)
```

## Arguments

- year:

  Election year the rules describe.

- rank_cap:

  Maximum number of ranks a voter may mark.

- overvote, skipped_rank, duplicate_candidate, tie:

  How the tabulator treats an overvote, a skipped rank, a repeated
  candidate and an elimination tie. See \[irv_rule_options()\] for the
  accepted values.

- batch_elimination:

  Whether to eliminate every candidate whose combined total is below the
  next candidate's.

- initial_majority, subsequent_majority:

  Which ballot pool the majority threshold is measured against.

## Value

An object of class \`irv_rules\`.
