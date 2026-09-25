# Resolve ballots against a continuing candidate set

Selects each ballot's highest-ranked continuing candidate, applying the
overvote, skipped-rank and duplicate rules.

## Usage

``` r
resolve_ballots(ballots, continuing, rules)
```

## Arguments

- ballots:

  Cast-vote marks, in the \[castvote_schema()\] format.

- continuing:

  Candidate ids still in the running.

- rules:

  An \[irv_rules()\] object.

## Value

A data table with \`ballot_id\`, \`candidate_id\`, and \`status\`.
