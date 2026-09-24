# Tabulate an instant-runoff election

Runs the full elimination sequence over canonical marks and returns the
winner, every round's tally, the elimination order, and the exhausted
count.

## Usage

``` r
tabulate_irv(
  marks,
  rules = NULL,
  seed = NULL,
  candidate_ids = NULL,
  ballot_weights = NULL,
  marks_are_canonical = FALSE
)
```

## Arguments

- marks:

  Cast-vote marks, in the \[castvote_schema()\] format.

- rules:

  An \[irv_rules()\] object, or \`NULL\` for the default behavior.

- seed:

  Optional integer seed for tie-breaking lots.

- candidate_ids:

  Optional candidate universe. Defaults to the candidates observed in
  the marks.

- ballot_weights:

  Optional positive weights named by ballot.

- marks_are_canonical:

  Whether \`marks\` is already canonical, skipping the validation pass.

## Value

A list with \`winner\`, \`rounds\`, \`eliminated\`, \`exhausted\`,
\`rules\`, and \`tie_breaks\`.
