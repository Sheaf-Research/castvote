# Verify a tabulation against an official report

Reconstructs the round states from an official round-by-round report and
compares them with a \[tabulate_irv()\] result: the winner, every
round's candidate totals, the elimination order, and the exhausted
count.

## Usage

``` r
verify_tabulator(tabulation, official)
```

## Arguments

- tabulation:

  A \[tabulate_irv()\] result.

- official:

  A list with \`winner\`, \`rounds\` (\`candidate_id\`, \`votes\`,
  \`round\`), \`eliminated\` (\`candidate_id\`, \`votes\`), and
  \`summaries\` (\`round\`, \`total_ballots\`).

## Value

A list with \`ok\`, the per-check flags, the mismatch tables, and the
official exhausted count.
