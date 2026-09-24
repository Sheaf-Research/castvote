# Read an official round-by-round report

Reads a San Francisco official results workbook into the structures
\[verify_tabulator()\] expects: per-round candidate totals, the
elimination order, the transfers, and the contest summaries.

## Usage

``` r
read_official_rounds(path)
```

## Arguments

- path:

  Path to an \`.xls\` or \`.xlsx\` round report.

## Value

A list with \`rounds\`, \`eliminated\`, \`summaries\`, \`transfers\`,
\`winner\`, and \`metadata\`.
