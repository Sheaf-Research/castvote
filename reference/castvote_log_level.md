# Logging level

castvote logs through the logger package under the \`"castvote"\`
namespace. Logging is quiet by default: only warnings and above are
emitted. Raise the level to \`"INFO"\` or \`"DEBUG"\` to follow
progress. The level is also read from the \`CASTVOTE_LOG_LEVEL\`
environment variable or the \`castvote.log_level\` option when the
package loads.

## Usage

``` r
castvote_log_level(level = NULL)
```

## Arguments

- level:

  A log level, one of \`"TRACE"\`, \`"DEBUG"\`, \`"INFO"\`, \`"WARN"\`,
  \`"ERROR"\`, or \`"FATAL"\`, matched case-insensitively. \`NULL\` (the
  default) returns the current level.

## Value

When reading, the current level as a string. When setting, the level
invisibly.
