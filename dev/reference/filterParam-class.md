# Parameter Classes for Data Filter Operations

Utility class that defines a data filter procedure and any params used
in performing it. Packages defining filter methods will create their own
child classes. These parameter objects are intended to be passed
alongside the data to filter to
[`filterData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/filterData.md).
Filter methods return a selection (typically
`list(feats_keep, cells_keep)` of character IDs) rather than transformed
data, distinguishing them from
[`processData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/processData.md).

## Slots

- `param`:

  list. Named parameters to use with the intended filter operation.
  These can be accessed and updated using the `$` operator.
