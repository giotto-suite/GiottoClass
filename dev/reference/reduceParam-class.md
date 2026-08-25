# Parameter Classes for Data Reduction Operations

Utility class that defines a dimensionality-reduction or decomposition
procedure and any params used in performing it. Packages defining
reduction methods (PCA, UMAP, tSNE, ...) will create their own child
classes. These parameter objects are intended to be passed alongside the
data to reduce to
[`reduceData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reduceData.md).
Reduction methods return a decomposition (typically a list of
matrices/vectors such as `list(u, d, v, ...)`), distinguishing them from
[`processData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/processData.md)
(same-shape transform),
[`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md)
(summary stats), and
[`filterData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/filterData.md)
(selection).

## Slots

- `param`:

  list. Named parameters to use with the intended reduction operation.
  These can be accessed and updated using the `$` operator.
