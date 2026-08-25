# Parameter Classes for Network Construction

Utility class that defines a network-construction procedure (kNN, sNN,
Delaunay, ...) and any params used in performing it. Packages defining
network methods will create their own child classes. These parameter
objects are intended to be passed alongside data to
[`createNetwork()`](https://giotto-suite.github.io/GiottoClass/dev/reference/createNetwork.md).
Network constructors return a graph (edges and optionally weights /
distances), distinguishing them from analysis-stage operations such as
[`processData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/processData.md),
[`filterData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/filterData.md),
[`reduceData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/reduceData.md),
and
[`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md).

## Slots

- `param`:

  list. Named parameters to use with the intended network operation.
  Accessed and updated via the `$` operator.
