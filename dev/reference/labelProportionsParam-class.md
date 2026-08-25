# Label proportions analysis parameter

Parameter class for
[`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md)
dispatching to neighborhood-level label compositions. The result is a
[spatEnrObj](https://giotto-suite.github.io/GiottoClass/dev/reference/spatEnrObj-class.md)
of proportions (groups x labels), suitable as input to
[`clusterData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/clusterData.md)
for niche clustering.

Three grouping methods, mirroring
[`calculateLabelProportions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateLabelProportions.md):

- `"table"` — explicit cell -\> group mapping (a `data.frame` or a
  metadata column name).

- `"spatialnetwork"` — per-cell neighborhoods derived from a spatial
  network.

- `"polygon"` — cells grouped under tessellation / region polygons.

`group_method` is read only by the
[`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md)
giotto-class router to select the primary data class to dispatch on; the
primary methods themselves dispatch on the data and never read it.

## See also

[`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md),
[`labelProportionsParam()`](https://giotto-suite.github.io/GiottoClass/dev/reference/labelProportionsParam.md),
[`calculateLabelProportions()`](https://giotto-suite.github.io/GiottoClass/dev/reference/calculateLabelProportions.md)
