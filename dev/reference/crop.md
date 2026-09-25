# Crop to a spatial subset

Spatially subset an object x using object y. `crop()` will only return a
modified object if the `y` extent is smaller than the original in `x`.
For object type specifics, see below.

### `giottoLargeImage` and `giottoAffineImage`

When `write = FALSE` and a `filename` is not specified,
[`terra::window()`](https://rspatial.github.io/terra/reference/window.html)
will instead be used to create a lazy subset of the image. An
independent version of the cropped subset will only be created when
either of the above are provided, at which point it is handled through
[`terra::crop()`](https://rspatial.github.io/terra/reference/crop.html).
`...` params are only used when materializing the subset.

### `giottoPoints` and `giottoPolygon`

An alternative faster crop operation is implemented through `data.table`
manipulation of the geometry information and are used by default. This
mode also only allows rectangular subsetting. Additionally,
`giottoPolygons` will be cropped using their centroids so that the
entire polygon is either present or not instead of the default `crop`
behavior that would keep the portion of the polygon that does fall
within the selected crop region. Set `DT = FALSE` in order to use the
default style of terra::crop behavior that also allows usage of
additional params through ...

## Usage

``` r
# S4 method for class 'giottoBinPoints,ANY'
crop(x, y, ext = FALSE, compact = "auto", ...)

# S4 method for class 'giottoLargeImage,ANY'
crop(x, y, write = FALSE, filename = tempfile(fileext = ".tif"), ...)

# S4 method for class 'giottoAffineImage,ANY'
crop(x, y, write = FALSE, filename = tempfile(fileext = ".tif"), ...)

# S4 method for class 'spatLocsObj,ANY'
crop(x, y, ...)

# S4 method for class 'giottoPoints,ANY'
crop(x, y, DT = TRUE, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, ...)

# S4 method for class 'giottoPolygon,ANY'
crop(x, y, DT = TRUE, xmin = NULL, xmax = NULL, ymin = NULL, ymax = NULL, ...)

# S4 method for class 'giottoView,ANY'
crop(
  x,
  y,
  relation = "intersects",
  geom = c("centroid", "poly"),
  ...,
  view = NULL,
  space = NULL
)

# S4 method for class 'gAny,ANY'
crop(
  x,
  y,
  relation = "intersects",
  geom = c("centroid", "poly"),
  ...,
  view = NULL,
  space = NULL
)
```

## Arguments

- x:

  object

- y:

  any object that has a SpatExtent or returns a SpatExtent

- ext:

  `logical`. When `TRUE`, the extent of y will be used instead of y

- compact:

  `character` or `logical` (default = "auto"). Whether to compact
  object. See
  [giottoBinPoints](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoBinPoints-class.md).
  `"auto"` will perform a compaction when number of spatial points
  referenced in `@counts` is 1/10 of that existing in `@spatial`

- ...:

  additional params to pass to terra::crop

- write:

  `logical` (default = FALSE). Whether to write and materialize the
  cropped subset to disk. This is overridden when a `filename` is
  specifically provided.

- filename:

  `character` (default is a .tif tempfile). This file is not actually
  written unless the user specifically provides a path or
  `write = TRUE`.

- DT:

  logical. Use alternative DT subsetting for crop operation

- xmin, xmax, ymin, ymax:

  only used if DT = TRUE. Set extent bounds independently

- relation:

  `character(1)`. Spatial predicate. A crop narrows the **cell set**, so
  each cell is reduced to a geometry (see `geom`) and tested against the
  region. One of `"intersects"` (default), `"disjoint"`, `"within"`,
  `"covered_by"`, `"touches"`, `"contains"`, `"covers"`, `"overlaps"`,
  `"crosses"`. The last four are always `FALSE` against a centroid, so
  requesting one promotes `geom` to `"poly"` with a warning.

- geom:

  `character(1)`. What represents a cell when the predicate is
  evaluated: `"centroid"` (default) uses the cell's `spatial_locs` row —
  cheap, and the conventional choice, but a cell whose polygon straddles
  the region boundary with its centroid outside is dropped. `"poly"`
  uses the cell's actual polygon — exact, and requires a polygon source
  on the object. The choice is recorded on the step, so a saved recipe
  states which question it asks.

- view:

  `NULL` or `character(1)`. When supplied, records the crop as a step on
  the named view, creating it if it does not exist yet, instead of
  executing eagerly. Eager `crop()` on a `giotto` is not yet
  implemented.

- space:

  `NULL` or `character(1)`. Name of a slotted space on `x`. Sets the
  view's `space` reference — the coordinate frame in which the crop
  region is interpreted at resolution time. First call sets it;
  subsequent calls that try to rebind to a different name error. `NULL`
  leaves the view in whatever frame it was already bound to (or the
  gobject's native frame if unbound).

## Value

object of same class as `x`, spatially subsetted

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
img1 <- getGiottoImage(g, name = "image")
plot(img1)

# modify extent
e <- ext(img1)
e <- e - 1000

img2 <- crop(img1, e)
plot(img2)
```
