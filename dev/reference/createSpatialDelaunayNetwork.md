# Create a spatial Delaunay network

Create a spatial Delaunay network based on cell centroid physical
distances.

## Usage

``` r
createSpatialDelaunayNetwork(
  gobject,
  name = NULL,
  default_name = "Delaunay_network",
  spat_unit = NULL,
  feat_type = NULL,
  spat_loc_name = NULL,
  method = c("deldir", "delaunayn_geometry", "RTriangle"),
  dimensions = "all",
  maximum_distance = "auto",
  minimum_k = 0,
  options = "Pp",
  Y = TRUE,
  j = TRUE,
  S = 0,
  verbose = TRUE,
  return_gobject = TRUE,
  output = c("spatialNetworkObj", "data.table"),
  space = NULL,
  ...
)
```

## Arguments

- gobject:

  giotto object

- name:

  name for spatial network (default = 'delaunay_network')

- default_name:

  name to fall back on when `name` is `NULL`, before the coordinate
  frame is prefixed. Exists so that one composition site can serve every
  entry point while each keeps its own spelling; callers do not normally
  set it.

- spat_unit:

  spatial unit

- feat_type:

  feature type

- spat_loc_name:

  name of spatial locations

- method:

  package to use to create a Delaunay network

- dimensions:

  which spatial dimensions to use. Use "sdimx" (spatial dimension x),
  "sdimy", "sdimz" respectively to refer to X (or the 1st), Y (or the
  2nd) and Z(or the 3rd) dimension, see details. (default = all)

- maximum_distance:

  distance cuttof for Delaunay neighbors to consider. If "auto", "upper
  whisker" value of the distance vector between neighbors is used; see
  the [`graphics::boxplot()`](https://rdrr.io/r/graphics/boxplot.html)
  documentation for more details.(default = "auto")

- minimum_k:

  minimum number of neighbours if maximum_distance != NULL

- options:

  (geometry) String containing extra control options for the underlying
  Qhull command; see the [Qhull
  documentation](http://www.qhull.org/html/qdelaun.htm) for the
  available options. (default = 'Pp', do not report precision problems)

- Y:

  (RTriangle) If TRUE prohibits the insertion of Steiner points on the
  mesh boundary.

- j:

  (RTriangle) If TRUE jettisons vertices that are not part of the final
  triangulation from the output.

- S:

  (RTriangle) Specifies the maximum number of added Steiner points.

- verbose:

  be verbose

- return_gobject:

  logical. return giotto object (default = TRUE)

- output:

  character. Object type to return spatial network as when
  `return_gobject = FALSE`. (default: 'spatialNetworkObj')

- space:

  (`giottoMulti` only) `character(1)`. Name of a coordinate frame
  recorded on the object, or `NULL` (default) for each sample's own
  native frame. The frame decides the job: every sample it covers gets a
  network built in that frame's coordinates, written into its own
  `@spatial_network` slot — so this mutates the wrapped children.

  This is **not** a sample selector. An artifact generator takes none,
  because once the rows are in a slot nothing downstream can tell which
  the selector admitted (see `adr/0006`). To build over a subset of
  samples, record a space over them, or subset with `mg[...]` first.

- ...:

  Other additional parameters

## Value

giotto object with updated spatial network slot

## Details

Creates a spatial Delaunay network as explained in `delaunayn`
(default), [`deldir`](https://rdrr.io/pkg/deldir/man/deldir.html), or
`triangulate`.

## Choosing a Delaunay backend

All three backends compute the same exact triangulation – on 50,000
uniform points each returns the identical 149,978 edges – so the choice
is purely one of implementation speed, and `deldir` is by a wide margin
the slowest:

|         |          |                      |
|---------|----------|----------------------|
| points  | `deldir` | `delaunayn_geometry` |
| 20,000  | 3.7 s    | 0.14 s               |
| 50,000  | 21.1 s   | 0.20 s               |
| 200,000 | ~363 s   | 0.94 s               |

`deldir` remains the default for backward compatibility, but
**`delaunay_method = "delaunayn_geometry"` is strongly preferred above a
few thousand points**. It requires the geometry package, and it is also
the only backend that handles 3D.

Two caveats. Qhull can struggle with exactly cocircular input – a
grid-aligned platform such as Visium – in which case pass
`options = "Qbb Qc Qz"`; a 10,000-point exact grid worked with the
default `"Pp"` in testing, but the failure mode is degenerate input
rather than size. And `deldir` peaks around 2.9 GB of memory at 200,000
points against `geometry`'s 0.24 GB, so on a large section it is the
memory, not just the wait, that bites.

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

createSpatialDelaunayNetwork(g)
```
