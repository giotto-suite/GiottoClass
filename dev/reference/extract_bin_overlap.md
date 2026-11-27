# Assign bins to polygons

Which polygon each bin belongs to, in the form
[`terra::extract()`](https://rspatial.github.io/terra/reference/extract.html)
reports points in polygons: one row per bin hit (`id.y`, the bin's row
in `bins`) with the polygon's attributes, `NA` for bins outside every
polygon.

- `"inside"`: the polygons that contain the bin's point
  (`terra::extract(polys, bins)`).

- `"area"`: a bin is the square of side `bin_width` centred on its point
  plus `bin_offset`, and goes to one polygon: the one containing the
  square's centre or, when none or several do, the one covering the
  largest part of the square (ties: the lowest `poly_id_col`). A polygon
  that only meets the square's edge does not count. This matches cell
  masks that give each bin to a cell whose outline crosses it
  (Stereo-seq cellbin).

GiottoDisk's engines apply the same rule to store-backed bins.

## Usage

``` r
extract_bin_overlap(
  polys,
  bins,
  bin_overlap = c("inside", "area"),
  bin_width = NULL,
  bin_offset = c(0, 0),
  poly_id_col = "poly_ID"
)
```

## Arguments

- polys:

  `SpatVector` polygons

- bins:

  `SpatVector` points, one per bin

- bin_overlap:

  `"inside"` or `"area"`

- bin_width:

  bin side in the units of `bins` (needed for `"area"`)

- bin_offset:

  offset `c(dx, dy)` from a bin's point to its centre

- poly_id_col:

  polygon ID attribute, used to break ties

## Value

a `data.frame`: `id.y` and the attributes of `polys`

## Examples

``` r
sq <- terra::vect("POLYGON ((0 0, 3 0, 3 3, 0 3, 0 0))")
sq$poly_ID <- "a"
bins <- terra::vect(cbind(c(1.5, 3.4, 5), 1.5), type = "points")
extract_bin_overlap(sq, bins) # only the first bin's point is inside
extract_bin_overlap(sq, bins, "area", bin_width = 1) # and the second bin
```
