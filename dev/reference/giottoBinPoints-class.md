# Binned point class

S4 class allowing point detection-like access patterns for binned
spatial values. Implemented more efficiently by only representing the
spatial points once and mapping the sparse values information against
the points.

## Usage

``` r
createGiottoBinPoints(expr_values, spatial_locs, feat_type = "rna")
```

## Arguments

- expr_values:

  `exprObj` Bin counts/values

- spatial_locs:

  `spatLocsObj` Spatial locations of bins

- feat_type:

  `character` (default = "rna"). Feature type of the data

## Functions

- `createGiottoBinPoints()`: constructor function

## Slots

- `spatial`:

  ANY (currently `SpatVector` only). Row-indexed spatial points

- `counts`:

  `data.table` with integer cols `i` and `j` mapping to `@fid` and
  `@bid` respectively. `x` is a numeric col standing for count or value
  of a feature for this bin point. Row indexing of this object is based
  on this slot.

- `bid`:

  `character`. Bin IDs to map against

- `pmap`:

  `integer`. For each spatial point, gives the index into `@bid`. Length
  equals `length(spatial)`. Forms a bridge between spatial and counts:
  both `pmap` and `counts$j` are indices into `@bid`. **Invariant**:
  every bin ID in `counts$j` must appear in `pmap` (i.e., counts is
  always a subset of spatial). Allows subsetting `counts` without
  modifying the more expensive `spatial` representation until
  compaction.

- `fid`:

  `character`. Feature IDs to map against

- `compact`:

  `logical`. State of compaction. When `TRUE`, `@bid`, `@pmap`, and
  `@spatial` contain only bins that appear in `@counts` (bidirectional
  relationship). When `FALSE`, they may contain bins not present in
  `@counts` (unidirectional: every bin in counts has spatial, but not
  every spatial has counts).

## Examples

``` r
ids <- sprintf("bin_%d", 1:50)
sl <- createSpatLocsObj(rnorm(100))
sl$cell_ID <- ids
m <- matrix(floor(runif(500) * 3),
    ncol = 50,
    dimnames = list(letters[1:10], ids)
)
ex <- createExprObj(m)
gbp <- createGiottoBinPoints(ex, sl)

# basics -------------------------------------------------------- #
force(gbp)
nrow(gbp)
dim(gbp)
data.table::as.data.table(gbp)
head(gbp)
tail(gbp)
objName(gbp)
featType(gbp)

# subsetting ---------------------------------------------------- #
gbp[50:100]
gbp["a"] # get only points for feature "a"
gbp[letters[1:4]] # get only points for features "a", "b", "c", "d"

# plotting ------------------------------------------------------ #
plot(gbp, dens = TRUE) # will take a long time on large datasets
plot(gbp["a"]) # plot feature "a" only
plot(gbp[c("a", "d")]) # plot features "a" and "d" together

# spatial ------------------------------------------------------- #
ext(gbp) # spatial extent

d <- Giotto::hexVertices(1)
d$poly_ID <- "a"
hex <- createGiottoPolygon(d)
plot(gbp, col = "blue")
plot(hex, add = TRUE, border = "red")
plot(crop(gbp, hex), add = TRUE, col = "green") # cropping

hex2 <- tessellate(ext(gbp), shape_size = 1)
res <- calculateOverlap(hex2, gbp) # overlapped feature calculation
m <- overlapToMatrix(res) # overlap info to expression matrix
force(m)
```
