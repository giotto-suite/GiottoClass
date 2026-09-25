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
#> An object of class giottoBinPoints
#> feat_type : "rna"
#> dimensions : 326, 2
#> compact    : TRUE
#>    feat_ID count
#>     <char> <num>
#> 1:       a     2
#> 2:       c     2
#> 3:       d     1
#> 4:       e     2
#> 5:       f     2
#> 6:       g     1
#> 
nrow(gbp)
#> [1] 326
dim(gbp)
#> [1] 326   2
data.table::as.data.table(gbp)
#>      feat_ID count
#>       <char> <num>
#>   1:       a     2
#>   2:       c     2
#>   3:       d     1
#>   4:       e     2
#>   5:       f     2
#>  ---              
#> 322:       d     1
#> 323:       e     1
#> 324:       g     1
#> 325:       i     1
#> 326:       j     2
head(gbp)
#> An object of class giottoBinPoints
#> feat_type : "rna"
#> dimensions : 6, 2
#> compact    : TRUE
#>    feat_ID count
#>     <char> <num>
#> 1:       a     2
#> 2:       c     2
#> 3:       d     1
#> 4:       e     2
#> 5:       f     2
#> 6:       g     1
#> 
tail(gbp)
#> An object of class giottoBinPoints
#> feat_type : "rna"
#> dimensions : 6, 2
#> compact    : TRUE
#>    feat_ID count
#>     <char> <num>
#> 1:       a     1
#> 2:       d     1
#> 3:       e     1
#> 4:       g     1
#> 5:       i     1
#> 6:       j     2
#> 
objName(gbp)
#> [1] "rna"
featType(gbp)
#> [1] "rna"

# subsetting ---------------------------------------------------- #
gbp[50:100]
#> An object of class giottoBinPoints
#> feat_type : "rna"
#> dimensions : 51, 2
#> compact    : FALSE
#>    feat_ID count
#>     <char> <num>
#> 1:       b     2
#> 2:       d     2
#> 3:       e     1
#> 4:       h     1
#> 5:       j     2
#> 6:       a     1
#> 
gbp["a"] # get only points for feature "a"
#> An object of class giottoBinPoints
#> feat_type : "rna"
#> dimensions : 30, 2
#> compact    : FALSE
#>    feat_ID count
#>     <char> <num>
#> 1:       a     2
#> 2:       a     2
#> 3:       a     1
#> 4:       a     2
#> 5:       a     2
#> 6:       a     2
#> 
gbp[letters[1:4]] # get only points for features "a", "b", "c", "d"
#> An object of class giottoBinPoints
#> feat_type : "rna"
#> dimensions : 130, 2
#> compact    : FALSE
#>    feat_ID count
#>     <char> <num>
#> 1:       a     2
#> 2:       c     2
#> 3:       d     1
#> 4:       a     2
#> 5:       b     1
#> 6:       c     2
#> 

# plotting ------------------------------------------------------ #
plot(gbp, dens = TRUE) # will take a long time on large datasets

plot(gbp["a"]) # plot feature "a" only

plot(gbp[c("a", "d")]) # plot features "a" and "d" together


# spatial ------------------------------------------------------- #
ext(gbp) # spatial extent
#> SpatExtent : -2.1999743298905319, 2.2291260856596962, -1.9512348808118125, 2.2280345687170522 (xmin, xmax, ymin, ymax)

d <- Giotto::hexVertices(1)
#> Error in loadNamespace(x): there is no package called ‘Giotto’
d$poly_ID <- "a"
#> Error: object 'd' not found
hex <- createGiottoPolygon(d)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'createGiottoPolygon': object 'd' not found
plot(gbp, col = "blue")

plot(hex, add = TRUE, border = "red")
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': object 'hex' not found
plot(crop(gbp, hex), add = TRUE, col = "green") # cropping
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'plot': error in evaluating the argument 'y' in selecting a method for function 'crop': object 'hex' not found

hex2 <- tessellate(ext(gbp), shape_size = 1)
#> 14 polygons generated
res <- calculateOverlap(hex2, gbp) # overlapped feature calculation
m <- overlapToMatrix(res) # overlap info to expression matrix
force(m)
#> 10 x 14 sparse Matrix of class "dgCMatrix"
#>   [[ suppressing 14 column names ‘ID_1’, ‘ID_2’, ‘ID_3’ ... ]]
#>                              
#> a 1 . . . 2 3 1 4 6 3 . 2 2 .
#> b 1 1 1 1 2 2 . 2 7 3 . . 2 .
#> c 1 2 . 1 2 4 2 6 3 6 . 2 2 .
#> d . 2 1 2 2 3 1 6 5 7 . 2 2 .
#> e 1 . 1 2 3 4 . 4 3 4 . 2 1 .
#> f 1 2 1 2 . 4 . 5 5 5 . 1 1 .
#> g . 1 1 1 2 3 2 6 3 3 . 1 1 .
#> h 1 . 1 . 3 4 1 5 6 5 . 2 1 .
#> i 1 1 . 2 2 2 2 6 1 4 . 2 . .
#> j 1 2 1 2 2 2 1 7 3 5 . 1 1 .
```
