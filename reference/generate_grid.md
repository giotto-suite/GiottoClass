# Spatial grids

Generate a spatial grid of spatial locations

## Usage

``` r
triGrid(extent, ccd, id_prefix = "ID_")

orthoGrid(extent, ccd, id_prefix = "ID_")
```

## Arguments

- extent:

  SpatExtent or anything else a SpatExtent can be extracted or created
  from

- ccd:

  center to center distance

- id_prefix:

  character. prefix to add to ID names generated

## Value

spatial grid

## Examples

``` r
e <- ext(0, 100, 0, 100)
x <- triGrid(extent = e, ccd = 10)
plot(x$sdimx, x$sdimy, asp = 1L)


x <- orthoGrid(extent = e, ccd = 10)
plot(x$sdimx, x$sdimy, asp = 1L)
```
