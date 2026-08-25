# Read list of polygons information

Function extract list of polygons when given raw input as either mask or
tabular data. Calls the respective createGiottoPolygons functions.  
If a `giottoPolygon` object is passed then no edits will be made other
than updating the `name` slot if the list is named.

## Usage

``` r
readPolygonData(
  data_list,
  default_name = "cell",
  input = "guess",
  polygon_mask_list_params = NULL,
  polygon_dfr_list_params = NULL,
  calc_centroids = FALSE,
  verbose = TRUE
)
```

## Arguments

- data_list:

  read polygon results from list

- default_name:

  default name to assign if `polygonlist` is not a list. If
  `polygonlist` is an unnamed list then `default_name` will be used as
  part of the template for generating indexed default names.

- input:

  what type of input is being used. When set to 'guess', uses 'mask' if
  `polygonlist` is of type character and 'table' when `polygonlist` is
  dataframe-like

- polygon_mask_list_params:

  parameters for when using 'mask' workflow

- polygon_dfr_list_params:

  parameters for when using 'table' workflow

- calc_centroids:

  whether centroids should be calculated during polygon creation

- verbose:

  be verbose

## Value

giottoPolygon

## Examples

``` r
x <- GiottoData::loadSubObjectMini("giottoPolygon")

readPolygonData(x)
```
