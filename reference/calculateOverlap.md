# Calculate features overlapped by polygons

Calculate subcellular points/feature info or image values overlapped by
polygon annotations. This provides a summary of the spatial data
overlapped by the polygon which can be further processed to become an
expression matrix.

## Usage

``` r
# S4 method for class 'giotto,missing'
calculateOverlap(
  x,
  name_overlap = NULL,
  spat_info = NULL,
  feat_info = NULL,
  image_names = NULL,
  poly_subset_ids = NULL,
  return_gobject = TRUE,
  verbose = TRUE,
  spatial_info = deprecated(),
  ...
)

# S4 method for class 'giottoPolygon,giottoPoints'
calculateOverlap(
  x,
  y,
  name_overlap = NULL,
  poly_subset_ids = NULL,
  feat_subset_column = NULL,
  feat_subset_values = NULL,
  feat_count_column = NULL,
  return_gpolygon = TRUE,
  verbose = TRUE,
  feat_subset_ids = deprecated(),
  count_info_column = deprecated(),
  ...
)

# S4 method for class 'giottoPolygon,giottoLargeImage'
calculateOverlap(
  x,
  y,
  name_overlap = NULL,
  poly_subset_ids = NULL,
  return_gpolygon = TRUE,
  verbose = TRUE,
  ...
)

# S4 method for class 'giottoPolygon,giottoAffineImage'
calculateOverlap(
  x,
  y,
  name_overlap = NULL,
  poly_subset_ids = NULL,
  return_gpolygon = TRUE,
  verbose = TRUE,
  ...
)

# S4 method for class 'giottoPolygon,SpatRaster'
calculateOverlap(
  x,
  y,
  name_overlap = NULL,
  poly_subset_ids = NULL,
  return_gpolygon = TRUE,
  verbose = TRUE,
  ...
)

# S4 method for class 'SpatVector,SpatRaster'
calculateOverlap(
  x,
  y,
  poly_subset_ids = NULL,
  verbose = TRUE,
  fun = "sum",
  ...
)

# S4 method for class 'SpatVector,SpatVector'
calculateOverlap(
  x,
  y,
  poly_subset_ids = NULL,
  feat_subset_column = NULL,
  feat_subset_values = NULL,
  feat_count_column = NULL,
  method = getOption("giotto.overlap_point_method", "vector"),
  verbose = TRUE,
  feat_subset_ids = deprecated(),
  count_info_column = deprecated()
)
```

## Arguments

- x:

  Object with spatial annotations: `giottoPolygon`, or `SpatVector`
  polygons. Can also be a `giotto` object

- name_overlap:

  name for the overlap results (default to feat_info parameter)

- feat_info:

  character. Name of vector feature information to overlap

- image_names:

  character vector. Name(s) of the image feature information to overlap

- poly_subset_ids:

  character vector. (optional) Specific poly_IDs to use

- return_gobject:

  return giotto object (default: TRUE)

- verbose:

  be verbose

- spatial_info:

  character. Name polygon information

- ...:

  additional params to pass to methods.

- y:

  Object with features to overlap: `giottoPoints`, `giottoLargeImage`,
  `SpatVector` points or `SpatRaster`

- feat_subset_column:

  character. (optional) feature info attribute to subset feature points
  on when performing overlap calculation.

- feat_subset_values:

  (optional) values matched against in `feat_subset_column` in order to
  subset feature points when performing overlap calculation.

- feat_count_column:

  character. (optional) column with count information. Useful in cases
  when more than one detection is reported per point. If a column called
  "count" is present in the feature points data, it will be
  automatically selected.

- return_gpolygon:

  default = TRUE. Whether to return the entire giottoPolygon provided to
  `x`, but with the overlaps information appended or as a bare terra
  `SpatVector`

- feat_subset_ids:

  deprecated. Use `feat_subset_values` instead.

- count_info_column:

  deprecated. Use `feat_count_column` instead.

- method:

  character. One of `"vector"` or `"raster"`, (default = `"vector"`).
  Method for polygon-point feature overlap calculation. Can also set as
  an option: `"giotto.overlap_point_method"`

  - `"vector"` uses direct spatial extraction (more accurate to input
    geometry, will double count features in overlapping polygon regions
    for all overlapping polygons).

  - `"raster"` uses rasterization (faster, assigns each feature to only
    one polygon even in overlapping regions as a byproduct of the
    rasterization).

## Value

Usually an object of the same class as `x`, with the overlaps
information appended. `return_*` logical params usually allow return of
a lower-level representation of the results instead. Only the
`SpatVector,SpatRaster` method is different in that it returns a
`data.table`

## Details

`feat_subset_column`, `feat_subset_values`, and `feat_count_column` are
specific to overlaps on feature points info, and should not be provided
when overlapping image data. These three params can also be passed to
the `giotto` method through the `...` param when working with overlaps
on feature points info.

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12
gpoly <- getPolygonInfo(g,
    polygon_name = "aggregate",
    return_giottoPolygon = TRUE
)
gpoints <- getFeatureInfo(g, return_giottoPoints = TRUE)
gimg <- getGiottoImage(g, image_type = "largeImage")

slot(gpoly, "overlaps") <- NULL
overlaps(gpoly) # Should now be NULL
#> NULL

# detections from 2 z-layers are provided
table(gpoints$global_z)
#> 
#>     0     1 
#> 35889 43872 

# calculate all transcripts overlapped
out_all <- calculateOverlap(gpoly, gpoints)
overlaps_all <- overlaps(out_all)
overlaps_all$rna
#> <overlapPointDT>
#> spat_unit : "aggregate"
#> feat_type : "rna"
#> provenance: aggregate 
#> * polygons : 462
#> * features : 79761
#> * relations: 35268

# calculate z1 only
out_z1 <- calculateOverlap(gpoly, gpoints,
    feat_subset_column = "global_z",
    feat_subset_values = c(1)
)
overlaps_z1 <- overlaps(out_z1)
overlaps_z1$rna
#> <overlapPointDT>
#> spat_unit : "aggregate"
#> feat_type : "rna"
#> provenance: aggregate 
#> * polygons : 462
#> * features : 43872
#> * relations: 18581

# overlap image to get sum intensities per cell
out_img <- calculateOverlap(gpoly, gimg, progress = FALSE)
#> Start image extract
#> End image extract
overlaps_img <- overlaps(out_img)
overlaps_img$intensity
#> $dapi_z0
#> <overlapIntensityDT>
#> spat_unit : "aggregate"
#> feat_type : "dapi_z0"
#> provenance: aggregate 
#>                                      poly_ID mini_dataset_dapi_z0
#>                                       <char>                <num>
#>   1: 100210519278873141813371229408401071444             514207.8
#>   2: 101161259912191124732236989250178928032             576698.0
#>   3: 101488859781016188084173008420811094152             593017.6
#>   4: 101523780333017320796881555775415156847             292254.4
#>   5: 102184699197574201819246996094734116255             386535.3
#>  ---                                                             
#> 458:   9677424102111816817518421117250891895             254194.3
#> 459:  97068595823890802071024838798130036179             381228.5
#> 460:  97411078642927912684859796714759494710             509129.4
#> 461:   9816437869021910185567097363182418837             582503.4
#> 462:  98561957902191275233320065611022298397             701973.7
#> * use `[]` to drop to `data.table`

# giotto method
# calculate z0 overlaps and return as gobject
out_g <- calculateOverlap(g,
    feat_subset_column = "global_z",
    feat_subset_values = 0
)
overlaps(getPolygonInfo(out_g, return_giottoPolygon = TRUE))
#> $rna
#> <overlapPointDT>
#> spat_unit : "z0"
#> feat_type : "rna"
#> provenance: z0 
#> * polygons : 498
#> * features : 35889
#> * relations: 15125
#> 

# note that z0 and z1 nrows match that from the table of global z values.
# With points overlaps, all points are returned, but non-overlapped points
# only have an `NA` value for the `poly_ID` column. Overlapped points will
# have the `poly_ID` of their overlapping polygon.
```
