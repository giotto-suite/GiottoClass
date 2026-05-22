# saveGiotto

Saves a Giotto object to a specific folder structure

## Usage

``` r
saveGiotto(
  gobject,
  foldername = "saveGiottoDir",
  dir = getwd(),
  method = c("RDS", "qs"),
  method_params = list(),
  overwrite = FALSE,
  export_image = TRUE,
  image_filetype = "PNG",
  include_feat_coord = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- gobject:

  Giotto object

- foldername:

  Folder name

- dir:

  Directory where to create the folder

- method:

  method to save main object

- method_params:

  additional method parameters for RDS or qs

- overwrite:

  Overwrite existing folders

- export_image:

  logical. Write out an image of the format specified by
  `image_filetype` when saving a `giottoLargeImage`. Future image loads
  and reconnects will point to this new file.

- image_filetype:

  the image filetype to use, see
  [`writeRaster`](https://rspatial.github.io/terra/reference/writeRaster.html).
  Default is "PNG". For TIFF outputs, try "COG"

- include_feat_coord:

  logical. Whether to keep the feature coordinates when saving. Dropping
  them can improve performance for large datasets.

- verbose:

  be verbose

- ...:

  additional parameters for
  [`writeRaster`](https://rspatial.github.io/terra/reference/writeRaster.html)

## Value

Creates a directory with Giotto object information

## Details

Works together with
[`loadGiotto`](https://giotto-suite.github.io/GiottoClass/reference/loadGiotto.md)
to save and re-load Giotto objects. Additional method_params need to be
provided as a list and will go to
[`saveRDS`](https://rdrr.io/r/base/readRDS.html) or `qsave`

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10

saveGiotto(gobject = g, dir = tempdir(), overwrite = TRUE)
#> Folder already exist and overwrite = TRUE,
#>  overwrite folder
#> 1. Start writing feature information
#> 2. Start writing spatial information
#> For spatial information: cell
#> 3. Start writing image information
#> For image information: alignment
#> For image information: image
#> [1] TRUE
```
