# Convert Specialized TIF Formats to Basic TIF

Simple converter from specialized formats to .tif format. Utilizes the
python tifffile package. Performs image conversions one page at a time.
Wrap this in a for loop or lapply for more than one image or page. Used
when image formats are unsupported by terra. This is implementation may
change in the future. Currently tested to work with `.ome.tif` and
`qptiff`

## Usage

``` r
to_simple_tif(
  input_file,
  output_dir = file.path(dirname(input_file), "tif_exports"),
  page = NULL,
  overwrite = FALSE
)

ometif_to_tif(
  input_file,
  output_dir = file.path(dirname(input_file), "tif_exports"),
  page = NULL,
  overwrite = FALSE
)
```

## Arguments

- input_file:

  character. Filepath to tif to convert

- output_dir:

  character. Output directory (default: "

  /tif_exports")

- page:

  integer or NULL. 1-based page index; NULL means first page, a "\_%04d"
  formatted suffix will be added to the output filename.

- overwrite:

  logical. Default = FALSE. Whether to overwrite if the filename already
  exists.

## Value

returns the written filepath invisibly

## Functions

- `ometif_to_tif()`: deprecated.

## See also

Other tif utility functions:
[`tif_metadata()`](https://giotto-suite.github.io/GiottoClass/reference/tif_metadata.md)
