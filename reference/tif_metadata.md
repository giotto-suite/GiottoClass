# Read Metadata of a Specialized tif

Use the python package tifffile to get the the XML metadata of a .tif
file. The R package {xml2} is then used to work with it to retrieve
specific nodes in the xml data and extract data (JPEG2000, multi-file
pyramids).

## Usage

``` r
tif_metadata(
  path,
  node = NULL,
  page = NULL,
  type = c("attribute", "text", "double", "integer"),
  output = c("data.frame", "xml", "list", "structure", "kv")
)

ometif_metadata(
  path,
  node = NULL,
  page = NULL,
  type = c("attribute", "text", "double", "integer"),
  output = c("data.frame", "xml", "list", "structure", "kv")
)
```

## Arguments

- path:

  character. filepath to tif image

- node:

  character vector. Specific xml node to get. More terms can be added to
  get a node from a specific hierarchy.

- page:

  numeric or NULL. Specific page to get metadata from. Currently only
  used for `.qptiff`.

- type:

  character. Type of data to extract. Only affects `output = data.frame`
  (Matches to one of "attribute", "text", "double", "integer").
  `output = "structure"` can help with figuring out which is most
  appropriate.

- output:

  character. One of "data.frame" to return a data.frame of the
  attributes information of the xml node, "xml" for an {xml2}
  representation of the node, "list" for an R native list (note that
  many items in the list may have overlapping names that make indexing
  difficult), "structure" to invisibly return NULL, but print the
  structure of the XML document/node, or "kv" (extract key/value pairs
  from OME MapAnnotations).

## Value

list/data.frame/XML depending on `output`

## Functions

- `ometif_metadata()`: deprecated.

## See also

Other tif utility functions:
[`to_simple_tif()`](https://giotto-suite.github.io/GiottoClass/reference/to_simple_tif.md)

## Examples

``` r
if (FALSE) {
# check structure of metadata
tif_metadata("path/to/ometif", output = "structure")

# xenium morphology ometif - find channels/biomarkers
tif_metadata("path/to/ometif", node = "Channel")

# phenocycler qptiff - find channels/biomarkers
tif_metadata("path/to/qptiff",
    page = NULL,
    node = "Biomarker",
    type = "text"
)
}
```
