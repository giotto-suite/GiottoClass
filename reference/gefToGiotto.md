# Convert gef to Giotto

Converts .gef file (output stereo-seq pipeline) into giotto subcellular
object

## Usage

``` r
gefToGiotto(
  gef_file,
  bin_size = "bin100",
  gene_column = NULL,
  verbose = FALSE,
  h5_file = NULL
)
```

## Arguments

- gef_file:

  path to .gef file

- bin_size:

  bin size to select from .gef file

- gene_column:

  (optional) character. Which column contains gene names within the
  geneExp information.

- verbose:

  be verbose

- h5_file:

  name to create and on-disk HDF5 file

## Value

giotto object

## Details

Function in beta. Converts .gef object to Giotto object.

There are six possible choices for bin_size: bin1, bin10, bin20, bin50,
bin100, bin200.

See SAW pipeline for additional information about the gef file.
