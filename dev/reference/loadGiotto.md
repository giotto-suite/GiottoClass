# loadGiotto

Saves a Giotto object to a specific folder structure

## Usage

``` r
loadGiotto(
  path_to_folder,
  load_params = list(),
  reconnect_giottoImage = TRUE,
  python_path = NULL,
  init_gobject = TRUE,
  verbose = TRUE,
  ...
)
```

## Arguments

- path_to_folder:

  path to folder where Giotto object was stored with
  [`saveGiotto`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)

- load_params:

  additional parameters for loading or reading giotto object

- reconnect_giottoImage:

  (default = TRUE) whether to attempt reconnection of magick based image
  objects

- python_path:

  (optional) manually set your python path

- init_gobject:

  logical. Whether to initialize the `giotto` object after loading.
  (default = TRUE)

- verbose:

  be verbose

## Value

Giotto object

## Details

Works together with
[`saveGiotto`](https://giotto-suite.github.io/GiottoClass/dev/reference/saveGiotto.md)
to save and re-load Giotto objects. Additional load_params need to be
provided as a list and will go to
[`readRDS`](https://rdrr.io/r/base/readRDS.html) or `qread` You can set
the python path, alternatively it will look for an existing Giotto
python environment.

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
td <- tempdir()
saveGiotto(gobject = g, dir = td)

loadGiotto(path_to_folder = paste0(td, "/saveGiottoDir"))
```
