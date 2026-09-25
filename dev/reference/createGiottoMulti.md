# Create a giottoMulti object

Container for multiple `giotto` objects analyzed in a shared expression
space. Each child keeps its own spatial information; shared analyses
(joint dim reduction, NN graphs, clustering) live on the parent.

## Usage

``` r
createGiottoMulti(objects, instructions = NULL, source = NULL)
```

## Arguments

- objects:

  named `list` of `giotto` objects

- instructions:

  a `giottoInstructions` object (optional)

- source:

  on-disk source / project manager (e.g. GiottoDisk::gDirSource) for
  cross-sample shared-domain artifacts. If `NULL` (default),
  auto-acquired from the first sourced child; if no child carries a
  source, the multi is in-memory. When supplied, must be the same
  backend class as any source the children carry.

## Value

`giottoMulti`

## Examples

``` r
if (FALSE) { # \dontrun{
g1 <- GiottoData::loadGiottoMini("visium")
g2 <- GiottoData::loadGiottoMini("viz")
mg <- createGiottoMulti(list(visium = g1, viz = g2))
} # }
```
