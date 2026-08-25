# Create S4 spatialNetworkObj

Create S4 spatialNetworkObj

## Usage

``` r
createSpatNetObj(
  network,
  name = "test",
  unfiltered = NULL,
  method = NULL,
  spat_unit = "cell",
  provenance = NULL,
  parameters = NULL,
  outputObj = NULL,
  cellShapeObj = NULL,
  crossSectionObjects = NULL,
  misc = NULL
)
```

## Arguments

- network:

  network as `igraph` (canonical) or `data.frame` with `from`/`to`
  columns. Data.frame input is coerced to `data.table`.

- name:

  name of spatialNetworkObj

- unfiltered:

  (optional) unfiltered network — same accepted forms as `network`.
  Stored for inspection.

- method:

  method used to generate spatial network

- spat_unit:

  spatial unit tag

- provenance:

  (optional) origin of aggregated information (if applicable)

- parameters:

  (optional) additional method-specific parameters used during spatial
  network generation

- outputObj:

  (optional) network geometry object

- cellShapeObj:

  (optional) network cell shape information

- crossSectionObjects:

  (optional) crossSectionObjects

- misc:

  misc

## Value

spatialNetworkObj

## Examples

``` r
x <- GiottoData::loadSubObjectMini("spatialNetworkObj")

createSpatNetObj(network = slot(x, "network"), name = "Delaunay_network")
```
