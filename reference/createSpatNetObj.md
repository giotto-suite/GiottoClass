# Create S4 spatialNetworkObj

Create S4 spatialNetworkObj

## Usage

``` r
createSpatNetObj(
  network,
  name = "test",
  networkDT_before_filter = NULL,
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

  network data with connections, distances, and weightings

- name:

  name of spatialNetworkObj

- networkDT_before_filter:

  (optional) unfiltered data.table of network connections, distances,
  and weightings

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

createSpatNetObj(network = slot(x, "networkDT"), name = "Delaunay_network")
#> An object of class spatialNetworkObj : "Delaunay_network"
#> spat_unit : "cell"
#>    1275 connections (filtered)
```
