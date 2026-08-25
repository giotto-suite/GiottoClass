# Compatible spatial network

Function to evaluate if a spatial network is compatible with a provided
expression matrix

## Usage

``` r
compatible_spatial_network(spatial_network, expression_matrix)
```

## Arguments

- spatial_network:

  spatial network to evaluate

- expression_matrix:

  expression to compare against

## Value

TRUE or character

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
spat_net <- getSpatialNetwork(g, output = "networkDT")
expr_m <- getExpression(g)

compatible_spatial_network(spat_net, expr_m)
```
