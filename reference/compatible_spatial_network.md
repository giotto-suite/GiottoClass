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
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
spat_net <- getSpatialNetwork(g, output = "networkDT")
expr_m <- getExpression(g)
#> Error in UseMethod("getExpression"): no applicable method for 'getExpression' applied to an object of class "giotto"

compatible_spatial_network(spat_net, expr_m)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'colnames': object 'expr_m' not found
```
