# Names of objects

Get or set the names of the layers of a `giottoLargeImage`

## Usage

``` r
# S4 method for class 'giottoLargeImage'
names(x)

# S4 method for class 'giottoLargeImage'
names(x) <- value

# S4 method for class 'processParam'
names(x)

# S4 method for class 'processParam'
names(x) <- value

# S4 method for class 'giottoPoints'
names(x)
```

## Arguments

- x:

  object

- value:

  character. Name(s) to set in the object

## Value

character or the object being renamed if the replacement function is
used.

## Examples

``` r
img <- GiottoData::loadSubObjectMini("giottoLargeImage")
#> Warning: [rast] unknown extent
names(img)
#> [1] "mini_dataset_dapi_z0"
names(img) <- "dapi"
```
