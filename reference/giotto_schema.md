# Giotto schema

Data within the `giotto` object is organized in a schema largely
revolving around the **spatial unit** (which spatial length scale or
polygonal annotation that is used as the unit of study) and the
**feature type** (data modality). Information is then further organized
based on the **name** or key of the object. In cases where a single
spatial unit is comprised of information from multiple others,
**provenance** is tracked to keep a record of which spatial units were
the sources of that data. The functions to get and set these aspects of
the schema on the Giotto object and subobjects are:

- **spatial unit:** `spatUnit()`, `spatUnit<-()`

- **feature type:** `featType()`, `featType<-()`

- **name:** `objName()`, `objName<-()`

- **provenance:** `prov()`, `prov<-()`

## Usage

``` r
# S4 method for class 'ANY'
spatUnit(x)

# S4 method for class 'giotto'
spatUnit(x)

# S4 method for class 'list'
spatUnit(x)

# S4 method for class 'spatData'
spatUnit(x)

# S4 method for class 'giottoPolygon'
spatUnit(x)

# S4 method for class 'ANY'
spatUnit(x) <- value

# S4 method for class 'spatData'
spatUnit(x) <- value

# S4 method for class 'giottoPolygon'
spatUnit(x) <- value

# S4 method for class 'list'
spatUnit(x) <- value

# S4 method for class 'giotto'
spatUnit(x, old) <- value

# S4 method for class 'ANY'
featType(x)

# S4 method for class 'giotto'
featType(x)

# S4 method for class 'list'
featType(x)

# S4 method for class 'featData'
featType(x)

# S4 method for class 'ANY'
featType(x) <- value

# S4 method for class 'featData'
featType(x) <- value

# S4 method for class 'list'
featType(x) <- value

# S4 method for class 'giotto'
featType(x, old) <- value

# S4 method for class 'ANY'
objName(x)

# S4 method for class 'list'
objName(x)

# S4 method for class 'nameData'
objName(x)

# S4 method for class 'giottoPoints'
objName(x)

# S4 method for class 'giottoLargeImage'
objName(x)

# S4 method for class 'giottoImage'
objName(x)

# S4 method for class 'list'
objName(x) <- value

# S4 method for class 'nameData'
objName(x) <- value

# S4 method for class 'giottoImage'
objName(x) <- value

# S4 method for class 'giottoLargeImage'
objName(x) <- value

# S4 method for class 'giottoPoints'
objName(x) <- value

# S4 method for class 'provData'
prov(x)

# S4 method for class 'provData'
prov(x) <- value
```

## Arguments

- x:

  `giotto` or {Giotto} S4 subobject

- value:

  value to set for this schema component

- old:

  character. Old value to replace

## Value

character. NA is returned when schema component is not applicable to
target object. If using the replacement function, the `giotto` object or
subobject is returned

## Examples

``` r
g <- GiottoData::loadGiottoMini("vizgen")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : '/usr/bin/python3'
#>  python version : 3.12

########### Get/set existing schema values within giotto object ####
spatUnit(g)
#> [1] "aggregate" "z0"        "z1"       
featType(g)
#> [1] "rna"

# rename a spatial unit
spatUnit(g, old = "z0") <- "slice1"
#> Warning: activeSpatUnit 'z0' no longer exists.
#>  activeSpatUnit defaulted to 'aggregate'
spatUnit(g)
#> [1] "aggregate" "slice1"    "z1"       

# rename a feature type
featType(g, old = "rna") <- "feature1"
#> Warning: activeFeatType 'rna' no longer exists.
#>  activeFeatType defaulted to 'feature1'
featType(g)
#> [1] "feature1"

########### Get schema values from a list of objects ###############

glist <- as.list(g)
spatUnit(glist)
#>  [1] "slice1"    "z1"        "aggregate" "slice1"    "z1"        "aggregate"
#>  [7] "aggregate" "aggregate" NA          "slice1"    "z1"        "aggregate"
#> [13] "aggregate" "aggregate" "aggregate" "slice1"    "z1"        "aggregate"
#> [19] "slice1"    "z1"        "aggregate" "aggregate" "aggregate" "aggregate"
#> [25] "aggregate" "aggregate" NA          NA          NA          NA         
featType(glist)
#>  [1] NA         NA         NA         NA         NA         NA        
#>  [7] NA         NA         "feature1" "feature1" "feature1" "feature1"
#> [13] "feature1" "feature1" "feature1" "feature1" "feature1" "feature1"
#> [19] "feature1" "feature1" "feature1" "feature1" "feature1" "feature1"
#> [25] "feature1" "feature1" NA         NA         NA         NA        
objName(glist)
#>  [1] "slice1"           "z1"               "aggregate"        "raw"             
#>  [5] "raw"              "raw"              "Delaunay_network" "kNN_network"     
#>  [9] "feature1"         "raw"              "raw"              "raw"             
#> [13] "normalized"       "scaled"           "pearson"          NA                
#> [17] NA                 NA                 NA                 NA                
#> [21] NA                 "cluster_metagene" "sNN.pca"          "pca"             
#> [25] "umap"             "tsne"             "dapi_z0"          "dapi_z1"         
#> [29] "polyT_z0"         "polyT_z1"        

########### Get and set schema values with single subobject ########

fx <- g[["feat_meta", spat_unit = "aggregate"]][[1]]

spatUnit(fx)
#> [1] "aggregate"
spatUnit(fx) <- "foo"
spatUnit(fx)
#> [1] "foo"

featType(fx)
#> [1] "feature1"
featType(fx) <- "bar"
featType(fx)
#> [1] "bar"

ex <- g[["expression", spat_unit = "aggregate"]][[1]]

objName(ex)
#> [1] "raw"
objName(ex) <- "baz"
objName(ex)
#> [1] "baz"

prov(ex)
#> [1] "z0" "z1"
prov(ex) <- "qux"
prov(ex)
#> [1] "qux"
```
