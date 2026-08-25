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

########### Get/set existing schema values within giotto object ####
spatUnit(g)
featType(g)

# rename a spatial unit
spatUnit(g, old = "z0") <- "slice1"
spatUnit(g)

# rename a feature type
featType(g, old = "rna") <- "feature1"
featType(g)

########### Get schema values from a list of objects ###############

glist <- as.list(g)
spatUnit(glist)
featType(glist)
objName(glist)

########### Get and set schema values with single subobject ########

fx <- g[["feat_meta", spat_unit = "aggregate"]][[1]]

spatUnit(fx)
spatUnit(fx) <- "foo"
spatUnit(fx)

featType(fx)
featType(fx) <- "bar"
featType(fx)

ex <- g[["expression", spat_unit = "aggregate"]][[1]]

objName(ex)
objName(ex) <- "baz"
objName(ex)

prov(ex)
prov(ex) <- "qux"
prov(ex)
```
