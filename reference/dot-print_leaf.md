# Hierarchical tree printing

Hierarchical tree printing

## Usage

``` r
.print_leaf(level_index, availableDT, inherit_last = TRUE, indent)
```

## Arguments

- level_index:

  Which col of availability matrix to start print from

- availableDT:

  availability matrix given as data.table

- inherit_last:

  (boolean) determine behavior from previous level for last level
  (intended for values print) Only TRUE behavior defined. \#TODO

- indent:

  indent characters to print for this level (top level is ”)

## Value

Hierarchical tree

## Details

Much inspiration taken from https://rdrr.io/cran/fs/src/R/tree.R
