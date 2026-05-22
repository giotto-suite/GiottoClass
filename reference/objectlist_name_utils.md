# Name wrangling for subobject lists

Name wrangling for subobject lists

## Usage

``` r
assign_objnames_2_list(obj_list, force_replace = FALSE)

assign_listnames_2_obj(obj_list)

.uniquify_dups(x, sep = ".", what = "", verbose = NULL)
```

## Arguments

- obj_list:

  list containing giotto subobjects

- force_replace:

  logical. default = FALSE. Whether to replace the names of objects for
  which the name already has a name for

- x:

  character vector

- sep:

  character. Separator used when making names unique. Default is "."

- what:

  character (optional). Description of character vector input used when
  printing verbose messages about what was made unique

- verbose:

  be verbose

## Value

list

## Examples

``` r
if (FALSE) { # \dontrun{
e <- new("exprObj")
a <- replicate(3L, e)
# name the objects
objName(a) <- letters[1:3]

# assign the object stored names to the list
names(a)
a <- assign_objnames_2_list(a)
names(a)

# assign list names to the object
b <- list("name_to_set" = new("exprObj"), "name2" = new("exprObj"))
objName(b)
b <- assign_listnames_2_obj(b)
objName(b)


# ensure that character values are unique
input <- c("a", "b", "b", "c", "a", "a", "a")
.uniquify_dups(input, verbose = FALSE)
} # }
```
