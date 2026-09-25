# gmulti sample group accessor

Get or set the `@groups` slot: registers a name that refers to several
samples at once. A group name is usable **anywhere a sample name is** —
`samples =`, `object =`, every getter — so registering one adds no new
parameter to any signature.

    gmultiGroup(mg, "tumor_pair") <- c("B191", "B215")
    getCellMetadata(mg, samples = "tumor_pair")
    spatPlot2D(mg, samples = "tumor_pair")

Groups may name other groups; membership is resolved recursively at use
time, deduplicated with [`unique()`](https://rdrr.io/r/base/unique.html)
in first-appearance order, so overlapping groups never read a child
twice. Assigning `NULL` drops a group.

## Usage

``` r
gmultiGroup(x, group, ...)

# S4 method for class 'giottoMulti'
gmultiGroup(x, group, ...)

gmultiGroup(x, group, ...) <- value

# S4 method for class 'giottoMulti'
gmultiGroup(x, group, ...) <- value

gmultiGroups(x, ...)

# S4 method for class 'giottoMulti'
gmultiGroups(x, ...)
```

## Arguments

- x:

  a `giottoMulti`

- group:

  `character(1)`. Group name.

- ...:

  additional arguments, currently unused

- value:

  `character` of member names (samples or other groups), or `NULL` to
  drop the group. May be named, in which case the **names** are the
  members and the values are per-child content handles, matching the
  shape of a `@mapping` entry.

## Value

`gmultiGroup()` the member vector, or `NULL` if unregistered;
`gmultiGroups()` the registered group names

## Late binding

A group stores the names it was given, not the samples they resolved to
at the time. Membership is expanded when the group is *used*, so a group
keeps tracking the child population as it changes. The same reason
filter predicates resolve against current metadata rather than against a
snapshot.

The consequence is that a group naming a child that has since been
removed is an error at use time, not at removal time — `@groups` is
deliberately not pruned by `[`. Renaming a child *does* rewrite group
members, because a rename has one correct rewrite and a removal has
none.

## Collisions

A group and a child may not share a name, and the clash is rejected at
registration from whichever side arrives second — `gmultiGroup<-`
refuses a name already held by a child, and `[[<-` / `names<-` refuse a
child name already held by a group. Resolution therefore never has to
break a tie, which is the point: a tie-break rule would make the answer
depend on registration order.

## Examples

``` r
if (FALSE) { # \dontrun{
gmultiGroup(mg, "tumor_pair") <- c("B191", "B215")
gmultiGroup(mg, "all_tumor") <- c("tumor_pair", "B651")  # nested
gmultiGroups(mg)
gmultiGroup(mg, "tumor_pair") <- NULL
} # }
```
