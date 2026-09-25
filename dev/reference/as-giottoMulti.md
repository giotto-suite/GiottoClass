# Wrap a single `giotto` as a one-child `giottoMulti`.

Useful as a quick path for code that wants to operate uniformly on a
multi (no class branches), and to gain the lazy view layer
([`subset()`](https://rdrr.io/r/base/subset.html) / view-filter on read)
on top of a single giotto without committing to a destructive in-place
subset.

## Details

The child is named `"sample1"` by default. To pick a different name use
`createGiottoMulti(list(my_name = g))` directly.
