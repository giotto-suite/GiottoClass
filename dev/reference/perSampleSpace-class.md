# Class for a frame applied to each sample independently

A frame whose samples never interact: each sits in its own copy of it,
so a job over this frame is N independent jobs. Steps may still be
scoped – two sections each rotated upright by a different angle are
per-sample, because nothing about that puts them in a shared coordinate
system.

Its membership is OPEN:
[`names()`](https://giotto-suite.github.io/GiottoClass/dev/reference/names.md)
reports the samples its steps mention, but an unscoped step also reaches
samples that appear nowhere in the recipe. Coverage is therefore
whatever the object holds, which is why a consumer sizing a job reads it
from the object rather than asking the space.

It is the kind you get by default: recording onto an unused name with
`space = "<name>"` creates one. That is because the kind decides job
size, and only this size round-trips — N artifacts, one per child, which
is the shape reading per child hands back.

## Value

a `perSampleSpace` object

## See also

[giottoSpace](https://giotto-suite.github.io/GiottoClass/dev/reference/giottoSpace-class.md),
[`perSampleSpace()`](https://giotto-suite.github.io/GiottoClass/dev/reference/space-constructors.md)
