# allMatrix

Class union of the matrix representations that expression-type methods
dispatch on: base `matrix` and Matrix classes, plus `DelayedArray` and
BPCells `IterableMatrix` when those packages are installed. The optional
members are added at load time (see `zzz.R`), so this package does not
depend on either.
