# Fingerprint a subobject payload

Extension point for
[`objManifest()`](https://giotto-suite.github.io/GiottoClass/dev/reference/objManifest.md).
Returns a short hash identifying the content of a payload - the matrix,
table, graph or store a subobject holds - so that a manifest diff can
tell when it changed.

GiottoClass hashes the in-memory payloads it knows itself. A package
that provides another payload representation, such as a disk-backed
store, registers a method here. The method must answer from handle state
(ids, recorded operations, schema, file metadata) and must never read
the data: an identical handle has to mean identical content, and a
changed one different content. Never hash the payload object directly.
Objects holding external pointers or environments hash by instance, not
by content.

A method returns a hash or errors; it never returns `NULL` for a payload
it was given. The default method errors, and the manifest reports that
as an unreadable fingerprint in its `warnings` rather than treating the
payload as unchanged. Only an absent payload (`NULL`) has no
fingerprint.

BPCells and HDF5Array matrices are fingerprinted from their handle: the
delayed operations, and the names and sizes of their backing files.

A function is fingerprinted by its code. Values a closure captures are
not seen, so two closures that differ only in a captured value share a
fingerprint.

## Usage

``` r
# S4 method for class 'ANY'
fingerprint_data(x, ...)

# S4 method for class 'NULL'
fingerprint_data(x, ...)

# S4 method for class 'vector'
fingerprint_data(x, ...)

# S4 method for class 'data.frame'
fingerprint_data(x, ...)

# S4 method for class 'Matrix'
fingerprint_data(x, ...)

# S4 method for class 'igraph'
fingerprint_data(x, ...)

# S4 method for class 'SpatVector'
fingerprint_data(x, k = .MANIFEST_FP_N, ...)

# S4 method for class 'affine2d'
fingerprint_data(x, ...)

# S4 method for class 'function'
fingerprint_data(x, ...)

# S4 method for class 'SpatRaster'
fingerprint_data(x, ...)
```

## Arguments

- x:

  a payload

- ...:

  additional params (none implemented)

- k:

  numeric. SpatVector only: number of geometries sampled

## Value

character scalar, or `NULL` for a `NULL` payload
