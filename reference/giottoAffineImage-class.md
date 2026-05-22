# S4 giottoAffineImage Class

Class extending `giottoLargeImage`. When
[`shear()`](https://giotto-suite.github.io/GiottoClass/reference/shear.md)
or
[`spin()`](https://giotto-suite.github.io/GiottoClass/reference/spin.md)
operations are performed on a `giottoLargeImage`, this class is
instantiated. It provides a way of storing the affine transformation and
also lazily performing it when required for a plotting preview. It is
possible to force the deferred affine transform using
[`doDeferred()`](https://giotto-suite.github.io/GiottoClass/reference/doDeferred.md)
and return a processed `giottoLargeImage`.

## Value

`giottoAffineImage`

## Slots

- `affine`:

  contains `affine2d` object allowing lazily performed spatial
  transforms

- `funs`:

  list of functions associated with the object. Primarily to perform the
  delayed/lazy operation
