# Affine Transform Object

Container for accumulating 2D affine transformations. Simple spatial
transforms
([`spatShift()`](https://giotto-suite.github.io/GiottoClass/reference/spatShift.md),
[`spin()`](https://giotto-suite.github.io/GiottoClass/reference/spin.md),
[`rescale()`](https://giotto-suite.github.io/GiottoClass/reference/rescale.md),
[`flip()`](https://giotto-suite.github.io/GiottoClass/reference/flip.md),
[`t()`](https://giotto-suite.github.io/GiottoClass/reference/transpose.md),
[`shear()`](https://giotto-suite.github.io/GiottoClass/reference/shear.md))
can be chained on an `affine2d` to build up a combined transform, which
is then applied to spatial objects in a single step via
[`affine()`](https://giotto-suite.github.io/GiottoClass/reference/affine.md).

Create an identity `affine2d` with
[`affine()`](https://giotto-suite.github.io/GiottoClass/reference/affine.md)
(no arguments). Before chaining centroid-relative operations
([`spin()`](https://giotto-suite.github.io/GiottoClass/reference/spin.md),
[`rescale()`](https://giotto-suite.github.io/GiottoClass/reference/rescale.md))
without explicit `x0`/`y0`, set `ext(aff) <- ext(your_object)` so the
pivot point matches your data.

The combined linear transform is stored as a 3x3 homogeneous matrix in
`@affine`. Translations are encoded in column 3 (rows 1-2). The
convention is post-multiply: `xy_out = xy_in %*% A`.

## Slots

- `anchor`:

  numeric(4). Anchoring spatial extent (`xmin, xmax, ymin, ymax`). Set
  via `ext(aff) <-` to match the data extent. Used as the centroid pivot
  for
  [`spin()`](https://giotto-suite.github.io/GiottoClass/reference/spin.md)
  and
  [`rescale()`](https://giotto-suite.github.io/GiottoClass/reference/rescale.md)
  when `x0`/`y0` are not supplied, and for translation calculation
  during transform composition.

- `affine`:

  matrix. 3x3 homogeneous transform matrix encoding the combined
  linear + translation transform.

- `order`:

  character. Records the order in which component operations were
  applied.

- `rotate`:

  numeric(1). Accumulated rotation angle in radians.

- `shear`:

  numeric(2). Accumulated shear factors `(x, y)`.

- `scale`:

  numeric(2). Accumulated scale factors `(x, y)`.

- `translate`:

  numeric(2). Accumulated translation `(x, y)`.
