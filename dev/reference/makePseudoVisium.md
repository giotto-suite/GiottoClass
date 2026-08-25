# makePseudoVisium

Generates a visium-like array of spots across a provided spatial extent.

## Usage

``` r
makePseudoVisium(
  extent = NULL,
  micron_scale = 1,
  micron_size = deprecated(),
  name = "pseudo_visium"
)
```

## Arguments

- extent:

  SpatExtent or anything else a SpatExtent can be extracted or created
  from

- micron_scale:

  scalefactor needed to convert the target coordinate space to microns.
  For supported datasets, this can be found from
  `instructions(gobject, "micron_scale")`. See details.

- micron_size:

  deprecated. Use `micron_scale`

- name:

  character. (default is 'pseudo_visium') Name of giottoPolygon object
  to create

## Value

A giottoPolygon for the pseudo-visium spots.

## Details

This function generates a pseudo-Visium array of spots across the
spatial extent provided. The `micron_scale` param is used to determine
the scaling of the array relative to the target coordinate system.

## `micron_scale`

If `a` is microns and `b` is dataset coordinate units, `micron_scale` is
calculated as `a / b`.

## Examples

``` r
e <- ext(0, 2000, 0, 2000)
x <- makePseudoVisium(extent = e, micron_scale = 1)
plot(x)
```
