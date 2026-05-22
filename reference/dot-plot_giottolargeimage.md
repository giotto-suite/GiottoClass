# .plot_giottolargeimage

Renders a `giottoLargeImage` by setting a lazy spatial window to the
requested extent, inferring bit depth from `max_intensity` to set the
display range, then dispatching to
[`terra::plotRGB()`](https://rspatial.github.io/terra/reference/plotRGB.html)
for multi-layer/RGB images or
[`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html)
for single-channel images with a linear-stretched greyscale palette.

## Usage

``` r
.plot_giottolargeimage(
  x,
  ext = NULL,
  xmin = NULL,
  xmax = NULL,
  ymin = NULL,
  ymax = NULL,
  max_intensity = NULL,
  asRGB = FALSE,
  ...
)
```

## Arguments

- x:

  `giottoLargeImage`

- max_intensity:

  (optional) value to treat as maximum intensity in color scale

- asRGB:

  (optional) logical. Force RGB plotting if not automatically detected

- ...:

  additional params to pass to terra::plot or terra::plotRGB depending
  on image type

- crop_extent:

  (optional) extent object to focus on specific region of image

- xmax_crop, xmin_crop, ymax_crop, ymin_crop:

  (optional) crop min/max x and y bounds

- stretch:

  character. Option to stretch the values to increase contrast: "lin"
  (linear) or "hist" (histogram)

- smooth:

  boolean. Default = TRUE. Whether to apply smoothing on the image

- mar:

  plot margins default = c(3,5,1.5,1)

- legend:

  whether to plot legend of color scale (grayscale only). default =
  FALSE

- maxcell:

  positive integer. Maximum number of image cells to use for the plot

- col:

  character. Colors for single channel images. The default is
  grDevices::grey.colors(n = 256, start = 0, end = 1, gamma = 1). It can
  also be a data.frame with two columns (value, color) to get a
  "classes" type legend or with three columns (from, to, color) to get
  an "interval" type legend

- asp:

  numeric (default = 1). Specific aspect ratio to use

## Value

plot
