# Create S4 spatLocsObj

Create an S4 spatLocsObj

## Usage

``` r
createSpatLocsObj(
  coordinates,
  name = "test",
  spat_unit = "cell",
  provenance = NULL,
  misc = NULL,
  numeric_format = c("pair", "triplet"),
  verbose = TRUE,
  ...
)
```

## Arguments

- coordinates:

  spatial coordinates

- name:

  name of spatLocsObj

- spat_unit:

  spatial unit of aggregated expression (e.g. 'cell')

- provenance:

  origin data of aggregated expression information (if applicable)

- misc:

  misc

- numeric_format:

  character. One of `"pair"` (default) or `"triplet"`. Whether `numeric`
  inputs should be understood as XY pairs or XYZ triplets.

- verbose:

  verbosity

- ...:

  additional params to pass

## Value

spatLocsObj

## Examples

``` r
# from data.frame
x <- data.frame(
    cell_ID = c("cell_1", "cell_2", "cell_3"),
    sdimx = c(6637.881, 6471.978, 6801.610),
    sdimy = c(-5140.465, -4883.541, -4968.685)
)
s1 <- createSpatLocsObj(coordinates = x, name = "raw")
plot(s1)


# from matrix
m <- matrix(c(2 ,3, 4, 2), ncol = 2)
rownames(m) <- c("cell1", "cell2")
s2 <- createSpatLocsObj(m)
#> [spatlocs] matrix input has rownames.
#>  Using these as IDs.
plot(s2)


# from numeric xy pairs
num2d <- c(1, 3, 5, 9)
s3 <- createSpatLocsObj(num2d)
plot(s3)

# from numeric xyz triplets
num3d <- c(3, 2, 9, 3, 8, 5)
s4 <- createSpatLocsObj(num3d, numeric_format = "triplet")
plot(s4)

{"x":{"visdat":{"2d7b5bb6be7f":["function () ","plotlyVisDat"]},"cur_data":"2d7b5bb6be7f","attrs":{"2d7b5bb6be7f":{"x":{},"y":{},"z":{},"mode":"markers","marker":{"size":1},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter3d"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"scene":{"xaxis":{"title":"x coordinates"},"yaxis":{"title":"y coordinates"},"zaxis":{"title":"z coordinates"},"aspectmode":"manual","aspectratio":{"x":1,"y":1,"z":1}},"legend":{"x":100,"y":0.5,"font":{"family":"sans-serif","size":12}},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[3,3],"y":[2,8],"z":[9,5],"mode":"markers","marker":{"color":"rgba(31,119,180,1)","size":1,"line":{"color":"rgba(31,119,180,1)"}},"type":"scatter3d","error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"line":{"color":"rgba(31,119,180,1)"},"frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}
```
