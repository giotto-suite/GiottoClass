# Add alpha channel to image array

Add alpha channel to image array

## Usage

``` r
add_img_array_alpha(x, alpha)
```

## Arguments

- x:

  image array to use

- alpha:

  global alpha value to use. Numeric. Scales from 0 to 1, with 0 being
  fully transparent and 1 being fully visible

## Value

image array with 4th channel for transparency

## Details

Add 4th alpha channel to 3 channel RGB image arrays

## Examples

``` r
x <- matrix(rnorm(4), nrow = 2)

add_img_array_alpha(x, alpha = 0.1)
#> , , 1
#> 
#>            [,1]       [,2]
#> [1,] -0.8289657 -0.8621887
#> [2,] -0.8413987  0.6702333
#> 
#> , , 2
#> 
#>            [,1]       [,2]
#> [1,] -0.8289657 -0.8621887
#> [2,] -0.8413987  0.6702333
#> 
#> , , 3
#> 
#>            [,1]       [,2]
#> [1,] -0.8289657 -0.8621887
#> [2,] -0.8413987  0.6702333
#> 
#> , , 4
#> 
#>      [,1] [,2]
#> [1,]  0.1  0.1
#> [2,]  0.1  0.1
#> 
```
