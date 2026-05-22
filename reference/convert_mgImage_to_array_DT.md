# convert_mgImage_to_array_DT

converts a magick image object to a data.table

## Usage

``` r
convert_mgImage_to_array_DT(mg_object)
```

## Arguments

- mg_object:

  magick image or Giotto image object

## Value

data.table with image pixel information

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")
#> 1. read Giotto object
#> 2. read Giotto feature information
#> 3. read Giotto spatial information
#> 4. read Giotto image information
#> python already initialized in this session
#>  active environment : 'giotto_env'
#>  python version : 3.10
g_image <- getGiottoImage(g, name = "image")
mgimg <- as(g_image, "giottoImage")

a <- convert_mgImage_to_array_DT(mgimg)
force(a)
#> Key: <x, y>
#>            x     y       c.1       c.2       c.3     4     RGB
#>        <int> <int>     <num>     <num>     <num> <num>  <char>
#>     1:     1     1 0.5098039 0.5215686 0.5019608     1 #828580
#>     2:     1     2 0.5098039 0.5176471 0.5019608     1 #828480
#>     3:     1     3 0.5098039 0.5176471 0.5019608     1 #828480
#>     4:     1     4 0.5098039 0.5176471 0.4980392     1 #82847F
#>     5:     1     5 0.5098039 0.5176471 0.4980392     1 #82847F
#>    ---                                                        
#> 41792:   172   239 0.3803922 0.3058824 0.4313725     1 #614E6E
#> 41793:   172   240 0.4313725 0.3372549 0.4509804     1 #6E5673
#> 41794:   172   241 0.4196078 0.3215686 0.4392157     1 #6B5270
#> 41795:   172   242 0.4470588 0.3215686 0.4549020     1 #725274
#> 41796:   172   243 0.4392157 0.2941176 0.4470588     1 #704B72
force(a)
#> Key: <x, y>
#>            x     y       c.1       c.2       c.3     4     RGB
#>        <int> <int>     <num>     <num>     <num> <num>  <char>
#>     1:     1     1 0.5098039 0.5215686 0.5019608     1 #828580
#>     2:     1     2 0.5098039 0.5176471 0.5019608     1 #828480
#>     3:     1     3 0.5098039 0.5176471 0.5019608     1 #828480
#>     4:     1     4 0.5098039 0.5176471 0.4980392     1 #82847F
#>     5:     1     5 0.5098039 0.5176471 0.4980392     1 #82847F
#>    ---                                                        
#> 41792:   172   239 0.3803922 0.3058824 0.4313725     1 #614E6E
#> 41793:   172   240 0.4313725 0.3372549 0.4509804     1 #6E5673
#> 41794:   172   241 0.4196078 0.3215686 0.4392157     1 #6B5270
#> 41795:   172   242 0.4470588 0.3215686 0.4549020     1 #725274
#> 41796:   172   243 0.4392157 0.2941176 0.4470588     1 #704B72
```
