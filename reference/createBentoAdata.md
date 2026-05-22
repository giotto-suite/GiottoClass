# Create bento adata object from gobject

Create bento adata object from gobject

## Usage

``` r
createBentoAdata(gobject = NULL, env_to_use = "giotto_env")
```

## Arguments

- gobject:

  Giotto object

- env_to_use:

  Python environment within which bento is installed. If it is not
  already installed, the user will be prompted to install `bento-tools`
  DEFAULT: "giotto_env"

## Value

bento_adata bento adata object
