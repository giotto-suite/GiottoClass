# Install Python Package with Reticulate

Install Python Package with Reticulate

## Usage

``` r
.install_py_pkg_reticulate(package = NULL, env = NULL)
```

## Arguments

- package:

  name of python package

- env:

  name of the environment into which the python package should be
  installed.

## Value

character or NULL

## Details

Installs `package` to python `env` after prompting user. Installation is
done via `py_install` from the `reticulate` package.
