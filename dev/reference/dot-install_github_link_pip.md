# Install Package from GitHub Link

Installs `link` to python `env`

## Usage

``` r
.install_github_link_pip(link = NULL, env = NULL)
```

## Arguments

- link:

  link to github repository containing a python package, e.g.
  `git+https://github.com/TencentAILabHealthcare/pysodb.git`

- env:

  conda environment to which `link` will be installed via pip

## Value

character or NULL
