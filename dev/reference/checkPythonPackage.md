# Check Python Package Installation

checks python environment for a provided package, installs if it is not
found.

## Usage

``` r
checkPythonPackage(
  package_name = NULL,
  github_package_url = NULL,
  env_to_use = "giotto_env"
)
```

## Arguments

- package_name:

  name of python package. See details.

- github_package_url:

  URL linking to github repository containing a python package that may
  be installed with pip, e.g.
  `git+https://github.com/TencentAILabHealthcare/pysodb.git`; see
  details.

- env_to_use:

  name of the environment into which the python package should be
  installed.

## Value

character or NULL

## Details

Parameter `github_package_url` takes precedent over `package_name`, i.e.
if both are provided, only the github URL will be installed. This
function should only be provided one parameter, or the other.
