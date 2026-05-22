# Giotto python environment

Giotto has several functions that utilize python packages. To facilitate
this, utilities are provided for creating, removing, and attaching
python environments. Python environments are currently handled entirely
through reticulate.

**Creating an environment**

`installGiottoEnvironment()` can be used to create a default miniconda
environment called `giotto_env` that includes some commonly used python
packages. See the **python versions** section for specific packages and
version numbers.

Custom environments manageable through reticulate are also compatible
and can be hooked into by Giotto after creation.

`checkGiottoEnvironment()` can be used in order to test if an envname or
full python path is accessible by Giotto. It will also check the
`"giotto.py_path"` option.

**Choosing environments**

Only one python environment may be initialized and used by reticulate
during a single R session. In order to switch to another environment,
the R session must be restarted.

Whenever any of the following happens for the first time in a session:

- `giotto` object creation (due to creation of a default
  `giottoInstructions`)

- `giottoInstructions` creation
  ([`createGiottoInstructions()`](https://giotto-suite.github.io/GiottoClass/reference/giotto_instructions.md))

- `GiottoClass::set_giotto_python_path()` is called (most direct)

For the above, Giotto automatically detects AND activates a python
environment based on the following defaults in decreasing priority:

1.  User provided (when `python_path` param is not `NULL`)

2.  Any provided path or envname in option `"giotto.py_path"`

3.  Default expected giotto environment location based on
    [`reticulate::miniconda_path()`](https://rstudio.github.io/reticulate/reference/miniconda_path.html)

4.  Envname `"giotto_env"`

5.  System default python environment

This behavior is mediated by the `set_giotto_python_path()` utility
function, which will find an environment and then initialize it.

## Usage

``` r
checkGiottoEnvironment(
  envname = NULL,
  mini_install_path = deprecated(),
  verbose = NULL
)

installGiottoEnvironment(
  packages_to_install = c("pandas==1.5.1", "networkx==2.8.8", "python-igraph==0.10.2",
    "leidenalg==0.9.0", "python-louvain==0.16", "python.app==1.4", "scikit-learn==1.1.3",
    "smfishhmrf", "session-info"),
  pip_packages = c("python-louvain", "smfishhmrf", "session-info"),
  python_version = "3.10.2",
  mini_install_path = NULL,
  confirm = TRUE,
  envname = "giotto_env",
  conda = "auto",
  force_miniconda = FALSE,
  force_environment = FALSE,
  verbose = NULL
)

removeGiottoEnvironment(
  envname = "giotto_env",
  mini_path = deprecated(),
  conda = "auto",
  verbose = TRUE
)

set_giotto_python_path(python_path = NULL, verbose = NULL, initialize = TRUE)
```

## Arguments

- envname:

  character. (optional) The name of a miniconda or conda environment OR
  path to a python executable. When using `installGiottoEnvironment()`,
  the default is `"giotto_env"`

- mini_install_path:

  (optional) desired miniconda installation location. Default is chosen
  by
  [`reticulate::install_miniconda()`](https://rstudio.github.io/reticulate/reference/install_miniconda.html)

- verbose:

  be verbose

- packages_to_install:

  python modules (packages) to install for Giotto.

- pip_packages:

  python packages mush installed with pip, only names are needed

- python_version:

  python version to use within the giotto conda environment. Default is
  v3.10.2

- confirm:

  whether to pause for confirmation of conda environment install
  location (default = TRUE)

- conda:

  either "auto" (default) to allow reticulate to handle it, or the full
  filepath to the conda executable. You can also set the option
  `"reticulate.conda_binary"` or
  [`Sys.setenv()`](https://rdrr.io/r/base/Sys.setenv.html)
  `"RETICULATE_CONDA"` to tell reticulate where to look.

- force_miniconda:

  force reinstallation of miniconda

- force_environment:

  force reinstallation of the giotto environment

- mini_path:

  deprecated

- python_path:

  character. Name of environment or full path to python executable.

- initialize:

  force initialization of set python path. Default = TRUE.

## Value

installed Giotto environment

## Functions

- `checkGiottoEnvironment()`:

  - Based on `envname`, detect if there a conda or miniconda environment
    accessible by Giotto. By default, the `envname` `"giotto_env"`, then
    the option `"giotto.py_path"` is checked, but an alternative can be
    provided.

  - Setting `envname` as `":auto:"` will let Giotto autodetect a python
    env to use. See section for `set_giotto_python_path()` for details
    on the autodetection.

  - Returns `TRUE` if an env is detected and accessible by Giotto.
    `FALSE` if not. Will not initialize a python environment during
    detection.

- `installGiottoEnvironment()`:

  - Install a giotto python environment using miniconda through
    reticulate. By default, the envname used will be `"giotto_env"`. If
    another name is used, you will have to provide that envname at the
    start of a session (see **Choosing an environment** above).  
    This includes a miniconda installation and also a set of python
    packages that Giotto may often use. See details for further
    information on setting up an environment with a .yml

  - Returns `NULL`

- `removeGiottoEnvironment()`:

  - Remove a python environment

  - Returns `NULL`

- `set_giotto_python_path()`:

  - Detect and activate a python path. The `python_path` param accepts
    both full filepaths to the python executable and envnames. The final
    path to use is determined as follows in decreasing priority:

    1.  User provided (when `python_path` is not `NULL`)

    2.  Any provided path or envname in option `"giotto.py_path"`

    3.  Default expected giotto environment location based on
        [`reticulate::miniconda_path()`](https://rstudio.github.io/reticulate/reference/miniconda_path.html)

    4.  Envname "giotto_env"

    5.  System default python environment

  - This function exits without doing anything if option
    `"giotto.use_conda"` is `FALSE`.

  - By default this function will force initialization of the python
    environment to set, locking the session to that environment. This
    can be skipped if `initialize = FALSE`, however in that case, the
    actual python path set downstream may differ from what is expected
    and reported by this function.

  - Returns detected path to python binary or `NULL` if none found.

## python versions

By default, Python v3.10.2 will be used with the following python
modules for giotto environment installation:


       - pandas==1.5.1
       - networkx==2.8.8
       - python-igraph==0.10.2
       - leidenalg==0.9.0
       - python-louvain==0.16
       - python.app==1.4
       - scikit-learn==1.1.3

The giotto environment can be custom installed by changing the
python_version parameter and module versions in the
`packages_to_install` parameter.

For example, this giotto environment works as well, and was the default
environment status for past releases of Giotto. Python v3.6


      - pandas==1.1.5
      - networkx==2.6.3
      - python-igraph==0.9.6
      - leidenalg==0.8.7
      - python-louvain==0.15
      - python.app==2 # macOS only
      - scikit-learn==0.24.2

## .yml installs

Please note that multiple .yml files are provided in the repository for
advanced installation and convenience. To install the most up-to-date
Giotto environment using a .yml file, open a shell compatible with
conda/miniconda and navigate to the directory specified by
system.file(package = "Giotto", "python/configuration"). Once in this
directory, run the following to create your environment in one step:

    conda env create -n giotto_env -f ./genv.yml

## Examples

``` r
# detect without initialization
# check default env location
checkGiottoEnvironment()
#> Giotto can access environment found at:
#>  '/usr/share/miniconda/envs/giotto_env/bin/python'
#>  If this is the wrong environment, try specifying `envname` param
#>  or set option "giotto.py_path" with the desired envname or path
#> [1] TRUE

# use environment name
checkGiottoEnvironment("giotto_env")
#> Giotto can access environment found at:
#>  '/usr/share/miniconda/envs/giotto_env/bin/python'
#>  If this is the wrong environment, try specifying `envname` param
#>  or set option "giotto.py_path" with the desired envname or path
#> [1] TRUE

# full path
# (use this if a different install location specified with .condarc)
if (FALSE) {
    checkGiottoEnvironment(
        "/Users/example/Library/r-miniconda-arm64/envs/giotto_env/bin/pythonw"
    )
}
if (FALSE) {
    # default environment installation
    installGiottoEnvironment()

    # install to alternate location
    temp_env <- tempdir()
    installGiottoEnvironment(mini_install_path = temp_env)
}
# detect AND initialize a python environment
if (FALSE) {
    set_giotto_python_path()
}
```
