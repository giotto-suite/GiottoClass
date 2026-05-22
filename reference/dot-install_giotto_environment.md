# .install_giotto_environment

installation options of giotto environment

## Usage

``` r
.install_giotto_environment(
  force_environment = FALSE,
  packages_to_install = c("pandas", "networkx", "python-igraph", "leidenalg",
    "python-louvain", "python.app", "scikit-learn", "smfishhmrf", "session-info"),
  pip_packages = c("python-louvain", "smfishhmrf", "session-info"),
  python_version = "3.10.2",
  mini_install_path = NULL,
  confirm = TRUE,
  envname = "giotto_env",
  conda = "auto",
  verbose = NULL
)
```

## Value

character or NULL
