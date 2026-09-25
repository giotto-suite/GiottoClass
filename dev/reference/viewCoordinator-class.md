# viewCoordinator virtual class

Base class for view + space resolution coordinators — polymorphic
dispatch surface that brokers IDs and joins between storage backings
during view resolution. The execution engine itself is owned by the
storage (see GiottoDisk::storeRead and its `output` modes).

Concrete subclasses include
[dataTableCoordinator](https://giotto-suite.github.io/GiottoClass/dev/reference/dataTableCoordinator-class.md)
in GiottoClass and (registered when loaded) `duckDBCoordinator` /
`sedonaCoordinator` in GiottoDisk.

## Value

`viewCoordinator`-inheriting object

## Slots

- `misc`:

  `list` for backend-specific options.
