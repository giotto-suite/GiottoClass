# Parameter Classes for Data Processing Operations

Utility class that defines a data processing procedure and any params
used in performing it. Packages defining processing methods will create
their own child classes. These parameter objects are intended to be
passed alongside the data to process to
[`processData()`](https://giotto-suite.github.io/GiottoClass/reference/processData.md).

## Slots

- `param`:

  list. Named parameters to use with the intended processing operation.
  These can be accessed and updated using the `$` operator.
