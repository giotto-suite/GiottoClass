# Parameter Classes for Data Analysis Operations

Utility class that defines a data analysis procedure and any params used
in performing it. Packages defining analysis methods will create their
own child classes. These parameter objects are intended to be passed
alongside the data to analyze to
[`analyzeData()`](https://giotto-suite.github.io/GiottoClass/dev/reference/analyzeData.md).

## Slots

- `param`:

  list. Named parameters to use with the intended analysis operation.
  These can be accessed and updated using the `$` operator.
