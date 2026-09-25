# Structured giotto object history

The `@parameters` history as structured records: `step_id`, `fn`,
`params`, `timestamp`, `seed`, `status`, `error` and `diff`. Entries
written before structured records existed are reported with
`status = "ok"` and their recorded arguments, so old objects still read.

## Usage

``` r
ghistory_records(object)
```

## Arguments

- object:

  giotto object

## Value

list of records

## Examples

``` r
g <- GiottoData::loadGiottoMini("visium")

str(head(ghistory_records(g), 1))
```
