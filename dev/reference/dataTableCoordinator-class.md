# dataTableCoordinator

In-memory coordinator. Carries surviving cell_IDs as a plain R character
vector and applies them via `[cell_ID %in% ids]`-style filtering. The
reference implementation and always-works fallback for any gobject
regardless of backing (at the cost of materializing backed subobjects
into R memory).

## Usage

``` r
dataTableCoordinator()
```

## Value

`dataTableCoordinator`

## Examples

``` r
dataTableCoordinator()
```
