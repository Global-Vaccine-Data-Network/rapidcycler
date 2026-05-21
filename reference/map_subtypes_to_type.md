# Map vaccine subtypes to types

Map vaccine subtypes to types

## Usage

``` r
map_subtypes_to_type(data, vaccine_mapping)
```

## Arguments

- data:

  A data.table containing at least a column called V_SUBTYPE

- vaccine_mapping:

  A data.table containing columns called V_SUBTYPE and V_TYPE

## Value

A data.table identical to data input, but with an extra column called
V_TYPE
