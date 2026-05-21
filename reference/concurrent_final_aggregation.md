# Complete aggregation of data with concurrent comparators

Helper function to combine risk/control AESI counts and risk/control
groups and calculate ratio, for concurrent study designs.

## Usage

``` r
concurrent_final_aggregation(
  control_group,
  risk_group,
  control_counts,
  case_counts,
  output,
  analysis_types,
  options
)
```

## Arguments

- control_group:

  Table describing people in control group on each day, obtained from
  [`aggregate_data_concurrent()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data_concurrent.md)
  function.

- risk_group:

  Table describing people in risk group on each day, obtained from
  [`aggregate_data_concurrent()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data_concurrent.md)
  function.

- control_counts:

  Table describing AESI counts from control group on each day, obtained
  from
  [`aggregate_data_concurrent()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data_concurrent.md)
  function.

- case_counts:

  Table describing AESI counts from risk group on each day, obtained
  from
  [`aggregate_data_concurrent()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data_concurrent.md)
  function.

- output:

  A data.table containing already aggregated results, to which these
  results will be added

- analysis_types:

  Character vector defining which of the primary and sub-analyses are
  included in this study (from options file)

- options:

  Options object, already converted from txt file. Supply either
  options_file_location or options, not both

## Value

A data.table object matching the 'Concurrent - Vac' or 'Concurrent -
Unvac' templates.
