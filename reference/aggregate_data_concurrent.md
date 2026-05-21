# Aggregate data with concurrent comparators

Aggregate risk/control AESI counts and ratio according to concurrent
study designs, within a given period of time.

## Usage

``` r
aggregate_data_concurrent(
  person_data,
  vaccination_data,
  outcome_data,
  period_start,
  period_end,
  options,
  comparator = "vaccinated",
  allow_side_effects = FALSE,
  analysis_types = NULL,
  return_preaggregated = FALSE
)
```

## Arguments

- person_data:

  Person data input data.table

- vaccination_data:

  Vaccination data input data.table. Set to NULL to perform historical
  data aggregation

- outcome_data:

  AESI outcome data input data.table

- period_start:

  Start date of period, as a date object. Period lengths are defined in
  the options file, with each cycle possibly containing multiple
  periods. Only a single period is aggregated at a time.

- period_end:

  End date of period, as a date object. Period lengths are defined in
  the options file, with each cycle possibly containing multiple
  periods. Only a single period is aggregated at a time.

- options:

  Options object, already converted from txt file. Supply either
  options_file_location or options, not both

- comparator:

  Specify whether a 'vaccinated' or 'unvaccinated' comparator group is
  used

- allow_side_effects:

  Whether or not changes made to the original input data should be
  allowed to persist after the function has completed.

- analysis_types:

  Character vector defining which of the primary and sub-analyses are
  included in this study (from options file)

- return_preaggregated:

  Due to the calculation of ratios and assignment of controls, two fully
  aggregated tables can not be combined under this study design. When
  TRUE, the function will not combine risk/control groups and calculate
  the ratio, but instead return these objects separately in a list. This
  allows the majority of the aggregation to be performed in stages,
  before combining and calculating the ratio at the end (using
  [`concurrent_final_aggregation()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/concurrent_final_aggregation.md)
  function)

## Value

If `return_preaggregated = FALSE`, returns a data.table object matching
the 'Concurrent - Vac' or 'Concurrent - Unvac' templates. If
`return_preaggregated = TRUE`, returns a list of four data.table objects
corresponding to the control/risk counts and control/risk groups, which
are required to produce the final table.
