# Aggregate post-vaccination exposure data

Aggregate AESI counts during post-vaccination risk intervals and person
days spent in those intervals, within a given period of time.

## Usage

``` r
aggregate_data_exposed(
  person_data,
  vaccination_data,
  outcome_data,
  period_start,
  period_end,
  options,
  allow_side_effects = FALSE,
  analysis_types = NULL
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

- allow_side_effects:

  Whether or not changes made to the original input data should be
  allowed to persist after the function has completed.

- analysis_types:

  Character vector defining which of the primary and sub-analyses are
  included in this study (from options file)

## Value

A list of two data.table objects, named 'cases' and person_days',
matching the 'Exposed cases' and 'Exposed person days' templates
respectively.
