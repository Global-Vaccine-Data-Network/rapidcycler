# Aggregate historic data

Aggregate historic AESI counts and person days in a given period of
time, not dependent on vaccination status.

## Usage

``` r
aggregate_data_historic(
  person_data,
  outcome_data,
  period_start,
  period_end,
  options,
  allow_side_effects = FALSE
)
```

## Arguments

- person_data:

  Person data input data.table

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

  Options object, already converted from txt file.

- allow_side_effects:

  Whether or not changes made to the original input data should be
  allowed to persist after the function has completed.

## Value

A list of two data.table objects, named 'cases' and person_days',
matching the 'Historic cases' and 'Historic person days' templates
respectively.
