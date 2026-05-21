# Aggregate AESI outcomes

Aggregate number of AESI outcomes in a given period of time, regardless
of vaccination status.

## Usage

``` r
aggregate_data_descriptive_outcomes(
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

A data.table object matching the 'Descriptive - AESI' template,
describing the AESI outcome counts for this period.
