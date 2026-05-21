# Aggregate data with self controls

Aggregate risk/control AESI counts and window ratios according to
self-controlled study designs, within a given period of time.

## Usage

``` r
aggregate_data_self(
  person_data,
  vaccination_data,
  outcome_data,
  period_start,
  period_end,
  options,
  comparator = "post",
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

- comparator:

  Specify whether a 'post' or 'pre' vaccination control window is used.

- allow_side_effects:

  Whether or not changes made to the original input data should be
  allowed to persist after the function has completed.

- analysis_types:

  Character vector defining which of the primary and sub-analyses are
  included in this study (from options file)

## Value

A data.table object matching the 'SCRI - post' or 'SCRI - pre'
templates.
