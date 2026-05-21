# Create notes file

This creates a txt file containing notes regarding the data aggregation,
including some basic descriptive statistics (cohort size, total
vaccinations, total outcomes) and the options that were used.

## Usage

``` r
create_notes_file(
  cycle_start_date,
  cycle_end_date,
  site_code,
  design_selection,
  options,
  stored_parameters,
  save_location
)
```

## Arguments

- cycle_start_date:

  Start date of cycle, as a date object, an 8 digit number of the form
  yyyymmdd, or a string of the form yyyy-mm-dd

- cycle_end_date:

  End date of cycle, as a date object, an 8 digit number of the form
  yyyymmdd, or a string of the form yyyy-mm-dd

- site_code:

  Site code (e.g. AUS_MCRI), as a string

- design_selection:

  Character vector of analytical study designs to run. Valid values are:
  `"self_post"`, `"self_pre"`, `"historical"`, `"concurrent_vac"`,
  `"concurrent_unvac"`. Default is NULL, which runs all five designs.
  Descriptive outcomes and vaccination datasets are always produced for
  non-historical aggregations and cannot be excluded via this parameter.

- options:

  Options object, already converted from txt file

- stored_parameters:

  A list of parameters used in this aggregation, saved temporarily
  whilst aggregation is ongoing

- save_location:

  Location in which to save the created txt file
