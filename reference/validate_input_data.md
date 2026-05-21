# Validate input datasets before aggregating

Checks input datasets for any obvious issues, including incorrect column
names or types, duplicated data, date ranges etc. This function is run
automatically from within the
[`aggregate_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data.md)
function, therefore does not need to be run manually (however it is
possible to do so if you wish to).

If validating historical input data, set `vaccination_data = NULL`.

## Usage

``` r
validate_input_data(
  person_data = NULL,
  vaccination_data,
  outcome_data,
  cycle_start_date,
  cycle_end_date,
  options_file_location = getwd(),
  options = NULL,
  split_size = 5e+05,
  skip_user_prompts = FALSE,
  patient_data = NULL
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

- cycle_start_date:

  Start date of cycle, as a date object, an 8 digit number of the form
  yyyymmdd, or a string of the form yyyy-mm-dd

- cycle_end_date:

  End date of cycle, as a date object, an 8 digit number of the form
  yyyymmdd, or a string of the form yyyy-mm-dd

- options_file_location:

  Location where options.txt file is stored. Supply either
  `options_file_location` or `options`, not both. Defaults to
  [`getwd()`](https://rdrr.io/r/base/getwd.html)

- options:

  Options object, already converted from txt file. Supply either
  options_file_location or options, not both

- split_size:

  Input data will be automatically split into pieces of this size
  (default 500,000). If memory is an issue, lower the split_size.

- skip_user_prompts:

  Boolean allowing user prompts to be skipped, mainly for the purpose of
  testing for which prompts are not possible. Recommended to leave as
  FALSE.

- patient_data:

  Deprecated. Use `person_data` instead.

## Value

A character vector of issues found. If no issues are found, the vector
will be empty.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate synthetic data and validate it
data <- generate_synthetic_data(pop_size = 1000, save_data = FALSE)

issues <- validate_input_data(
  person_data = data$person_data,
  vaccination_data = data$vaccination_data,
  outcome_data = data$outcome_data,
  cycle_start_date = 20210101,
  cycle_end_date = 20230101,
  options_file_location = "path/to/options",
  skip_user_prompts = TRUE
)

if (length(issues) == 0) message("No issues found!")
} # }
```
