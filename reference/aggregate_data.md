# Aggregate line list datasets

This function will take the input datasets and create a folder
containing the aggregated tables as separate parquet files, depending on
your design selection. A separate folder is created for each cycle
period.

Aggregation can be stopped and resumed (see
[`resume_cycle_aggregation()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/resume_cycle_aggregation.md)
function).

To trigger historical data aggregation (i.e. before vaccinations began),
set `vaccination_data = NULL`.

Requires an 'options.txt' file to run - in a multi-site study this is
provided by the coordinating centre; in a single-site study you create
it yourself (see
[`vignette("options", "rapidcycler")`](https://global-vaccine-data-network.github.io/rapidcycler/articles/options.md)).

Validation of the input datasets is automatically run as part of this
function, and will throw an error if it finds any issues that require
fixing. If you would like to run validation separately, use the
[`validate_input_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/validate_input_data.md)
function.

## Usage

``` r
aggregate_data(
  person_data = NULL,
  vaccination_data,
  outcome_data,
  cycle_start_date,
  cycle_end_date,
  site_code,
  design_selection = NULL,
  options_file_location = getwd(),
  options = NULL,
  suppression_limit = NULL,
  split_size = 5e+05,
  stop_after_n_splits = NULL,
  restore_input_data = TRUE,
  working_directory = getwd(),
  input_data_in_chunks = FALSE,
  final_chunk = FALSE,
  skip_user_prompts = FALSE,
  output_format = "parquet",
  patient_data = NULL,
  participation_level = NULL
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

- site_code:

  Site code (e.g. AUS_MCRI), as a string

- design_selection:

  Character vector of analytical study designs to run. Valid values are:
  `"self_post"`, `"self_pre"`, `"historical"`, `"concurrent_vac"`,
  `"concurrent_unvac"`. Default is NULL, which runs all five designs.
  Descriptive outcomes and vaccination datasets are always produced for
  non-historical aggregations and cannot be excluded via this parameter.

- options_file_location:

  Location where options.txt file is stored. Supply either
  `options_file_location` or `options`, not both. Defaults to
  [`getwd()`](https://rdrr.io/r/base/getwd.html)

- options:

  Options object, already converted from txt file. Supply either
  options_file_location or options, not both

- suppression_limit:

  If required, low numbers can be suppressed. Numbers less than the
  limit will be suppressed, except 0

- split_size:

  Input data will be automatically split into pieces of this size
  (default 500,000). If memory is an issue, lower the split_size.
  Aggregation is done on the split data separately, and then combined
  once all splits are complete. Aggregation can be stopped and restarted
  from the last completed split (see
  [`resume_cycle_aggregation()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/resume_cycle_aggregation.md)
  function)

- stop_after_n_splits:

  Use this to stop aggregation after a given number of splits.
  Aggregation can then be resumed at a later date (see
  [`resume_cycle_aggregation()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/resume_cycle_aggregation.md)
  function)

- restore_input_data:

  If TRUE (default) the input data will be restored from the data folder
  to the global environment once the aggregation is complete, or
  stop_after_n_splits is reached. If FALSE, the input data will not be
  restored. WARNING: the data folder is removed once aggregation is
  complete (not if stopped using `stop_after_n_splits`)

- working_directory:

  Location of working directory. This is where the folder for this cycle
  will be created. Defaults to
  [`getwd()`](https://rdrr.io/r/base/getwd.html).

- input_data_in_chunks:

  Option to pre-split input data. Only use this if your data is too
  large to load into R in one go (otherwise just use split_size)

- final_chunk:

  If you have pre-split your input data, use this to indicate when you
  are running the last chunk of your input data. This will trigger the
  function to combine all the temporary results at the end

- skip_user_prompts:

  Boolean allowing user prompts to be skipped, mainly for the purpose of
  testing for which prompts are not possible. Recommended to leave as
  FALSE.

- output_format:

  Output file format: `"parquet"` (default, recommended for efficiency
  and cross-language compatibility) or `"csv"` (for legacy workflows).

- patient_data:

  Deprecated. Use `person_data` instead.

- participation_level:

  Deprecated. Use `design_selection` instead. Previously controlled
  which designs were available: 1 = minimal, 2 = partial, 3 = full.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate synthetic data and aggregate
data <- generate_synthetic_data(pop_size = 1000, save_data = FALSE)

aggregate_data(
  person_data = data$person_data,
  vaccination_data = data$vaccination_data,
  outcome_data = data$outcome_data,
  cycle_start_date = "2021-01-01",
  cycle_end_date = "2023-01-01",
  site_code = "TEST_SITE",
  options_file_location = "path/to/options",
  skip_user_prompts = TRUE
)
} # }
```
