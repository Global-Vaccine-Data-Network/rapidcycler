# Resume data aggregation

If aggregation for a particular cycle has been started but not
completed, this function can be used to easily resume aggregation from
where it was stopped.

## Usage

``` r
resume_cycle_aggregation(
  folder_name,
  working_directory = getwd(),
  stop_after_n_splits = NULL,
  input_data_in_chunks = FALSE,
  final_chunk = FALSE
)
```

## Arguments

- folder_name:

  Name of main results folder for this cycle (not including path)

- working_directory:

  Location of the main results folder for this cycle (path only),
  defaults to [`getwd()`](https://rdrr.io/r/base/getwd.html).

- stop_after_n_splits:

  Use this to stop aggregation after a given number of splits.
  Aggregation can then be resumed at a later date (again).

- input_data_in_chunks:

  Option to pre-split input data. Only use this if your data is too
  large to load into R in one go (otherwise just use split_size). This
  should match what was used for the initial call of
  [`aggregate_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data.md).

- final_chunk:

  If you have pre-split your input data, use this to indicate when you
  are running the last chunk of your input data. This will trigger the
  function to combine all the temporary results at the end. This should
  match what was used for the initial call of
  [`aggregate_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data.md).

## Examples

``` r
if (FALSE) { # \dontrun{
# Resume a previously interrupted aggregation
resume_cycle_aggregation(
  folder_name = "RCA_COVID_TEST_SITE_20220101-20220131",
  working_directory = "~/Documents/RCA_COVID/"
)
} # }
```
