# Combine temporary results

Aggregation is not performed all in one go, but in sections, to manage
memory use and allow stopping/restarting. The results are saved as they
are completed, in a 'temp' folder. Once all the sections have been
aggregated, the results are combined using this function, saved in the
main results folder, and the 'temp' folder and its contents are removed.

## Usage

``` r
combine_temp_results(
  design_selection,
  results_file_path,
  options,
  historical_aggregation,
  output_format = "parquet"
)
```

## Arguments

- design_selection:

  Character vector of specific study designs to run

- results_file_path:

  File path to results folder (the main folder created for this cycle)

- options:

  Options object, already converted from txt file.

- historical_aggregation:

  Boolean stating whether or not these results are for historical
  aggregation, so that it knows what files to expect.

- output_format:

  Output file format: `"parquet"` or `"csv"`.
