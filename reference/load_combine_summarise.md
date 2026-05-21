# Load, combine, and summarise temp data

A helper function to load, combine, and summarise temp data, called from
`combine_temp_results` function. Incorporates a catch so that empty data
does not cause an error.

## Usage

``` r
load_combine_summarise(
  file_name_pattern,
  file_path,
  summarise_value,
  summarise_columns
)
```

## Arguments

- file_name_pattern:

  Pattern to look for in file names. Files with matching pattern will be
  loaded and combined

- file_path:

  Path to where to look for files

- summarise_value:

  Name(s) of summary column(s), e.g. COUNT

- summarise_columns:

  Names of columns to summarise by

## Value

Combined data table
