# Suppress low counts

Suppress low counts

## Usage

``` r
suppress_low_counts(data, min_count)
```

## Arguments

- data:

  A data.table containing data to suppress counts in (will only suppress
  columns called "COUNT", "CASE_COUNT", "CONTROL_COUNT", or
  "PERSON_DAYS")

- min_count:

  Minimum count allowed - counts less than this will be suppressed,
  except 0 counts

## Value

A data.table containing data with low counts suppressed
