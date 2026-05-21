# Split time periods

Given a period of time, it will split that interval into sub-periods
based on the given length.

## Usage

``` r
split_time_periods(start, end, period_length, align_periods = FALSE)
```

## Arguments

- start:

  Start of time interval to split, as a date object

- end:

  End of time interval to split, as a date object

- period_length:

  Character representing desired length of sub-periods, e.g. 'month',
  'day'

- align_periods:

  Boolean whether or not to adjust time periods so that they will align,
  even if start dates differ. Currently only set up to handle monthly
  periods, but may be generalised in the future.

## Value

A data.table containing start and end dates of sub-periods
