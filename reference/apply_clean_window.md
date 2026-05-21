# Apply clean window rules

Given a clean window, only the first AESI outcome within that window
will be kept. This is to aid identification of incident outcomes.

## Usage

``` r
apply_clean_window(data, windows)
```

## Arguments

- data:

  A data.table of AESI outcome data on which to apply the clean window

- windows:

  A data.table of AESI outcome codes and their corresponding clean
  windows (columns named 'AESI' and 'clean_window')

## Value

A data.table of AESI outcome data with non-incident cases removed
