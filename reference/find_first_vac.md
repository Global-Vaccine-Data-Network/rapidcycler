# Find first vaccination

Finds the date of first vaccination for each person, based on the data
provided.

## Usage

``` r
find_first_vac(data)
```

## Arguments

- data:

  A data.table of vaccination data, containing at least PID and V_DATE
  columns

## Value

A data.table of vaccination data, with additional column 'first_vac'
representing date of first vaccination for each person
