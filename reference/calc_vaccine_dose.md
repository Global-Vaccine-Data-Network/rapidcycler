# Calculate cumulative vaccine dose numbers

Calculate cumulative vaccine dose numbers

## Usage

``` r
calc_vaccine_dose(data, reference)
```

## Arguments

- data:

  A data.table of vaccination data, containing at least PID and V_DATE
  columns, and possibly V_SUBTYPE and V_TYPE depending on specified
  reference

- reference:

  Defines how to accumulate vaccine doses: by 'dose' (all doses
  counted), 'subtype' (accumulates by subtype), or 'type' (accumulates
  by type).

## Value

A data.table of vaccination data, with additional column 'V_DOSE'
representing cumulative vaccine dose for each person
