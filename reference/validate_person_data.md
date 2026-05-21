# Validate person data

Validate person data

## Usage

``` r
validate_person_data(person_data, cycle_start_date, cycle_end_date)
```

## Arguments

- person_data:

  Person data input data.table

- cycle_start_date:

  Start date of cycle, as a date object, an 8 digit number of the form
  yyyymmdd, or a string of the form yyyy-mm-dd

- cycle_end_date:

  End date of cycle, as a date object, an 8 digit number of the form
  yyyymmdd, or a string of the form yyyy-mm-dd

## Value

A character vector of issues found. If no issues are found, the vector
will be empty.
