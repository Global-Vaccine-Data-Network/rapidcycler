# Calculate descriptive statistics

Calculate descriptive statistics

## Usage

``` r
get_descriptives(
  person_data,
  vaccination_data,
  outcome_data,
  age_reference_date
)
```

## Arguments

- person_data:

  Person data input data.table. If NULL, descriptives for this dataset
  will be skipped.

- vaccination_data:

  Vaccination data input data.table. If NULL, descriptives for this
  dataset will be skipped.

- outcome_data:

  AESI outcome data input data.table. If NULL, descriptives for this
  dataset will be skipped.

- age_reference_date:

  Reference date to calculate age from (generally cycle start)

## Value

Character vector of descriptive statistics
