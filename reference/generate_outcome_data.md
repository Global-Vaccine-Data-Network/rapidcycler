# Generate synthetic AESI outcome data

Generates synthetic AESI outcome data for those people contained in
`person_data` with vaccinations given in `vaccination_data`. It uses the
vaccination data and specified `RR` to appropriately adjust outcome
rates during exposed periods.

For each person, the amount of time spent in risk (exposed) periods and
unexposed periods is calculated. Then, the number of AESI outcomes
experienced by each person is drawn from a poisson distribution, for
their total exposed and non-exposed periods independently, with the rate
parameter being scaled by `RR` for exposed cases. Finally, the cases
that occur are uniform randomly assigned a date during the relevant
period (exposed cases to exposed period of time, non-exposed cases to
non-exposed period of time), and encounter type is randomly assigned
from 1-4.

## Usage

``` r
generate_outcome_data(
  person_data,
  vaccination_data,
  study_start_date = as.Date("2021/01/01"),
  study_end_date = as.Date("2024/01/01"),
  outcomes = c("ADEM", "MYO", "ST"),
  outcome_daily_rates = rep(1e-06, length(outcomes)),
  RR = 5,
  risk_window = 42
)
```

## Arguments

- person_data:

  Person data input data.table (with date columns as Date objects, not
  8-digit number format)

- vaccination_data:

  Vaccination data input data.table (with date columns as Date objects,
  not 8-digit number format)

- study_start_date:

  Date object defining start of the study

- study_end_date:

  Date object defining end of the study

- outcomes:

  Character vector of options for AESI codes.

- outcome_daily_rates:

  Numeric vector of daily outcome rates, with same length and in same
  order as `outcomes` vector.

- RR:

  Numeric value or vector defining increased risk due to vaccination. If
  a vector, must be same length and order as `outcomes`.

- risk_window:

  Length of risk window post-vaccination. Currently only accepts a
  single number (same for all outcomes), and risk window is assumed to
  start on day of vaccination.

## Value

A data.table matching the AESI data template (BUT date columns are date
objects, not in 8-digit number format)
