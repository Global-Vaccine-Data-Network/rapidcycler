# Generate synthetic person data

Generates a cohort of size `pop_size`, with dates of birth sampled
uniform randomly from between two specified dates (default is between
100 years before the study start and the study end). Age at death is
based on a beta distribution with specified parameters, which defines
date of death. Anyone who dies before the study start is removed, and
the whole process is repeated until `pop_size` is reached.

## Usage

``` r
generate_person_data(
  pop_size = 1e+06,
  study_start_date = as.Date("2021/01/01"),
  study_end_date = as.Date("2024/01/01"),
  dob_start_date = study_start_date - 100 * 365.25,
  dob_end_date = study_end_date,
  death_age_beta1 = 10,
  death_age_beta2 = 2,
  death_age_max = 100,
  sexes = c("m", "f")
)
```

## Arguments

- pop_size:

  Number of people in population

- study_start_date:

  Date object defining start of the study

- study_end_date:

  Date object defining end of the study

- dob_start_date:

  Date object defining earliest possible date of birth in cohort
  (defaults to 100 years before start of study)

- dob_end_date:

  Date object defining latest possible date of birth in cohort (defaults
  to study end date)

- death_age_beta1:

  Shape parameter 1 for beta distribution of age at death

- death_age_beta2:

  Shape parameter 2 for beta distribution of age at death

- death_age_max:

  Maximum age at death

- sexes:

  Options for biological sex at birth (drawn uniformly)

## Value

A data.table matching the person data template, with `pop_size` rows
(BUT date columns are date objects, not in 8-digit number format)
