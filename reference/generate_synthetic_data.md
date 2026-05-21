# Generate synthetic input datasets

This function is a simplified wrapper around three more complex
functions which generate synthetic person, vaccination, and AESI outcome
data (see
[`generate_person_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/generate_person_data.md),
[`generate_vaccination_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/generate_vaccination_data.md),
and
[`generate_outcome_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/generate_outcome_data.md)
respectively). Importantly, it also converts the date columns of all
three datasets from Date objects to the 8-digit number format required.

For greater control over the datasets produced, use the 3 specific
functions listed above.

## Usage

``` r
generate_synthetic_data(
  pop_size = 1e+06,
  study_start_date = as.Date("2021/01/01"),
  study_end_date = as.Date("2024/01/01"),
  save_data = TRUE,
  save_location = getwd()
)
```

## Arguments

- pop_size:

  Number of people in population (defines size of person_data)

- study_start_date:

  Date object defining start of vaccination campaign and study

- study_end_date:

  Date object defining end of study

- save_data:

  Boolean whether or not to save the data, as well as return it (default
  is TRUE, will save)

- save_location:

  Location where to save the data, if `save_data = TRUE`. Defaults to
  current working directory

## Value

Named list containing 3 datasets: "person_data", "vaccination_data", and
"outcome_data", matching their respective templates.

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate synthetic data for 1000 people (without saving to disk)
data <- generate_synthetic_data(
  pop_size = 1000,
  study_start_date = as.Date("2021-01-01"),
  study_end_date = as.Date("2023-01-01"),
  save_data = FALSE
)

head(data$person_data)
head(data$vaccination_data)
head(data$outcome_data)
} # }
```
