# Generate synthetic vaccination data

Generates vaccination data for those people contained in `person_data`.
The number of vaccinations each person receives is drawn from a poisson
distribution with specified mean (can be 0). After the number of
vaccinations is drawn, vaccination dates and brands are uniform randomly
sampled.

## Usage

``` r
generate_vaccination_data(
  person_data,
  campaign_start_date = as.Date("2021/01/01"),
  campaign_end_date = as.Date("2024/01/01"),
  vaccine_codes = c("ABC", "DEF", "GHI"),
  mean_doses = 3
)
```

## Arguments

- person_data:

  Person data input data.table (with date columns as Date objects, not
  8-digit number format)

- campaign_start_date:

  Date object defining start of vaccination campaign (generally same as
  study start date)

- campaign_end_date:

  Date object defining end of vaccination campaign

- vaccine_codes:

  Character vector of options for vaccine brand codes

- mean_doses:

  Mean number of doses per person during the campaign, as parameter for
  Poisson distribution

## Value

A data.table matching the vaccination data template (BUT date columns
are date objects, not in 8-digit number format)
