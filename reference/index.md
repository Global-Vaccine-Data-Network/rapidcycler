# Package index

## Primary functions

Exported functions available via
[`library(rapidcycler)`](https://global-vaccine-data-network.github.io/rapidcycler/)
or the `::` notation.

### Aggregate

Aggregate line-list data into a de-identified format for sharing.

- [`aggregate_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data.md)
  : Aggregate line list datasets
- [`resume_cycle_aggregation()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/resume_cycle_aggregation.md)
  : Resume data aggregation
- [`recombine_input_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/recombine_input_data.md)
  : Recombine input datasets

### Validate

Validate input datasets before aggregation.

- [`validate_input_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/validate_input_data.md)
  : Validate input datasets before aggregating

### Generate

Generate synthetic input datasets for testing and illustration.

- [`generate_synthetic_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/generate_synthetic_data.md)
  : Generate synthetic input datasets

## Internal functions

Internal helper functions used by the primary functions, accessible via
`:::`. **It is not necessary to manually call any of these functions.**

### Aggregate

- [`aggregate_data_concurrent()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data_concurrent.md)
  : Aggregate data with concurrent comparators
- [`aggregate_data_descriptive_outcomes()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data_descriptive_outcomes.md)
  : Aggregate AESI outcomes
- [`aggregate_data_descriptive_vaccinations()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data_descriptive_vaccinations.md)
  : Aggregate vaccinations
- [`aggregate_data_exposed()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data_exposed.md)
  : Aggregate post-vaccination exposure data
- [`aggregate_data_historic()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data_historic.md)
  : Aggregate historic data
- [`aggregate_data_self()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data_self.md)
  : Aggregate data with self controls
- [`concurrent_final_aggregation()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/concurrent_final_aggregation.md)
  : Complete aggregation of data with concurrent comparators

### Validate

- [`validate_column_names()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/validate_column_names.md)
  : Validate names of input data columns
- [`validate_column_types()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/validate_column_types.md)
  : Validate names of input data columns
- [`validate_outcome_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/validate_outcome_data.md)
  : Validate AESI outcome data
- [`validate_outcome_dates()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/validate_outcome_dates.md)
  : Validate outcome dates
- [`validate_person_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/validate_person_data.md)
  : Validate person data
- [`validate_vaccination_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/validate_vaccination_data.md)
  : Validate vaccination data
- [`validate_vaccination_dates()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/validate_vaccination_dates.md)
  : Validate vaccination dates

### Generate

- [`generate_outcome_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/generate_outcome_data.md)
  : Generate synthetic AESI outcome data
- [`generate_person_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/generate_person_data.md)
  : Generate synthetic person data
- [`generate_vaccination_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/generate_vaccination_data.md)
  : Generate synthetic vaccination data

### Vaccination formatting

- [`calc_vaccine_dose()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/calc_vaccine_dose.md)
  : Calculate cumulative vaccine dose numbers
- [`map_subtypes_to_type()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/map_subtypes_to_type.md)
  : Map vaccine subtypes to types
- [`find_first_vac()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/find_first_vac.md)
  : Find first vaccination

### Date formatting

- [`check_date_format()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/check_date_format.md)
  : Check format of date
- [`convert_date_to_number()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/convert_date_to_number.md)
  : Convert date to number
- [`convert_number_to_date()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/convert_number_to_date.md)
  : Convert number to date

### Time periods

- [`split_time_periods()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/split_time_periods.md)
  : Split time periods

### Clean window

- [`apply_clean_window()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/apply_clean_window.md)
  : Apply clean window rules

### Options and notes files

- [`read_options_file()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/read_options_file.md)
  : Read and convert options file
- [`check_options_object()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/check_options_object.md)
  : Check options object
- [`extract_and_split_line()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/extract_and_split_line.md)
  : Extract data from line of text
- [`create_notes_file()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/create_notes_file.md)
  : Create notes file

### Low count suppression

- [`suppress_low_counts()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/suppress_low_counts.md)
  : Suppress low counts

### Other

- [`combine_temp_results()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/combine_temp_results.md)
  : Combine temporary results
- [`get_descriptives()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/get_descriptives.md)
  : Calculate descriptive statistics
- [`load_combine_summarise()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/load_combine_summarise.md)
  : Load, combine, and summarise temp data
