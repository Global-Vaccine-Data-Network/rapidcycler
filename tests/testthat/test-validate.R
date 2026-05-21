cycle_start_date = as.Date('2021/01/01')
cycle_end_date = as.Date('2024/01/01')

data <- generate_synthetic_data(pop_size = 100000, save_location = "./fixtures")

person_data <- fread("fixtures/person_data.csv")
vaccination_data <- fread("fixtures/vaccination_data.csv")
outcome_data <- fread("fixtures/outcome_data.csv")

test_that("validation of historical data works", {

  issues <- validate_input_data(person_data, vaccination_data = NULL, outcome_data,
                      cycle_start_date, cycle_end_date,
                      options_file_location = "fixtures")

  expect_equal(issues, c())
})

test_that("validation of all data works", {

  issues <- validate_input_data(person_data, vaccination_data, outcome_data,
                                cycle_start_date, cycle_end_date,
                                options_file_location = "fixtures")

  expect_equal(issues, c())
})

test_that("configurable lookback_length triggers validation at 0.75 of the specified length", {

  options_fixture <- read_options_file("fixtures")

  # Filter outcome data to only include outcomes from the 6 months before first vaccination.
  # This gives a maximum gap of ~6 months between first vaccination and earliest outcome.
  first_vac_date_int <- min(vaccination_data$V_DATE)
  first_vac_date <- as.Date(as.character(first_vac_date_int), "%Y%m%d")
  cutoff_date_int <- as.integer(format(first_vac_date - 182, "%Y%m%d"))
  outcome_recent <- outcome_data[EVENT_DATE >= cutoff_date_int]

  # Trigger case: lookback_length = 3 years → threshold = 2.25 years.
  # Earliest outcome is only ~6 months before vaccination → 6 months < 2.25 years → triggers.
  options_long <- options_fixture
  options_long$lookback_length <- 3

  expect_warning(
    issues_trigger <- validate_input_data(
      person_data, vaccination_data, outcome_recent,
      cycle_start_date, cycle_end_date,
      options_file_location = NULL,
      options = options_long,
      skip_user_prompts = TRUE
    ),
    regexp = "Lookback threshold not met"
  )
  expect_true(any(grepl("Lookback warning \\(automated run\\)", issues_trigger)))

  # No-trigger case: lookback_length = 0.5 years → threshold = 0.375 years ≈ 4.5 months.
  # Earliest outcome is ~6 months before vaccination → 6 months > 4.5 months → no trigger.
  options_short <- options_fixture
  options_short$lookback_length <- 0.5

  issues_ok <- validate_input_data(
    person_data, vaccination_data, outcome_recent,
    cycle_start_date, cycle_end_date,
    options_file_location = NULL,
    options = options_short,
    skip_user_prompts = TRUE
  )
  expect_false(any(grepl("lookback", issues_ok)))

})

file.remove("fixtures/person_data.csv")
file.remove("fixtures/vaccination_data.csv")
file.remove("fixtures/outcome_data.csv")
