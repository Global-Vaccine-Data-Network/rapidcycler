
test_that("splitting time periods and aligning first period works", {

  expect_equal(split_time_periods(as.Date("2020-01-01"), as.Date("2020-12-31"), "year"),
               data.table(period_start = as.Date("2020-01-01"),
                          period_end = as.Date("2020-12-31")))

  expect_equal(split_time_periods(as.Date("2020-01-01"), as.Date("2020-06-30"), "year"),
               data.table(period_start = as.Date("2020-01-01"),
                          period_end = as.Date("2020-06-30")))

  expect_equal(split_time_periods(as.Date("2020-01-01"), as.Date("2020-03-31"), "month"),
               data.table(period_start = c(as.Date("2020-01-01"), as.Date("2020-02-01"), as.Date("2020-03-01")),
                          period_end = c(as.Date("2020-01-31"), as.Date("2020-02-29"), as.Date("2020-03-31"))))

  expect_equal(split_time_periods(as.Date("2020-01-15"), as.Date("2020-04-14"), "month"),
               data.table(period_start = c(as.Date("2020-01-15"), as.Date("2020-02-15"), as.Date("2020-03-15")),
                          period_end = c(as.Date("2020-02-14"), as.Date("2020-03-14"), as.Date("2020-04-14"))))

  expect_equal(split_time_periods(as.Date("2020-01-10"), as.Date("2020-04-14"), "month"),
               data.table(period_start = c(as.Date("2020-01-10"), as.Date("2020-02-10"), as.Date("2020-03-10"), as.Date("2020-04-10")),
                          period_end = c(as.Date("2020-02-09"), as.Date("2020-03-09"), as.Date("2020-04-09"), as.Date("2020-04-14"))))

  expect_equal(split_time_periods(as.Date("2020-01-15"), as.Date("2020-04-14"), "month", align_periods = TRUE),
               data.table(period_start = c(as.Date("2020-01-15"), as.Date("2020-02-01"), as.Date("2020-03-01"), as.Date("2020-04-01")),
                          period_end = c(as.Date("2020-01-31"), as.Date("2020-02-29"), as.Date("2020-03-31"), as.Date("2020-04-14"))))

  expect_equal(split_time_periods(as.Date("2020-01-10"), as.Date("2020-04-14"), "month", align_periods = TRUE),
               data.table(period_start = c(as.Date("2020-01-10"), as.Date("2020-02-01"), as.Date("2020-03-01"), as.Date("2020-04-01")),
                          period_end = c(as.Date("2020-01-31"), as.Date("2020-02-29"), as.Date("2020-03-31"), as.Date("2020-04-14"))))

  expect_error(split_time_periods(as.Date("2020-01-15"), as.Date("2020-04-14"), "year", align_periods = TRUE),
               "Time period alignment is currently only set up to work for monthly periods. Either change the period length, or opt not to align periods.")
})

test_that("suppression works", {

  input_data <- data.table(
    RATIO = c(1, 2, 3, 4, 5),
    COUNT = c(0, 1, 2, 3, 4),
    CASE_COUNT = c(0, 1, 2, 3, 4),
    CONTROL_COUNT = c(0, 1, 2, 3, 4),
    PERSON_DAYS = c(0, 2, 4, 6, 8)
  )

  output_data <- data.table(
    RATIO = c(1, 2, 3, 4, 5),
    COUNT = c(0, -1, -1, -1, 4),
    CASE_COUNT = c(0, -1, -1, -1, 4),
    CONTROL_COUNT = c(0, -1, -1, -1, 4),
    PERSON_DAYS = c(0, -1, 4, 6, 8)
  )

  expect_equal(suppress_low_counts(input_data, 4), output_data)

})

test_that("recombining empty temp data works", {

  data <- generate_synthetic_data(pop_size = 10000, save_data = FALSE)

  person_data <- data$person_data[(is.na(CENSOR_DATE) | CENSOR_DATE >= 20200101) &
                                      (is.na(ENROL_DATE) | ENROL_DATE <= 20200131)]
  vaccination_data <- data$vaccination_data[PID %in% person_data$PID]
  outcome_data <- data$outcome_data[PID %in% person_data$PID][, AESI := as.character(AESI)]

  options <- list(study_codename = "COVID",
                  period_length = "month",
                  align_periods = TRUE,
                  ratio_precision = 3,
                  included_analyses = c("primary", "subgroup_dose", "subgroup_platform", "subgroup_platform_dose", "subgroup_brand", "subgroup_brand_dose"),
                  age_groups = list(bounds = c(seq(from = 0, to = 80, by = 5), Inf),
                                    labels = c(paste(seq(0, 75, 5), seq(4, 79, 5), sep = "-"), "80+")),
                  outcome_info = data.table(AESI = c("ADEM", "MYO", "ST"),
                                            clean_window = c(0, 0, 0),
                                            risk_lower = c(0, 0, 0),
                                            risk_upper = c(2, 2, 2),
                                            washout_post = c(1, 1, 1),
                                            control_post_target = c(1, 1, 1),
                                            control_post_min = c(0, 0, 0),
                                            washout_pre = c(1, 1, 1),
                                            control_pre_target = c(1, 1, 1),
                                            control_pre_min = c(0, 0, 0)),
                  vaccine_info = data.table(V_SUBTYPE = c("ABC", "DEF", "GHI"),
                                            V_TYPE = c("PABC", "PDEF", "PGHI")))

  expect_no_warning(aggregate_data(person_data, vaccination_data, outcome_data,
                 cycle_start_date = as.Date('2020/01/01'),
                 cycle_end_date = as.Date('2020/01/31'),
                 site_code = "TEST_SITE",
                 options_file_location = NULL,
                 options = options,
                 skip_user_prompts = TRUE))

  unlink("RCA_COVID_TEST_SITE_20200101-20200131", recursive = TRUE)


})

test_that("case ascertainment works", {

})

test_that("participation_level deprecation warning is issued and produces correct design_selection", {

  data <- generate_synthetic_data(pop_size = 10000, save_data = FALSE)

  person_data <- data$person_data[(is.na(CENSOR_DATE) | CENSOR_DATE >= 20200101) &
                                      (is.na(ENROL_DATE) | ENROL_DATE <= 20200131)]
  vaccination_data <- data$vaccination_data[PID %in% person_data$PID]
  outcome_data <- data$outcome_data[PID %in% person_data$PID][, AESI := as.character(AESI)]

  options <- list(study_codename = "COVID",
                  period_length = "month",
                  align_periods = TRUE,
                  ratio_precision = 3,
                  included_analyses = c("primary", "subgroup_dose", "subgroup_platform", "subgroup_platform_dose", "subgroup_brand", "subgroup_brand_dose"),
                  age_groups = list(bounds = c(seq(from = 0, to = 80, by = 5), Inf),
                                    labels = c(paste(seq(0, 75, 5), seq(4, 79, 5), sep = "-"), "80+")),
                  outcome_info = data.table(AESI = c("ADEM", "MYO", "ST"),
                                            clean_window = c(0, 0, 0),
                                            risk_lower = c(0, 0, 0),
                                            risk_upper = c(2, 2, 2),
                                            washout_post = c(1, 1, 1),
                                            control_post_target = c(1, 1, 1),
                                            control_post_min = c(0, 0, 0),
                                            washout_pre = c(1, 1, 1),
                                            control_pre_target = c(1, 1, 1),
                                            control_pre_min = c(0, 0, 0)),
                  vaccine_info = data.table(V_SUBTYPE = c("ABC", "DEF", "GHI"),
                                            V_TYPE = c("PABC", "PDEF", "PGHI")))

  expect_warning(
    aggregate_data(person_data, vaccination_data, outcome_data,
                   cycle_start_date = as.Date('2020/01/01'),
                   cycle_end_date = as.Date('2020/01/31'),
                   site_code = "DEPTEST",
                   participation_level = 1,
                   options_file_location = NULL,
                   options = options,
                   skip_user_prompts = TRUE),
    regexp = "participation_level.*deprecated"
  )

  unlink("RCA_COVID_DEPTEST_20200101-20200131", recursive = TRUE)

})

test_that("invalid design_selection raises an error", {

  expect_error(
    aggregate_data(data.table(), data.table(), data.table(),
                   cycle_start_date = as.Date('2020/01/01'),
                   cycle_end_date = as.Date('2020/01/31'),
                   site_code = "TEST",
                   design_selection = c("self_post", "bad_design"),
                   options_file_location = NULL,
                   options = list(study_codename = "X"),
                   skip_user_prompts = TRUE),
    regexp = "Unknown design"
  )

})
