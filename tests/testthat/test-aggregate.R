
period_start <- as.Date("2021-01-01")
period_end <- period_start + 15

person_data <- data.table(
  PID = 1:20,
  DOB = seq(from = as.Date("1940-01-01"), to = as.Date("2020-01-01"), length.out = 20),
  SEX = rep(c("M", "F"), 10),
  ENROL_DATE = seq(from = as.Date("1940-01-01"), to = as.Date("2020-01-01"), length.out = 20),
  CENSOR_DATE = NA_integer_,
  CENSOR_TYPE = NA_character_
)

vaccination_data <- data.table(
  PID = c(1:16, 2:6, 6, 7, 8, 9, 9, 10, 11, 11, 12, 12, 13, 14, 15, 15, 16, 16),
  V_DATE = c(rep(period_start, 16),
             period_start + 8,
             period_start + 5,
             period_start + 4,
             period_start + 2,
             period_start + 2, period_start + 8,
             period_start + 8,
             period_start + 2,
             period_start + 2, period_start + 7,
             period_start + 4,
             period_start + 4, period_start + 6,
             period_start + 4, period_start + 9,
             period_start + 9,
             period_start + 6,
             period_start + 6, period_start + 8,
             period_start + 3, period_start + 5),
  V_SUBTYPE = c(rep("abc", 16),
              "abc",
              "abc",
              "abc",
              "abc",
              "abc", "abc",
              "def",
              "def",
              "def", "ghi",
              "def",
              "def", "abc",
              "def", "abc",
              "def",
              "def",
              "def", "abc",
              "def", "abc"),
  V_DOSE = -1
)

options <- list(study_codename = "COVID",
                period_length = "month",
                align_periods = TRUE,
                ratio_precision = 3,
                included_analyses = c("primary", "subgroup_dose", "subgroup_platform", "subgroup_platform_dose", "subgroup_brand", "subgroup_brand_dose"),
                age_groups = list(bounds = c(seq(from = 0, to = 80, by = 5), Inf),
                                  labels = c(paste(seq(0, 75, 5), seq(4, 79, 5), sep = "-"), "80+")),
                outcome_info = data.table(AESI = c("ADEM", "Myo", "Stroke"),
                                          clean_window = c(0, 0, 0),
                                          risk_lower = c(0, 0, 0),
                                          risk_upper = c(2, 2, 2),
                                          washout_post = c(1, 1, 1),
                                          control_post_target = c(1, 1, 1),
                                          control_post_min = c(0, 0, 0),
                                          washout_pre = c(1, 1, 1),
                                          control_pre_target = c(1, 1, 1),
                                          control_pre_min = c(0, 0, 0)),
                vaccine_info = data.table(V_SUBTYPE = c("abc", "def", "ghi"),
                                          V_TYPE = c("PABC", "PDEF", "PGHI")))

test_that("full period aggregation with self-controlled (post) study design works", {

  #tests 1-8
  outcome_days <- c(-2, 0, 2, 3, 4, 6, 8, 10)

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "WINDOW_RATIO", "CASE_COUNT", "CONTROL_COUNT")

  fixed_data <- data.table(PERIOD_START = period_start, PERIOD_END = period_end,
                           ENCOUNTER_TYPE = 1,
                           AESI = "ADEM",
                           AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                                           labels = options$age_groups$labels, right = FALSE),
                           SEX = "M", V_DOSE = -1)

  expected_outputs <- list(
    data.table(),
    data.table(V_SUBTYPE = "abc",
               V_TYPE = "PABC",
               WINDOW_RATIO = c(1, 3, 5/2, 2, 3/2),
               CASE_COUNT = c(10, 2, 1, 1, 2),
               CONTROL_COUNT = 0
    ),
    data.table(V_SUBTYPE = c(rep("abc", 4), rep("def", 2)),
               V_TYPE = c(rep("PABC", 4), rep("PDEF", 2)),
               WINDOW_RATIO = c(1, 3, 5/2, 3/2, 1, 3),
               CASE_COUNT = c(9, 2, 1, 2, 1, 1),
               CONTROL_COUNT = 0
    ),
    data.table(V_SUBTYPE = c(rep("abc", 2), rep("def", 2)),
               V_TYPE = c(rep("PABC", 2), rep("PDEF", 2)),
               WINDOW_RATIO = c(1, 5/2, 1, 3),
               CASE_COUNT = c(1, 1, 2, 1),
               CONTROL_COUNT = 0
    ),
    data.table(V_SUBTYPE = c(rep("abc", 4), rep("def", 2)),
               V_TYPE = c(rep("PABC", 4), rep("PDEF", 2)),
               WINDOW_RATIO = c(1, 3, 5/2, 3/2, 1, 3),
               CASE_COUNT = c(2, 0, 1, 0, 4, 2),
               CONTROL_COUNT = c(4, 1, 0, 2, 0, 0)
    ),
    data.table(V_SUBTYPE = c(rep("abc", 3), rep("def", 2)),
               V_TYPE = c(rep("PABC", 3), rep("PDEF", 2)),
               WINDOW_RATIO = c(1, 2, 5/2, 1, 3),
               CASE_COUNT = c(4, 0, 0, 3, 1),
               CONTROL_COUNT = c(6, 1, 1, 1, 1)
    ),
    data.table(V_SUBTYPE = c(rep("abc", 2), rep("def", 2), "ghi"),
               V_TYPE = c(rep("PABC", 2), rep("PDEF", 2), "PGHI"),
               WINDOW_RATIO = c(1, 3, 1, 3, 1),
               CASE_COUNT = c(4, 0, 2, 0, 1),
               CONTROL_COUNT = c(3, 1, 2, 1, 0)
    ),
    data.table(V_SUBTYPE = c(rep("abc", 1), rep("def", 1)),
               V_TYPE = c(rep("PABC", 1), rep("PDEF", 1)),
               WINDOW_RATIO = c(1, 1),
               CASE_COUNT = c(4, 2),
               CONTROL_COUNT = c(6, 4)
    )
  )

  for (i in 1:length(outcome_days)) {
    outcome_data <- data.table(
      PID = 1:20,
      AESI = "ADEM",
      EVENT_DATE = period_start + outcome_days[i],
      ENCOUNTER_TYPE = 1
    )
    output <- aggregate_data_self(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                  vaccination_data, outcome_data,
                                  period_start, period_end, options, comparator = "post")

    if (nrow(output) > 0) {
      setorderv(output, col_order)
      setcolorder(output, col_order)
    }
    if (nrow(expected_outputs[[i]] > 0)) {
      expected_output <- dplyr::bind_cols(
        expected_outputs[[i]],
        fixed_data
      )
      setDT(expected_output)
      setorderv(expected_output, col_order)
      setcolorder(expected_output, col_order)
    } else {
      expected_output <- expected_outputs[[i]]
    }

    expect_equal(output, expected_output)
  }

})

test_that("full period aggregation with self-controlled (pre) study design works", {

  # tests 9-17
  outcome_days <- c(-5, -3, -1, 0, 2, 4, 6, 8, 13)

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "WINDOW_RATIO", "CASE_COUNT", "CONTROL_COUNT")

  fixed_data <- data.table(PERIOD_START = period_start, PERIOD_END = period_end,
                           ENCOUNTER_TYPE = 1,
                           AESI = "ADEM",
                           AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                                           labels = options$age_groups$labels, right = FALSE),
                           SEX = "M", V_DOSE = -1)

  expected_outputs <- list(
    data.table(V_SUBTYPE = "abc",
               V_TYPE = "PABC",
               WINDOW_RATIO = c(1),
               CASE_COUNT = c(0),
               CONTROL_COUNT = c(3)
    ),
    data.table(V_SUBTYPE = c("abc", "def", "ghi"),
               V_TYPE = c("PABC", "PDEF", "PGHI"),
               WINDOW_RATIO = c(1, 1, 1),
               CASE_COUNT = 0,
               CONTROL_COUNT = c(16, 10, 1)
    ),
    data.table(),
    data.table(V_SUBTYPE = c(rep("abc", 1)),
               V_TYPE = c(rep("PABC", 1)),
               WINDOW_RATIO = c(1),
               CASE_COUNT = c(16),
               CONTROL_COUNT = c(0)
    ),
    data.table(V_SUBTYPE = c(rep("abc", 1), rep("def", 1)),
               V_TYPE = c(rep("PABC", 1), rep("PDEF", 1)),
               WINDOW_RATIO = c(1, 1),
               CASE_COUNT = c(14, 2),
               CONTROL_COUNT = c(0, 0)
    ),
    data.table(V_SUBTYPE = c(rep("abc", 2), rep("def", 1)),
               V_TYPE = c(rep("PABC", 2), rep("PDEF", 1)),
               WINDOW_RATIO = c(1, 3, 1),
               CASE_COUNT = c(2, 0, 6),
               CONTROL_COUNT = c(1, 1, 0)
    ),
    data.table(V_SUBTYPE = c(rep("abc", 2), rep("def", 1)),
               V_TYPE = c(rep("PABC", 2), rep("PDEF", 1)),
               WINDOW_RATIO = c(1, 3, 1),
               CASE_COUNT = c(1, 0, 4),
               CONTROL_COUNT = c(1, 1, 0)
    ),
    data.table(V_SUBTYPE = c(rep("abc", 2), rep("def", 1), "ghi"),
               V_TYPE = c(rep("PABC", 2), rep("PDEF", 1), "PGHI"),
               WINDOW_RATIO = c(1, 3, 1, 1),
               CASE_COUNT = c(1, 2, 2, 1),
               CONTROL_COUNT = c(0, 0, 0, 0)
    ),
    data.table()
  )

  for (i in 1:length(outcome_days)) {
    outcome_data <- data.table(
      PID = 1:20,
      AESI = "ADEM",
      EVENT_DATE = period_start + outcome_days[i],
      ENCOUNTER_TYPE = 1
    )
    output <- aggregate_data_self(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                  vaccination_data, outcome_data,
                                  period_start, period_end, options, comparator = "pre")

    if (nrow(output) > 0) {
      setorderv(output, col_order)
      setcolorder(output, col_order)
    }
    if (nrow(expected_outputs[[i]] > 0)) {
      expected_output <- dplyr::bind_cols(
        expected_outputs[[i]],
        fixed_data
      )
      setDT(expected_output)
      setorderv(expected_output, col_order)
      setcolorder(expected_output, col_order)
    } else {
      expected_output <- expected_outputs[[i]]
    }

    expect_equal(output, expected_output)
  }

})

test_that("full period aggregation with concurrent (vaccinated) study design works", {

  #tests 18-24
  outcome_days <- c(-1, 0, 3, 4, 6, 8, 13)

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "RATIO", "CASE_COUNT", "CONTROL_COUNT")

  fixed_data <- data.table(PERIOD_START = period_start-2, PERIOD_END = period_end,
                           ENCOUNTER_TYPE = 1,
                           AESI = "ADEM",
                           AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                                           labels = options$age_groups$labels, right = FALSE),
                           SEX = "M", V_DOSE = -1)

  expected_outputs <- list(
    data.table(),
    data.table(),
    data.table(),
    data.table(V_SUBTYPE = c("abc", "def"),
               V_TYPE = c("PABC", "PDEF"),
               RATIO = round(c(3/7, 6/7), options$ratio_precision),
               CASE_COUNT = c(3, 6),
               CONTROL_COUNT = c(7, 7)),
    data.table(V_SUBTYPE = c("abc", "def"),
               V_TYPE = c("PABC", "PDEF"),
               RATIO = round(c(4/8, 4/8), options$ratio_precision),
               CASE_COUNT = c(4, 4),
               CONTROL_COUNT = c(8, 8)),
    data.table(V_SUBTYPE = c("abc", "def", "ghi"),
               V_TYPE = c("PABC", "PDEF", "PGHI"),
               RATIO = round(c(4/7, 2/7, 1/7), options$ratio_precision),
               CASE_COUNT = c(4, 2, 1),
               CONTROL_COUNT = c(7, 7, 7)),
    data.table()
  )

  for (i in 1:length(outcome_days)) {
    outcome_data <- data.table(
      PID = 1:20,
      AESI = "ADEM",
      EVENT_DATE = period_start + outcome_days[i],
      ENCOUNTER_TYPE = 1
    )
    output <- aggregate_data_concurrent(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                        vaccination_data, outcome_data,
                                        period_start - 2, period_end,
                                        options, comparator = "vaccinated")

    if (nrow(output) > 0) {
      setorderv(output, col_order)
      setcolorder(output, col_order)
    }
    if (nrow(expected_outputs[[i]] > 0)) {
      expected_output <- dplyr::bind_cols(
        expected_outputs[[i]],
        fixed_data
      )
      setDT(expected_output)
      setorderv(expected_output, col_order)
      setcolorder(expected_output, col_order)

      expect_equal(output, expected_output)
    } else {
      expect_equal(nrow(output), 0)
    }


  }

})

test_that("full period aggregation with concurrent (unvaccinated) study design works", {

  v_date_adjustments <- c(0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 5, 5, 4, 4, 4,
                          3, 3, 2, 2, 2, 3, 3, 3, 4, 4, 9, 9, 11, 11, 11, 13, 13, 13)

  #tests 25-33
  outcome_days <- c(-1, 0, 2, 4, 6, 8, 10, 12, 13)

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "RATIO", "CASE_COUNT", "CONTROL_COUNT")

  fixed_data <- data.table(PERIOD_START = period_start-1, PERIOD_END = period_end,
                           ENCOUNTER_TYPE = 1,
                           AESI = "ADEM",
                           AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                                           labels = options$age_groups$labels, right = FALSE),
                           SEX = "M", V_DOSE = -1)

  expected_outputs <- list(
    data.table(),
    data.table(V_SUBTYPE = c("abc"),
               V_TYPE = c("PABC"),
               RATIO = round(c(1/19), options$ratio_precision),
               CASE_COUNT = c(1),
               CONTROL_COUNT = c(19)),
    data.table(V_SUBTYPE = c("abc"),
               V_TYPE = c("PABC"),
               RATIO = round(c(4/16), options$ratio_precision),
               CASE_COUNT = c(4),
               CONTROL_COUNT = c(16)),
    data.table(V_SUBTYPE = c("abc"),
               V_TYPE = c("PABC"),
               RATIO = round(c(8/10), options$ratio_precision),
               CASE_COUNT = c(8),
               CONTROL_COUNT = c(10)),
    data.table(V_SUBTYPE = c("abc", "def"),
               V_TYPE = c("PABC", "PDEF"),
               RATIO = round(c(5/7, 2/7), options$ratio_precision),
               CASE_COUNT = c(5, 2),
               CONTROL_COUNT = c(7, 7)),
    data.table(V_SUBTYPE = c("abc", "def"),
               V_TYPE = c("PABC", "PDEF"),
               RATIO = round(c(6/7, 4/7), options$ratio_precision),
               CASE_COUNT = c(6, 4),
               CONTROL_COUNT = c(7, 7)),
    data.table(V_SUBTYPE = c("abc"),
               V_TYPE = c("PABC"),
               RATIO = round(c(3/6), options$ratio_precision),
               CASE_COUNT = c(3),
               CONTROL_COUNT = c(6)),
    data.table(V_SUBTYPE = c("abc", "ghi"),
               V_TYPE = c("PABC", "PGHI"),
               RATIO = round(c(2/5, 1/5), options$ratio_precision),
               CASE_COUNT = c(2, 1),
               CONTROL_COUNT = c(5, 5)),
    data.table(V_SUBTYPE = c("abc", "def", "ghi"),
               V_TYPE = c("PABC", "PDEF", "PGHI"),
               RATIO = round(c(4/4, 1/4, 1/4), options$ratio_precision),
               CASE_COUNT = c(4, 1, 1),
               CONTROL_COUNT = c(4, 4, 4))
  )

  for (i in 1:length(outcome_days)) {
    outcome_data <- data.table(
      PID = 1:20,
      AESI = "ADEM",
      EVENT_DATE = period_start + outcome_days[i],
      ENCOUNTER_TYPE = 1
    )
    output <- aggregate_data_concurrent(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                        vaccination_data %>% dplyr::arrange(PID, V_DATE) %>% dplyr::mutate(V_DATE = V_DATE + v_date_adjustments),
                                        outcome_data,
                                        period_start - 1, period_end,
                                        options, comparator = "unvaccinated")

    if (nrow(output) > 0) {
      setorderv(output, col_order)
      setcolorder(output, col_order)
    }
    if (nrow(expected_outputs[[i]] > 0)) {
      expected_output <- dplyr::bind_cols(
        expected_outputs[[i]],
        fixed_data
      )
      setDT(expected_output)
      setorderv(expected_output, col_order)
      setcolorder(expected_output, col_order)

      expect_equal(output, expected_output)
    } else {
      expect_equal(nrow(output), 0)
    }


  }

})

test_that("full period aggregation of exposed cases works", {

  #tests 34-41
  outcome_days <- c(-1, 0, 2, 4, 6, 8, 10, 12)

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "CASE_COUNT")

  fixed_data <- data.table(PERIOD_START = period_start - 1, PERIOD_END = period_end,
                           ENCOUNTER_TYPE = 1,
                           AESI = "ADEM",
                           AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                                           labels = options$age_groups$labels, right = FALSE),
                           SEX = "M", V_DOSE = -1)

  expected_outputs <- list(
    data.table(),
    data.table(V_SUBTYPE = c("abc"),
               V_TYPE = c("PABC"),
               CASE_COUNT = c(16)),
    data.table(V_SUBTYPE = c("abc", "def"),
               V_TYPE = c("PABC", "PDEF"),
               CASE_COUNT = c(14, 2)),
    data.table(V_SUBTYPE = c("abc", "def"),
               V_TYPE = c("PABC", "PDEF"),
               CASE_COUNT = c(3, 6)),
    data.table(V_SUBTYPE = c("abc", "def"),
               V_TYPE = c("PABC", "PDEF"),
               CASE_COUNT = c(4, 4)),
    data.table(V_SUBTYPE = c("abc", "def", "ghi"),
               V_TYPE = c("PABC", "PDEF", "PGHI"),
               CASE_COUNT = c(4, 2, 1)),
    data.table(V_SUBTYPE = c("abc", "def"),
               V_TYPE = c("PABC", "PDEF"),
               CASE_COUNT = c(4, 2)),
    data.table()
  )

  for (i in 1:length(outcome_days)) {
    outcome_data <- data.table(
      PID = 1:20,
      AESI = "ADEM",
      EVENT_DATE = period_start + outcome_days[i],
      ENCOUNTER_TYPE = 1
    )
    output <- aggregate_data_exposed(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                     vaccination_data, outcome_data,
                                     period_start = period_start - 1, period_end = period_end,
                                     options)$cases

    if (nrow(output) > 0) {
      setorderv(output, col_order)
      setcolorder(output, col_order)
    }
    if (nrow(expected_outputs[[i]] > 0)) {
      expected_output <- dplyr::bind_cols(
        expected_outputs[[i]],
        fixed_data
      )
      setDT(expected_output)
      setorderv(expected_output, col_order)
      setcolorder(expected_output, col_order)

      expect_equal(output, expected_output)
    } else {
      expect_equal(nrow(output), 0)
    }
  }

})

test_that("full period aggregation of exposed person days works", {

  #tests 42-46
  outcome_days <- c(-1, 0, 2, 8, 12)

  col_order <- c("PERIOD_START", "PERIOD_END", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "PERSON_DAYS")

  fixed_data <- data.table(PERIOD_START = period_start - 1, PERIOD_END = period_end,
                           AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                                           labels = options$age_groups$labels, right = FALSE),
                           SEX = "M", V_DOSE = -1)

  expected_outputs <- list(
    data.table(AESI = c(rep("ADEM", 3), rep("Myo", 3), rep("Stroke", 3)),
               V_SUBTYPE = c("abc", "def", "ghi"),
               V_TYPE = c("PABC", "PDEF", "PGHI"),
               PERSON_DAYS = c(74, 27, 3)),
    data.table(AESI = c(rep("ADEM", 3), rep("Myo", 3), rep("Stroke", 3)),
               V_SUBTYPE = c("abc", "def", "ghi"),
               V_TYPE = c("PABC", "PDEF", "PGHI"),
               PERSON_DAYS = c(74, 27, 3)),
    data.table(AESI = c(rep("ADEM", 3), rep("Myo", 3), rep("Stroke", 3)),
               V_SUBTYPE = c("abc", "def", "ghi"),
               V_TYPE = c("PABC", "PDEF", "PGHI"),
               PERSON_DAYS = c(74, 27, 3)),
    data.table(AESI = c(rep("ADEM", 3), rep("Myo", 3), rep("Stroke", 3)),
               V_SUBTYPE = c("abc", "def", "ghi"),
               V_TYPE = c("PABC", "PDEF", "PGHI"),
               PERSON_DAYS = c(74, 27, 3)),
    data.table(AESI = c(rep("ADEM", 3), rep("Myo", 3), rep("Stroke", 3)),
               V_SUBTYPE = c("abc", "def", "ghi"),
               V_TYPE = c("PABC", "PDEF", "PGHI"),
               PERSON_DAYS = c(74, 27, 3))
  )

  for (i in 1:length(outcome_days)) {
    outcome_data <- data.table(
      PID = 1:20,
      AESI = "ADEM",
      EVENT_DATE = period_start + outcome_days[i],
      ENCOUNTER_TYPE = 1
    )
    output <- aggregate_data_exposed(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                     vaccination_data, outcome_data,
                                     period_start = period_start - 1, period_end = period_end,
                                     options)$person_days

    if (nrow(output) > 0) {
      setorderv(output, col_order)
      setcolorder(output, col_order)
    }
    if (nrow(expected_outputs[[i]] > 0)) {
      expected_output <- dplyr::bind_cols(
        expected_outputs[[i]],
        fixed_data
      )
      setDT(expected_output)
      setorderv(expected_output, col_order)
      setcolorder(expected_output, col_order)

      expect_equal(output, expected_output)
    } else {
      expect_equal(nrow(output), 0)
    }
  }
})

test_that("historical case aggregation works", {

  #tests 47-48
  outcome_days <- c(0, 10)

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "COUNT")

  fixed_data <- data.table(PERIOD_START = period_start - 1, PERIOD_END = period_end,
                           ENCOUNTER_TYPE = 1,
                           AESI = "ADEM",
                           AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                                           labels = options$age_groups$labels, right = FALSE),
                           SEX = "M")

  expected_outputs <- list(
    data.table(COUNT = c(20)),
    data.table(COUNT = c(20))
  )

  for (i in 1:length(outcome_days)) {
    outcome_data <- data.table(
      PID = 1:21,
      AESI = "ADEM",
      EVENT_DATE = c(rep(period_start + outcome_days[i], 20), period_start - 30),
      ENCOUNTER_TYPE = 1
    )

    output <- aggregate_data_historic(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                      outcome_data,
                                      period_start - 1, period_end,
                                      options)$cases

    if (nrow(output) > 0) {
      setorderv(output, col_order)
      setcolorder(output, col_order)
    }
    if (nrow(expected_outputs[[i]] > 0)) {
      expected_output <- dplyr::bind_cols(
        expected_outputs[[i]],
        fixed_data
      )
      setDT(expected_output)
      setorderv(expected_output, col_order)
      setcolorder(expected_output, col_order)

      expect_equal(output, expected_output)
    } else {
      expect_equal(nrow(output), 0)
    }
  }
})

test_that("historical person days aggregation works", {

  #tests 47-48
  outcome_days <- c(0, 10)

  col_order <- c("PERIOD_START", "PERIOD_END", "AGE_GROUP", "SEX", "PERSON_DAYS")

  fixed_data <- data.table(PERIOD_START = period_start - 1, PERIOD_END = period_end,
                           AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                                           labels = options$age_groups$labels, right = FALSE),
                           SEX = "M")

  expected_outputs <- list(
    data.table(PERSON_DAYS = c(20*17)),
    data.table(PERSON_DAYS = c(20*17))
  )

  for (i in 1:length(outcome_days)) {
    outcome_data <- data.table(
      PID = 1:21,
      AESI = "ADEM",
      EVENT_DATE = c(rep(period_start + outcome_days[i], 20), period_start - 30),
      ENCOUNTER_TYPE = 1
    )

    output <- aggregate_data_historic(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                      outcome_data,
                                      period_start - 1, period_end,
                                      options)$person_days

    if (nrow(output) > 0) {
      setorderv(output, col_order)
      setcolorder(output, col_order)
    }
    if (nrow(expected_outputs[[i]] > 0)) {
      expected_output <- dplyr::bind_cols(
        expected_outputs[[i]],
        fixed_data
      )
      setDT(expected_output)
      setorderv(expected_output, col_order)
      setcolorder(expected_output, col_order)

      expect_equal(output, expected_output)
    } else {
      expect_equal(nrow(output), 0)
    }
  }
})

test_that("multiple period aggregation with self-controlled (post) study design works", {

  options <- options
  options$period_length <- "week"

  outcome_data <- data.table(
    PID = 2:19,
    AESI = "ADEM",
    EVENT_DATE = period_start + c(-2, -1, 1:13, -1, 2, 8),
    ENCOUNTER_TYPE = 1
  )

  expected_outputs <- list(
    data.table(),
    data.table(
      PERIOD_START = rep(period_start, 1),
      PERIOD_END = rep(period_start + 6, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c(rep("abc", 1)),
      V_TYPE = c(rep("PABC", 1)),
      V_DOSE = -1,
      WINDOW_RATIO = c(1),
      CASE_COUNT = c(0),
      CONTROL_COUNT = c(1)
    ),
    data.table(
      PERIOD_START = rep(period_start + 7, 1),
      PERIOD_END = rep(period_start + 13, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c(rep("abc", 3), rep("def", 2)),
      V_TYPE = c(rep("PABC", 3), rep("PDEF", 2)),
      V_DOSE = -1,
      WINDOW_RATIO = c(1, 5/2, 2, 1, 3),
      CASE_COUNT = c(2, 1, 0, 0, 0),
      CONTROL_COUNT = c(0, 0, 1, 2, 1)
    ),
    data.table(
      PERIOD_START = rep(period_start + 14, 1),
      PERIOD_END = rep(period_start + 20, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c(rep("abc", 1), rep("def", 1)),
      V_TYPE = c(rep("PABC", 1), rep("PDEF", 1)),
      V_DOSE = -1,
      WINDOW_RATIO = c(1, 1),
      CASE_COUNT = c(2, 1),
      CONTROL_COUNT = c(2, 0)
    )
  )

  for (i in 1:4) {
    output <- aggregate_data_self(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                  vaccination_data, outcome_data,
                                  period_start + (i-2)*7, period_start + (i-2)*7 + 6,
                                  options, comparator = "post")

    col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "WINDOW_RATIO", "CASE_COUNT", "CONTROL_COUNT")

    expected_output <- expected_outputs[[i]]

    if (i > 1) {
      setorderv(output, col_order)
      setcolorder(output, col_order)
      setorderv(expected_output, col_order)
      setcolorder(expected_output, col_order)
    }

    expect_equal(output, expected_output)
  }

})

test_that("multiple period aggregation with self-controlled (pre) study design works", {

  options <- options
  options$period_length <- "week"

  outcome_data <- data.table(
    PID = 1:16,
    AESI = "ADEM",
    EVENT_DATE = period_start + c(-2, -1, 1, 5, 2, -2, -3, 4, -2, 6, 8, -2, 10, -3, 4, -2),
    ENCOUNTER_TYPE = 1
  )

  expected_outputs <- list(
    data.table(),
    data.table(
      PERIOD_START = rep(period_start, 1),
      PERIOD_END = rep(period_start + 6, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c(rep("abc", 1), rep("def", 1)),
      V_TYPE = c(rep("PABC", 1), rep("PDEF", 1)),
      V_DOSE = -1,
      WINDOW_RATIO = c(1, 1),
      CASE_COUNT = c(2, 2),
      CONTROL_COUNT = c(6, 2)
    ),
    data.table(
      PERIOD_START = rep(period_start + 7, 1),
      PERIOD_END = rep(period_start + 13, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c(rep("abc", 2), rep("def", 1), rep("ghi", 1)),
      V_TYPE = c(rep("PABC", 2), rep("PDEF", 1), rep("PGHI", 1)),
      V_DOSE = -1,
      WINDOW_RATIO = c(1, 3, 1, 1),
      CASE_COUNT = c(0, 0, 1, 0),
      CONTROL_COUNT = c(1, 1, 3, 1)
    ),
    data.table()
  )

  for (i in 1:4) {
    output <- aggregate_data_self(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                  vaccination_data, outcome_data,
                                  period_start + (i-2)*7, period_start + (i-2)*7 + 6,
                                  options, comparator = "pre")

    col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "WINDOW_RATIO", "CASE_COUNT", "CONTROL_COUNT")

    expected_output <- expected_outputs[[i]]

    if (i > 1 & i < 4) {
      setorderv(output, col_order)
      setcolorder(output, col_order)
      setorderv(expected_output, col_order)
      setcolorder(expected_output, col_order)
    }

    expect_equal(output, expected_output)
  }

})

test_that("multiple period aggregation with concurrent (vaccinated) study design works", {

  # using unvac dataset
  v_date_adjustments <- c(0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 5, 5, 4, 4, 4,
                          3, 3, 2, 2, 2, 3, 3, 3, 4, 4, 9, 9, 11, 11, 11, 13, 13, 13)

  options <- options
  options$period_length <- "week"

  outcome_data <- data.table(
    PID = 2:18,
    AESI = "ADEM",
    EVENT_DATE = period_start + c(1, 6, 7, 5, 10, 11, 12, 10, 8, 15, 1, 15, 18, 16, 22, 6, 13),
    ENCOUNTER_TYPE = 1
  )

  expected_outputs <- list(
    data.table(
      PERIOD_START = rep(period_start - 1, 1),
      PERIOD_END = rep(period_start + 5, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c(rep("abc", 1)),
      V_TYPE = c(rep("PABC", 1)),
      V_DOSE = -1,
      RATIO = c(8/2),
      CASE_COUNT = c(1),
      CONTROL_COUNT = c(0)
    ),
    data.table(
      PERIOD_START = rep(period_start + 6, 1),
      PERIOD_END = rep(period_start + 12, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c("abc", "def", "abc", "def", "abc", "abc", "ghi", "abc"),
      V_TYPE = c("PABC", "PDEF", "PABC", "PDEF", "PABC", "PABC", "PGHI", "PABC"),
      V_DOSE = -1,
      RATIO = round(c(5/3, 2/3, 5/2, 4/3, 3/5, 3/10, 1/10, 2/10), options$ratio_precision),
      CASE_COUNT = c(0, 0, 1, 1, 0, 0, 0, 0),
      CONTROL_COUNT = c(1, 1, 0, 0, 1, 1, 2, 1)
    ),
    data.table(
      PERIOD_START = rep(period_start + 13, 1),
      PERIOD_END = rep(period_start + 19, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c("abc", "def", "def"),
      V_TYPE = c("PABC", "PDEF", "PDEF"),
      V_DOSE = -1,
      RATIO = round(c(2/10, 3/10, 3/11), options$ratio_precision),
      CASE_COUNT = c(0, 1, 0),
      CONTROL_COUNT = c(1, 1, 1)
    ),
    data.table()
  )

  for (i in 1:4) {
    output <- aggregate_data_concurrent(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                        copy(vaccination_data %>% dplyr::arrange(PID, V_DATE) %>% dplyr::mutate(V_DATE = V_DATE + v_date_adjustments)),
                                        outcome_data,
                                        period_start - 1 + (i-1)*7, period_start + 5 + (i-1)*7,
                                        options, comparator = "vaccinated")

    col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "RATIO", "CASE_COUNT", "CONTROL_COUNT")

    expected_output <- expected_outputs[[i]]

    if (i < 4) {
      setorderv(output, col_order)
      setcolorder(output, col_order)
      setorderv(expected_output, col_order)
      setcolorder(expected_output, col_order)

      expect_equal(output, expected_output)
    } else {
      expect_equal(nrow(output), 0)
    }
  }

})

test_that("multiple period aggregation with concurrent (unvaccinated) study design works", {

  # using unvac dataset
  v_date_adjustments <- c(0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 5, 5, 4, 4, 4,
                          3, 3, 2, 2, 2, 3, 3, 3, 4, 4, 9, 9, 11, 11, 11, 13, 13, 13)

  options <- options
  options$period_length <- "week"

  outcome_data <- data.table(
    PID = 2:18,
    AESI = "ADEM",
    EVENT_DATE = period_start + c(1, 0, 7, 5, 4, 5, 12, 10, 8, 15, 1, 15, 8, 6, 12, 6, 13),
    ENCOUNTER_TYPE = 1
  )

  expected_outputs <- list(
    data.table(
      PERIOD_START = rep(period_start - 1, 1),
      PERIOD_END = rep(period_start + 5, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c(rep("abc", 1)),
      V_TYPE = c("PABC"),
      V_DOSE = -1,
      RATIO = round(c(1/19, 2/18, 8/10, 8/8), options$ratio_precision),
      CASE_COUNT = c(0, 1, 0, 1),
      CONTROL_COUNT = c(1, 1, 1, 1)
    ),
    data.table(
      PERIOD_START = rep(period_start + 6, 1),
      PERIOD_END = rep(period_start + 12, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c("abc", "def", "abc", "def", "abc", "ghi"),
      V_TYPE = c("PABC", "PDEF", "PABC", "PDEF", "PABC", "PGHI"),
      V_DOSE = -1,
      RATIO = round(c(5/7, 2/7, 6/7, 4/7, 2/5, 1/5), options$ratio_precision),
      CASE_COUNT = c(1, 0, 0, 1, 0, 0),
      CONTROL_COUNT = c(2, 2, 1, 1, 1, 1)
    ),
    data.table(
      PERIOD_START = rep(period_start + 13, 1),
      PERIOD_END = rep(period_start + 19, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c("abc", "def", "ghi", "def"),
      V_TYPE = c("PABC", "PDEF", "PGHI", "PDEF"),
      V_DOSE = -1,
      RATIO = round(c(4/4, 1/4, 1/4, 3/4), options$ratio_precision),
      CASE_COUNT = c(0, 0, 0, 1),
      CONTROL_COUNT = c(1, 1, 1, 0)
    ),
    data.table()
  )

  for (i in 1:4) {
    output <- aggregate_data_concurrent(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                        copy(vaccination_data %>% dplyr::arrange(PID, V_DATE) %>% dplyr::mutate(V_DATE = V_DATE + v_date_adjustments)),
                                        outcome_data,
                                        period_start - 1 + (i-1)*7, period_start + 5 + (i-1)*7,
                                        options, comparator = "unvaccinated")

    col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "RATIO", "CASE_COUNT", "CONTROL_COUNT")

    expected_output <- expected_outputs[[i]]

    if (i < 4) {
      setorderv(output, col_order)
      setcolorder(output, col_order)
      setorderv(expected_output, col_order)
      setcolorder(expected_output, col_order)

      expect_equal(output, expected_output)
    } else {
      expect_equal(nrow(output), 0)
    }
  }

})

test_that("multiple period aggregation of exposed cases works", {

  # using unvac dataset
  v_date_adjustments <- c(0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 5, 5, 4, 4, 4,
                          3, 3, 2, 2, 2, 3, 3, 3, 4, 4, 9, 9, 11, 11, 11, 13, 13, 13)

  options <- options
  options$period_length <- "week"

  outcome_data <- data.table(
    PID = 2:18,
    AESI = "ADEM",
    EVENT_DATE = period_start + c(1, 0, 7, 5, 4, 5, 12, 10, 8, 15, 1, 15, 8, 6, 12, 6, 13),
    ENCOUNTER_TYPE = 1
  )

  expected_outputs <- list(
    data.table(
      PERIOD_START = rep(period_start - 1, 1),
      PERIOD_END = rep(period_start + 5, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c(rep("abc", 1)),
      V_TYPE = c("PABC"),
      V_DOSE = -1,
      CASE_COUNT = c(2)
    ),
    data.table(
      PERIOD_START = rep(period_start + 6, 1),
      PERIOD_END = rep(period_start + 12, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c("abc", "def"),
      V_TYPE = c("PABC", "PDEF"),
      V_DOSE = -1,
      CASE_COUNT = c(1, 1)
    ),
    data.table(
      PERIOD_START = rep(period_start + 13, 1),
      PERIOD_END = rep(period_start + 19, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c("def"),
      V_TYPE = c("PDEF"),
      V_DOSE = -1,
      CASE_COUNT = c(1)
    ),
    data.table()
  )

  for (i in 1:4) {
    output <- aggregate_data_exposed(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                     vaccination_data %>% dplyr::arrange(PID, V_DATE) %>% dplyr::mutate(V_DATE = V_DATE + v_date_adjustments),
                                     outcome_data,
                                     period_start - 1 + (i-1)*7, period_start + 5 + (i-1)*7,
                                     options)$cases

    col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "CASE_COUNT")

    expected_output <- expected_outputs[[i]]

    if (i < 4) {
      setorderv(output, col_order)
      setcolorder(output, col_order)
      setorderv(expected_output, col_order)
      setcolorder(expected_output, col_order)

      expect_equal(output, expected_output)
    } else {
      expect_equal(nrow(output), 0)
    }


  }

})

test_that("multiple period aggregation of exposed person days works", {

  # using unvac dataset
  v_date_adjustments <- c(0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 5, 5, 4, 4, 4,
                          3, 3, 2, 2, 2, 3, 3, 3, 4, 4, 9, 9, 11, 11, 11, 13, 13, 13)

  options <- options
  options$period_length <- "week"

  outcome_data <- data.table(
    PID = 2:18,
    AESI = "ADEM",
    EVENT_DATE = period_start + c(1, 0, 7, 5, 4, 5, 12, 10, 8, 15, 1, 15, 8, 6, 12, 6, 13),
    ENCOUNTER_TYPE = 1
  )

  expected_outputs <- list(
    data.table(
      PERIOD_START = rep(period_start - 1, 1),
      PERIOD_END = rep(period_start + 5, 1),
      AESI = c(rep("ADEM", 1), rep("Myo", 1), rep("Stroke", 1)),
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c(rep("abc", 1)),
      V_TYPE = c("PABC"),
      V_DOSE = -1,
      PERSON_DAYS = c(29)
    ),
    data.table(
      PERIOD_START = rep(period_start + 6, 1),
      PERIOD_END = rep(period_start + 12, 1),
      AESI = c(rep("ADEM", 3), rep("Myo", 3), rep("Stroke", 3)),
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c("abc", "def", "ghi"),
      V_TYPE = c("PABC", "PDEF", "PGHI"),
      V_DOSE = -1,
      PERSON_DAYS = c(30, 14, 2)
    ),
    data.table(
      PERIOD_START = rep(period_start + 13, 1),
      PERIOD_END = rep(period_start + 19, 1),
      AESI = c(rep("ADEM", 3), rep("Myo", 3), rep("Stroke", 3)),
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c("abc", "def", "ghi"),
      V_TYPE = c("PABC", "PDEF", "PGHI"),
      V_DOSE = -1,
      PERSON_DAYS = c(12, 13, 1)
    ),
    data.table(
      PERIOD_START = rep(period_start + 20, 1),
      PERIOD_END = rep(period_start + 26, 1),
      AESI = c(rep("ADEM", 1), rep("Myo", 1), rep("Stroke", 1)),
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      V_SUBTYPE = c("abc"),
      V_TYPE = c("PABC"),
      V_DOSE = -1,
      PERSON_DAYS = c(3)
    )
  )

  for (i in 1:4) {
    output <- aggregate_data_exposed(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                     vaccination_data %>% dplyr::arrange(PID, V_DATE) %>% dplyr::mutate(V_DATE = V_DATE + v_date_adjustments),
                                     outcome_data,
                                     period_start - 1 + (i-1)*7, period_start + 5 + (i-1)*7,
                                     options)$person_days

    col_order <- c("PERIOD_START", "PERIOD_END", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "PERSON_DAYS")

    expected_output <- expected_outputs[[i]]

    setorderv(output, col_order)
    setcolorder(output, col_order)
    setorderv(expected_output, col_order)
    setcolorder(expected_output, col_order)

    expect_equal(output, expected_output)
  }

})

test_that("multiple period aggregation of historical cases works", {

  # using unvac dataset
  v_date_adjustments <- c(0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 5, 5, 4, 4, 4,
                          3, 3, 2, 2, 2, 3, 3, 3, 4, 4, 9, 9, 11, 11, 11, 13, 13, 13)

  options <- options
  options$period_length <- "week"

  outcome_data <- data.table(
    PID = 1:18,
    AESI = "ADEM",
    EVENT_DATE = period_start + c(-10, 1, 0, 7, 5, 4, 5, 12, 10, 8, 15, 1, 15, 8, 6, 12, 6, 13),
    ENCOUNTER_TYPE = 1
  )

  expected_outputs <- list(
    data.table(
      PERIOD_START = rep(period_start - 1, 1),
      PERIOD_END = rep(period_start + 5, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      COUNT = c(6)
    ),
    data.table(
      PERIOD_START = rep(period_start + 6, 1),
      PERIOD_END = rep(period_start + 12, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      COUNT = c(8)
    ),
    data.table(
      PERIOD_START = rep(period_start + 13, 1),
      PERIOD_END = rep(period_start + 19, 1),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      COUNT = c(3)
    ),
    data.table()
  )

  for (i in 1:4) {
    output <- aggregate_data_historic(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                      outcome_data,
                                      period_start - 1 + (i-1)*7, period_start + 5 + (i-1)*7,
                                      options)$cases

    col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "COUNT")

    expected_output <- expected_outputs[[i]]

    if (i < 4) {
      setorderv(output, col_order)
      setcolorder(output, col_order)
      setorderv(expected_output, col_order)
      setcolorder(expected_output, col_order)

      expect_equal(output, expected_output)
    } else {
      expect_equal(nrow(output), 0)
    }

  }

})

test_that("multiple period aggregation of historic person days works", {

  # using unvac dataset
  v_date_adjustments <- c(0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 5, 5, 4, 4, 4,
                          3, 3, 2, 2, 2, 3, 3, 3, 4, 4, 9, 9, 11, 11, 11, 13, 13, 13)

  options <- options
  options$period_length <- "week"

  outcome_data <- data.table(
    PID = 1:18,
    AESI = "ADEM",
    EVENT_DATE = period_start + c(-10, 1, 0, 7, 5, 4, 5, 12, 10, 8, 15, 1, 15, 8, 6, 12, 6, 13),
    ENCOUNTER_TYPE = 1
  )

  expected_outputs <- list(
    data.table(
      PERIOD_START = rep(period_start - 1, 1),
      PERIOD_END = rep(period_start + 5, 1),
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      PERSON_DAYS = c(140)
    ),
    data.table(
      PERIOD_START = rep(period_start + 6, 1),
      PERIOD_END = rep(period_start + 12, 1),
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      PERSON_DAYS = c(140)
    ),
    data.table(
      PERIOD_START = rep(period_start + 13, 1),
      PERIOD_END = rep(period_start + 19, 1),
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      PERSON_DAYS = c(140)
    ),
    data.table(
      PERIOD_START = rep(period_start + 20, 1),
      PERIOD_END = rep(period_start + 26, 1),
      AGE_GROUP = cut(80, breaks = options$age_groups$bounds,
                      labels = options$age_groups$labels, right = FALSE),
      SEX = "M",
      PERSON_DAYS = c(140)
    )
  )

  for (i in 1:4) {
    output <- aggregate_data_historic(copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01"))),
                                      outcome_data,
                                      period_start - 1 + (i-1)*7, period_start + 5 + (i-1)*7,
                                      options)$person_days

    col_order <- c("PERIOD_START", "PERIOD_END", "AGE_GROUP", "SEX", "PERSON_DAYS")

    expected_output <- expected_outputs[[i]]

    setorderv(output, col_order)
    setcolorder(output, col_order)
    setorderv(expected_output, col_order)
    setcolorder(expected_output, col_order)

    expect_equal(output, expected_output)

  }

})

test_that("dose splits works", {

  pd <- copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01")))
  vd <- copy(vaccination_data)
  vd[order(V_DATE), V_DOSE_disease := seq_len(.N), by = .(PID)]
  od <- data.table(PID = 1:5, AESI = "ADEM", EVENT_DATE = period_start + 1, ENCOUNTER_TYPE = 1L)

  output <- aggregate_data_self(pd, vd, od, period_start, period_end, options, "post",
                                analysis_types = c("primary", "subgroup_dose"))

  expect_true(-1L %in% output$V_DOSE)
  expect_true(any(output$V_DOSE >= 1L))
  expect_true(all(output$V_SUBTYPE == "pooled"))

})

test_that("age splits works", {

  pd <- data.table(PID = 1:2,
                   DOB = c(as.Date("1940-01-01"), as.Date("1970-01-01")),
                   SEX = "M", ENROL_DATE = as.Date("1940-01-01"),
                   CENSOR_DATE = NA_integer_, CENSOR_TYPE = NA_character_)
  vd <- data.table(PID = 1:2, V_DATE = period_start, V_SUBTYPE = "abc", V_DOSE = -1L)
  od <- data.table(PID = 1:2, AESI = "ADEM", EVENT_DATE = period_start + 1, ENCOUNTER_TYPE = 1L)

  output <- aggregate_data_exposed(pd, vd, od, period_start, period_end, options)$cases

  expect_setequal(as.character(unique(output$AGE_GROUP)), c("80+", "50-54"))

})

test_that("different risk-washout windows works", {

  pd <- data.table(PID = 1L, DOB = as.Date("1940-01-01"), SEX = "M",
                   ENROL_DATE = as.Date("1940-01-01"),
                   CENSOR_DATE = NA_integer_, CENSOR_TYPE = NA_character_)
  vd <- data.table(PID = 1L, V_DATE = period_start, V_SUBTYPE = "abc", V_DOSE = -1L)
  od <- data.table(PID = 1L, AESI = "ADEM", EVENT_DATE = period_start + 5, ENCOUNTER_TYPE = 1L)

  opts_narrow <- options
  opts_narrow$outcome_info <- copy(options$outcome_info)
  opts_narrow$outcome_info[AESI == "ADEM", risk_upper := 2L]

  opts_wide <- options
  opts_wide$outcome_info <- copy(options$outcome_info)
  opts_wide$outcome_info[AESI == "ADEM", risk_upper := 7L]

  output_narrow <- aggregate_data_exposed(copy(pd), copy(vd), od, period_start, period_end, opts_narrow)$cases
  output_wide   <- aggregate_data_exposed(copy(pd), copy(vd), od, period_start, period_end, opts_wide)$cases

  expect_equal(nrow(output_narrow), 0L)
  expect_equal(sum(output_wide$CASE_COUNT), 1L)

})

test_that("summarising by ratio works", {

  pd <- data.table(PID = 1:5, DOB = as.Date("1940-01-01"), SEX = "M",
                   ENROL_DATE = as.Date("1940-01-01"),
                   CENSOR_DATE = NA_integer_, CENSOR_TYPE = NA_character_)
  vd <- data.table(PID = 1:5, V_DATE = period_start, V_SUBTYPE = "abc", V_DOSE = -1L)
  od <- data.table(PID = 1:5, AESI = "ADEM", EVENT_DATE = period_start + 1, ENCOUNTER_TYPE = 1L)

  opts_ratio <- options
  opts_ratio$outcome_info <- copy(options$outcome_info)
  opts_ratio$outcome_info[AESI == "ADEM", control_post_target := 3L]

  opts_1dp <- opts_ratio
  opts_1dp$ratio_precision <- 1L
  output_1dp <- aggregate_data_self(copy(pd), copy(vd), od, period_start, period_end, opts_1dp, "post")

  opts_0dp <- opts_ratio
  opts_0dp$ratio_precision <- 0L
  output_0dp <- aggregate_data_self(copy(pd), copy(vd), od, period_start, period_end, opts_0dp, "post")

  expect_true(round(1/3, 1) %in% output_1dp$WINDOW_RATIO)
  expect_true(round(1/3, 0) %in% output_0dp$WINDOW_RATIO)
  expect_false(identical(sort(unique(output_1dp$WINDOW_RATIO)), sort(unique(output_0dp$WINDOW_RATIO))))

})

test_that("multiple outcomes works", {

  pd <- copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01")))
  od <- rbindlist(list(
    data.table(PID = 1:5, AESI = "ADEM", EVENT_DATE = period_start + 1, ENCOUNTER_TYPE = 1L),
    data.table(PID = 1:3, AESI = "Myo",  EVENT_DATE = period_start + 1, ENCOUNTER_TYPE = 1L)
  ))

  output <- aggregate_data_self(pd, copy(vaccination_data), od, period_start, period_end, options, "post")

  expect_setequal(unique(output$AESI), c("ADEM", "Myo"))

})

test_that("relatively short period lengths works", {

  periods <- split_time_periods(period_start, period_end, "day", align_periods = FALSE)
  expect_equal(nrow(periods), as.integer(period_end - period_start))
  expect_equal(periods$period_start[1], period_start)
  expect_equal(periods$period_end[nrow(periods)], period_end)

  pd <- data.table(PID = 1:20, DOB = as.Date("1940-01-01"), SEX = "M",
                   ENROL_DATE = as.Date("1940-01-01"),
                   CENSOR_DATE = NA_integer_, CENSOR_TYPE = NA_character_)
  od <- data.table(PID = 1:5, AESI = "ADEM", EVENT_DATE = period_start, ENCOUNTER_TYPE = 1L)

  output_hist <- aggregate_data_historic(pd, od, period_start, period_start, options)

  expect_equal(sum(output_hist$person_days$PERSON_DAYS), 20L)
  expect_equal(sum(output_hist$cases$COUNT), 5L)

})

test_that("no outcomes works", {

  pd <- copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01")))
  od_before <- data.table(PID = 1L, AESI = "ADEM", EVENT_DATE = period_start - 100, ENCOUNTER_TYPE = 1L)

  output_self <- aggregate_data_self(copy(pd), copy(vaccination_data), od_before, period_start, period_end, options, "post")
  output_hist <- aggregate_data_historic(copy(pd), od_before, period_start, period_end, options)

  expect_equal(nrow(output_self), 0L)
  expect_equal(nrow(output_hist$cases), 0L)
  expect_true(nrow(output_hist$person_days) > 0L)

})

test_that("enrol date works", {

  pd <- data.table(PID = 1:2, DOB = as.Date("1940-01-01"), SEX = "M",
                   ENROL_DATE = c(as.Date("1940-01-01"), period_start + 7L),
                   CENSOR_DATE = c(as.Date(NA), as.Date(NA)),
                   CENSOR_TYPE = NA_character_)
  od <- data.table(PID = integer(0), AESI = character(0),
                   EVENT_DATE = as.Date(character(0)), ENCOUNTER_TYPE = integer(0))

  output <- aggregate_data_historic(pd, od, period_start, period_end, options)

  # Person 1 (enrolled 1940): full 16 days; Person 2 (enrolled period_start+7): 16 - 7 = 9 days
  expect_equal(sum(output$person_days$PERSON_DAYS), 16L + 9L)

})

test_that("censoring works", {

  pd <- data.table(PID = 1:2, DOB = as.Date("1940-01-01"), SEX = "M",
                   ENROL_DATE = as.Date("1940-01-01"),
                   CENSOR_DATE = c(as.Date(NA), period_start + 5L),
                   CENSOR_TYPE = c(NA_character_, "D"))
  od <- data.table(PID = integer(0), AESI = character(0),
                   EVENT_DATE = as.Date(character(0)), ENCOUNTER_TYPE = integer(0))

  output <- aggregate_data_historic(pd, od, period_start, period_end, options)

  # Person 1: full 16 days; Person 2 (censored at period_start+5): 16 - (period_end - (period_start+5)) = 16 - 10 = 6 days
  expect_equal(sum(output$person_days$PERSON_DAYS), 16L + 6L)

})

test_that("passing datasets by reference works", {

  pd <- copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01")))
  setDT(pd)
  vd <- copy(vaccination_data)
  od <- data.table(PID = 1:5, AESI = "ADEM", EVENT_DATE = period_start + 1, ENCOUNTER_TYPE = 1L)

  pd_cols_before <- names(pd)
  vd_cols_before <- names(vd)

  aggregate_data_self(pd, vd, od, period_start, period_end, options, "post", allow_side_effects = FALSE)

  expect_equal(names(pd), pd_cols_before)
  expect_equal(names(vd), vd_cols_before)

})

test_that("unrecognised vaccine brands error works", {

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  pd <- data.table(PID = paste0("p", 1:5), DOB = 19400101L, SEX = "M",
                   ENROL_DATE = NA_integer_, CENSOR_DATE = NA_integer_, CENSOR_TYPE = NA_character_)
  vd <- data.table(PID = paste0("p", 1:5),
                   V_DATE = convert_date_to_number(period_start), V_SUBTYPE = "UNKNOWN_BRAND")
  od <- data.table(PID = paste0("p", 1:5), AESI = "ADEM",
                   EVENT_DATE = convert_date_to_number(period_start + 1), ENCOUNTER_TYPE = 1L)

  expect_error(
    aggregate_data(pd, vd, od,
                   cycle_start_date = period_start, cycle_end_date = period_end,
                   site_code = "TEST", options = options, options_file_location = NULL,
                   working_directory = tmp_dir,
                   restore_input_data = FALSE, skip_user_prompts = TRUE),
    regexp = "not recognised"
  )

})

test_that("invalid design_selection values produce an error", {

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  pd <- data.table(PID = paste0("p", 1:5), DOB = 19400101L, SEX = "m",
                   ENROL_DATE = NA_integer_, CENSOR_DATE = NA_integer_, CENSOR_TYPE = NA_character_)
  vd <- data.table(PID = paste0("p", 1:5),
                   V_DATE = convert_date_to_number(period_start), V_SUBTYPE = "abc")
  od <- data.table(PID = paste0("p", 1:5), AESI = "ADEM",
                   EVENT_DATE = convert_date_to_number(period_start + 1), ENCOUNTER_TYPE = 1L)

  expect_error(
    aggregate_data(pd, vd, od,
                   cycle_start_date = period_start, cycle_end_date = period_end,
                   site_code = "TEST", options = options, options_file_location = NULL,
                   working_directory = tmp_dir,
                   design_selection = c("self_post", "not_a_design"),
                   restore_input_data = FALSE, skip_user_prompts = TRUE),
    regexp = "Unknown design"
  )

})

test_that("participation_level deprecation translates to correct design_selection", {

  tmp_dir_1 <- tempfile()
  tmp_dir_2 <- tempfile()
  tmp_dir_3 <- tempfile()
  dir.create(tmp_dir_1); dir.create(tmp_dir_2); dir.create(tmp_dir_3)
  on.exit({
    unlink(tmp_dir_1, recursive = TRUE)
    unlink(tmp_dir_2, recursive = TRUE)
    unlink(tmp_dir_3, recursive = TRUE)
  }, add = TRUE)

  pd <- data.table(PID = paste0("p", 1:5), DOB = 19400101L, SEX = "m",
                   ENROL_DATE = NA_integer_, CENSOR_DATE = NA_integer_, CENSOR_TYPE = NA_character_)
  vd <- data.table(PID = paste0("p", 1:5),
                   V_DATE = convert_date_to_number(period_start), V_SUBTYPE = "abc")
  od <- data.table(
    PID = c("p1", paste0("p", 1:5)),
    AESI = "ADEM",
    EVENT_DATE = c(convert_date_to_number(period_start - 730),
                   rep(convert_date_to_number(period_start + 1), 5L)),
    ENCOUNTER_TYPE = 1L
  )

  folder_name <- paste0("RCA_COVID_TEST_", convert_date_to_number(period_start), "-", convert_date_to_number(period_end))

  # participation_level = 1 → self_post only
  pd1 <- copy(pd); vd1 <- copy(vd); od1 <- copy(od)
  expect_warning(
    aggregate_data(pd1, vd1, od1,
                   cycle_start_date = period_start, cycle_end_date = period_end,
                   site_code = "TEST", options = options, options_file_location = NULL,
                   working_directory = tmp_dir_1,
                   participation_level = 1,
                   restore_input_data = FALSE, skip_user_prompts = TRUE),
    regexp = "participation_level.*deprecated"
  )
  expect_true(file.exists(file.path(tmp_dir_1, folder_name, "data_self_post.parquet")))
  expect_false(file.exists(file.path(tmp_dir_1, folder_name, "data_self_pre.parquet")))

  # participation_level = 2 → self_post, self_pre, historical, concurrent_vac
  pd2 <- copy(pd); vd2 <- copy(vd); od2 <- copy(od)
  expect_warning(
    aggregate_data(pd2, vd2, od2,
                   cycle_start_date = period_start, cycle_end_date = period_end,
                   site_code = "TEST", options = options, options_file_location = NULL,
                   working_directory = tmp_dir_2,
                   participation_level = 2,
                   restore_input_data = FALSE, skip_user_prompts = TRUE),
    regexp = "participation_level.*deprecated"
  )
  expect_true(file.exists(file.path(tmp_dir_2, folder_name, "data_self_pre.parquet")))
  expect_true(file.exists(file.path(tmp_dir_2, folder_name, "data_concurrent_vac.parquet")))
  expect_false(file.exists(file.path(tmp_dir_2, folder_name, "data_concurrent_unvac.parquet")))

  # participation_level = 3 → all five designs
  pd3 <- copy(pd); vd3 <- copy(vd); od3 <- copy(od)
  expect_warning(
    aggregate_data(pd3, vd3, od3,
                   cycle_start_date = period_start, cycle_end_date = period_end,
                   site_code = "TEST", options = options, options_file_location = NULL,
                   working_directory = tmp_dir_3,
                   participation_level = 3,
                   restore_input_data = FALSE, skip_user_prompts = TRUE),
    regexp = "participation_level.*deprecated"
  )
  expect_true(file.exists(file.path(tmp_dir_3, folder_name, "data_concurrent_unvac.parquet")))

})

test_that("allowing and disallowing side effects works", {

  od <- data.table(PID = 1:5, AESI = "ADEM", EVENT_DATE = period_start + 1, ENCOUNTER_TYPE = 1L)

  pd1 <- copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01")))
  setDT(pd1)
  aggregate_data_self(pd1, copy(vaccination_data), od, period_start, period_end, options, "post",
                      allow_side_effects = FALSE)
  expect_false("AGE_GROUP" %in% names(pd1))

  pd2 <- copy(person_data %>% dplyr::mutate(DOB = as.Date("1940-01-01"), SEX = "M", ENROL_DATE = as.Date("1940-01-01")))
  setDT(pd2)
  aggregate_data_self(pd2, copy(vaccination_data), od, period_start, period_end, options, "post",
                      allow_side_effects = TRUE)
  expect_true("AGE_GROUP" %in% names(pd2))

})

test_that("splitting or not splitting by ID works", {

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  opts_simple <- options
  opts_simple$included_analyses <- c("primary")
  opts_simple$period_length <- "month"
  opts_simple$align_periods <- FALSE

  pd <- data.table(PID = paste0("p", 1:10), DOB = 19400101L, SEX = "m",
                   ENROL_DATE = NA_integer_, CENSOR_DATE = NA_integer_, CENSOR_TYPE = NA_character_)
  vd <- data.table(PID = paste0("p", 1:10),
                   V_DATE = convert_date_to_number(period_start), V_SUBTYPE = "abc")
  od <- data.table(
    PID = c("p1", paste0("p", 1:10)), AESI = "ADEM",
    EVENT_DATE = c(convert_date_to_number(period_start - 800),
                   rep(convert_date_to_number(period_start + 1), 10L)),
    ENCOUNTER_TYPE = 1L
  )

  folder_name <- paste0("RCA_COVID_TEST_", convert_date_to_number(period_start), "-", convert_date_to_number(period_end))

  tmp_nosplit <- file.path(tmp_dir, "nosplit")
  dir.create(tmp_nosplit)
  pd1 <- copy(pd); vd1 <- copy(vd); od1 <- copy(od)
  aggregate_data(pd1, vd1, od1,
                 cycle_start_date = period_start, cycle_end_date = period_end,
                 site_code = "TEST", options = opts_simple, options_file_location = NULL,
                 working_directory = tmp_nosplit, split_size = 1000,
                 design_selection = c("self_post"),
                 restore_input_data = FALSE, skip_user_prompts = TRUE)

  tmp_split <- file.path(tmp_dir, "split")
  dir.create(tmp_split)
  pd2 <- copy(pd); vd2 <- copy(vd); od2 <- copy(od)
  aggregate_data(pd2, vd2, od2,
                 cycle_start_date = period_start, cycle_end_date = period_end,
                 site_code = "TEST", options = opts_simple, options_file_location = NULL,
                 working_directory = tmp_split, split_size = 3,
                 design_selection = c("self_post"),
                 restore_input_data = FALSE, skip_user_prompts = TRUE)

  result_nosplit <- read_output_file(file.path(tmp_nosplit, folder_name, "data_self_post.parquet"), "parquet")
  result_split   <- read_output_file(file.path(tmp_split,   folder_name, "data_self_post.parquet"), "parquet")

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI",
                 "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "WINDOW_RATIO",
                 "CASE_COUNT", "CONTROL_COUNT")
  setorderv(result_nosplit, col_order)
  setorderv(result_split, col_order)
  setcolorder(result_nosplit, col_order)
  setcolorder(result_split, col_order)

  expect_equal(result_nosplit, result_split)

})

test_that("extra PIDs in vaccination and outcome inputs are removed", {

  pd <- data.table(PID = 1:5, DOB = as.Date("1940-01-01"), SEX = "M",
                   ENROL_DATE = as.Date("1940-01-01"),
                   CENSOR_DATE = NA_integer_, CENSOR_TYPE = NA_character_)
  vd_normal <- data.table(PID = 1:5, V_DATE = period_start, V_SUBTYPE = "abc", V_DOSE = -1L)
  od_normal <- data.table(PID = 1:5, AESI = "ADEM", EVENT_DATE = period_start + 1, ENCOUNTER_TYPE = 1L)

  vd_extra <- rbindlist(list(
    vd_normal,
    data.table(PID = 50:55, V_DATE = period_start, V_SUBTYPE = "abc", V_DOSE = -1L)
  ))
  od_extra <- rbindlist(list(
    od_normal,
    data.table(PID = 50:55, AESI = "ADEM", EVENT_DATE = period_start + 1, ENCOUNTER_TYPE = 1L)
  ))

  output_normal <- aggregate_data_self(copy(pd), copy(vd_normal), od_normal,
                                       period_start, period_end, options, "post")
  output_extra  <- aggregate_data_self(copy(pd), copy(vd_extra),  od_extra,
                                       period_start, period_end, options, "post")

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI",
                 "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "WINDOW_RATIO",
                 "CASE_COUNT", "CONTROL_COUNT")
  setorderv(output_normal, col_order)
  setorderv(output_extra, col_order)
  setcolorder(output_normal, col_order)
  setcolorder(output_extra, col_order)

  expect_equal(output_normal, output_extra)

})

test_that("inputting in chunks works", {

  tmp_dir <- tempfile()
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  opts_simple <- options
  opts_simple$included_analyses <- c("primary")
  opts_simple$period_length <- "month"
  opts_simple$align_periods <- FALSE

  pd_all <- data.table(PID = paste0("p", 1:10), DOB = 19400101L, SEX = "m",
                       ENROL_DATE = NA_integer_, CENSOR_DATE = NA_integer_, CENSOR_TYPE = NA_character_)
  vd_all <- data.table(PID = paste0("p", 1:10),
                       V_DATE = convert_date_to_number(period_start), V_SUBTYPE = "abc")
  od_all <- data.table(
    PID = c("p1", paste0("p", 1:10)), AESI = "ADEM",
    EVENT_DATE = c(convert_date_to_number(period_start - 800),
                   rep(convert_date_to_number(period_start + 1), 10L)),
    ENCOUNTER_TYPE = 1L
  )

  folder_name <- paste0("RCA_COVID_TEST_", convert_date_to_number(period_start), "-", convert_date_to_number(period_end))

  tmp_all <- file.path(tmp_dir, "all")
  dir.create(tmp_all)
  pd_r <- copy(pd_all); vd_r <- copy(vd_all); od_r <- copy(od_all)
  aggregate_data(pd_r, vd_r, od_r,
                 cycle_start_date = period_start, cycle_end_date = period_end,
                 site_code = "TEST", options = opts_simple, options_file_location = NULL,
                 working_directory = tmp_all, design_selection = c("self_post"),
                 restore_input_data = FALSE, skip_user_prompts = TRUE)

  tmp_chunks <- file.path(tmp_dir, "chunks")
  dir.create(tmp_chunks)

  pd1 <- copy(pd_all[1:5])
  vd1 <- data.table(PID = paste0("p", 1:5), V_DATE = convert_date_to_number(period_start), V_SUBTYPE = "abc")
  od1 <- data.table(
    PID = c("p1", paste0("p", 1:5)), AESI = "ADEM",
    EVENT_DATE = c(convert_date_to_number(period_start - 800),
                   rep(convert_date_to_number(period_start + 1), 5L)),
    ENCOUNTER_TYPE = 1L
  )
  aggregate_data(pd1, vd1, od1,
                 cycle_start_date = period_start, cycle_end_date = period_end,
                 site_code = "TEST", options = opts_simple, options_file_location = NULL,
                 working_directory = tmp_chunks, design_selection = c("self_post"),
                 input_data_in_chunks = TRUE, final_chunk = FALSE,
                 restore_input_data = FALSE, skip_user_prompts = TRUE)

  pd2 <- copy(pd_all[6:10])
  vd2 <- data.table(PID = paste0("p", 6:10), V_DATE = convert_date_to_number(period_start), V_SUBTYPE = "abc")
  od2 <- data.table(
    PID = c("p6", paste0("p", 6:10)), AESI = "ADEM",
    EVENT_DATE = c(convert_date_to_number(period_start - 800),
                   rep(convert_date_to_number(period_start + 1), 5L)),
    ENCOUNTER_TYPE = 1L
  )
  aggregate_data(pd2, vd2, od2,
                 cycle_start_date = period_start, cycle_end_date = period_end,
                 site_code = "TEST", options = opts_simple, options_file_location = NULL,
                 working_directory = tmp_chunks, design_selection = c("self_post"),
                 input_data_in_chunks = TRUE, final_chunk = TRUE,
                 restore_input_data = FALSE, skip_user_prompts = TRUE)

  result_all    <- read_output_file(file.path(tmp_all,    folder_name, "data_self_post.parquet"), "parquet")
  result_chunks <- read_output_file(file.path(tmp_chunks, folder_name, "data_self_post.parquet"), "parquet")

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI",
                 "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "WINDOW_RATIO",
                 "CASE_COUNT", "CONTROL_COUNT")
  setorderv(result_all, col_order)
  setorderv(result_chunks, col_order)
  setcolorder(result_all, col_order)
  setcolorder(result_chunks, col_order)

  expect_equal(result_all, result_chunks)

})


test_that("complete aggregation process works", {

  # using unvac dataset
  v_date_adjustments <- c(0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 5, 6, 6, 5, 5, 4, 4, 4,
                          3, 3, 2, 2, 2, 3, 3, 3, 4, 4, 9, 9, 11, 11, 11, 13, 13, 13)

  options <- options
  options$period_length <- "week"
  options$align_periods <- FALSE
  options$vaccine_info <- data.table(V_SUBTYPE = c("abc", "def", "ghi"),
                                     V_TYPE = c("PABC", "PDEF", "PABC"))

  person_data <- data.table(
    PID = paste0("p", 1:20),
    DOB = 19400101,
    SEX = "m",
    ENROL_DATE = NA_integer_,
    CENSOR_DATE = NA_integer_,
    CENSOR_TYPE = NA_character_
  )

  vaccination_data <- data.table(
    PID = paste0("p", c(1:16, 2:6, 6, 7, 8, 9, 9, 10, 11, 11, 12, 12, 13, 14, 15, 15, 16, 16)),
    V_DATE = convert_date_to_number(c(rep(period_start, 16) + c(0:6, 5, 4, 3, 2:4, 9, 11, 13),
                                      period_start + 9,
                                      period_start + 7,
                                      period_start + 7,
                                      period_start + 6,
                                      period_start + 7, period_start + 13,
                                      period_start + 14,
                                      period_start + 7,
                                      period_start + 6, period_start + 11,
                                      period_start + 7,
                                      period_start + 6, period_start + 8,
                                      period_start + 7, period_start + 12,
                                      period_start + 13,
                                      period_start + 15,
                                      period_start + 17, period_start + 19,
                                      period_start + 16, period_start + 18)),
    V_SUBTYPE = c(rep("abc", 16),
                "abc",
                "abc",
                "abc",
                "abc",
                "abc", "abc",
                "def",
                "def",
                "def", "ghi",
                "def",
                "def", "abc",
                "def", "abc",
                "def",
                "def",
                "def", "abc",
                "def", "abc")
  )

  outcome_data <- data.table(
    PID = paste0("p", 1:18),
    AESI = "ADEM",
    EVENT_DATE = convert_date_to_number(period_start + c(-700, 1, 6, 7, 5, 10, 5, 12, 10, 8, 15, 1, 15, 8, 6, 22, 6, 24)),
    ENCOUNTER_TYPE = 1
  )

  output <- aggregate_data(person_data,
                           vaccination_data,
                           outcome_data,
                           cycle_start = period_start - 1,
                           cycle_end = period_start + 33,
                           site_code = "TEST_SITE",
                           options_file_location = NULL,
                           options = options,
                           skip_user_prompts = TRUE)

  observed_self_pre <- read_output_file("RCA_COVID_TEST_SITE_20201231-20210203/data_self_pre.parquet", "parquet")
  observed_self_post <- read_output_file("RCA_COVID_TEST_SITE_20201231-20210203/data_self_post.parquet", "parquet")
  observed_exposed_cases <- read_output_file("RCA_COVID_TEST_SITE_20201231-20210203/data_exposed_cases.parquet", "parquet")
  observed_exposed_pd <- read_output_file("RCA_COVID_TEST_SITE_20201231-20210203/data_exposed_person_days.parquet", "parquet")
  observed_concurrent_vac <- read_output_file("RCA_COVID_TEST_SITE_20201231-20210203/data_concurrent_vac.parquet", "parquet")
  observed_concurrent_unvac <- read_output_file("RCA_COVID_TEST_SITE_20201231-20210203/data_concurrent_unvac.parquet", "parquet")
  observed_desc_vac <- read_output_file("RCA_COVID_TEST_SITE_20201231-20210203/data_descriptive_vaccinations.parquet", "parquet")
  observed_desc_outcome <- read_output_file("RCA_COVID_TEST_SITE_20201231-20210203/data_descriptive_outcomes.parquet", "parquet")

  expected_exposed_cases <- rbindlist(list(
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 1), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 1), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = "pooled",
      V_DOSE = -1,
      CASE_COUNT = c(2, 2, 1)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 1), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 1), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = "pooled",
      V_DOSE = c(1, 2, 2),
      CASE_COUNT = c(2, 2, 1)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 2), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 2), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = c("PABC", "PABC", "PDEF", "PDEF"),
      V_DOSE = -1,
      CASE_COUNT = c(2, 1, 1, 1)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 2), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 2), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = c("PABC", "PABC", "PDEF", "PDEF"),
      V_DOSE = c(1, 2, 1, 1),
      CASE_COUNT = c(2, 1, 1, 1)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 2), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 2), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = c("abc", "abc", "def", "def"),
      V_TYPE = c("PABC", "PABC", "PDEF", "PDEF"),
      V_DOSE = -1,
      CASE_COUNT = c(2, 1, 1, 1)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 2), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 2), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = c("abc", "abc", "def", "def"),
      V_TYPE = c("PABC", "PABC", "PDEF", "PDEF"),
      V_DOSE = c(1, 2, 1, 1),
      CASE_COUNT = c(2, 1, 1, 1)
    )
  ))

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "CASE_COUNT")

  setorderv(expected_exposed_cases, col_order)
  setcolorder(expected_exposed_cases, col_order)
  setorderv(observed_exposed_cases, col_order)
  setcolorder(observed_exposed_cases, col_order)

  expect_equal(observed_exposed_cases, expected_exposed_cases)

  expected_exposed_pd <- rbindlist(list(
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 1), rep(period_start + 13, 1), rep(period_start + 20, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 1), rep(period_start + 19, 1), rep(period_start + 26, 1))),
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = "pooled",
      V_DOSE = -1,
      PERSON_DAYS = c(29, 46, 26, 3)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 3), rep(period_start + 13, 3), rep(period_start + 20, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 3), rep(period_start + 19, 3), rep(period_start + 26, 1))),
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = "pooled",
      V_DOSE = c(1, 1, 2, 3, 1, 2, 3, 3),
      PERSON_DAYS = c(29, 11, 29, 6, 4, 13, 9, 3)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 2), rep(period_start + 13, 2), rep(period_start + 20, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 2), rep(period_start + 19, 2), rep(period_start + 26, 1))),
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = c("PABC", "PABC", "PDEF", "PABC", "PDEF", "PABC"),
      V_DOSE = -1,
      PERSON_DAYS = c(29, 32, 14, 13, 13, 3)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 3), rep(period_start + 13, 4), rep(period_start + 20, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 3), rep(period_start + 19, 4), rep(period_start + 26, 1))),
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = c("PABC", "PABC", "PABC", "PDEF", "PABC", "PABC", "PABC", "PDEF", "PABC"),
      V_DOSE = c(1, 1, 2, 1, 1, 2, 3, 1, 2),
      PERSON_DAYS = c(29, 11, 21, 14, 4, 6, 3, 13, 3)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 3), rep(period_start + 13, 3), rep(period_start + 20, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 3), rep(period_start + 19, 3), rep(period_start + 26, 1))),
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = c("abc", "abc", "def", "ghi", "abc", "def", "ghi", "abc"),
      V_TYPE = c("PABC", "PABC", "PDEF", "PABC", "PABC", "PDEF", "PABC", "PABC"),
      V_DOSE = -1,
      PERSON_DAYS = c(29, 30, 14, 2, 12, 13, 1, 3)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 4), rep(period_start + 13, 5), rep(period_start + 20, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 4), rep(period_start + 19, 5), rep(period_start + 26, 1))),
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = c("abc", "abc", "abc", "def", "ghi", "abc", "abc", "abc", "def", "ghi", "abc"),
      V_TYPE = c("PABC", "PABC", "PABC", "PDEF", "PABC", "PABC", "PABC", "PABC", "PDEF", "PABC", "PABC"),
      V_DOSE = c(1, 1, 2, 1, 1, 1, 2, 3, 1, 1, 2),
      PERSON_DAYS = c(29, 11, 19, 14, 2, 4, 5, 3, 13, 1, 3)
    )
  ))

  expected_exposed_pd <- rbindlist(list(
    expected_exposed_pd,
    expected_exposed_pd %>% dplyr::mutate(AESI = "Myo"),
    expected_exposed_pd %>% dplyr::mutate(AESI = "Stroke")
  ))

  col_order <- c("PERIOD_START", "PERIOD_END", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "PERSON_DAYS")

  setorderv(expected_exposed_pd, col_order)
  setcolorder(expected_exposed_pd, col_order)
  setorderv(observed_exposed_pd, col_order)
  setcolorder(observed_exposed_pd, col_order)

  expect_equal(observed_exposed_pd, expected_exposed_pd)


  expected_concurrent_vac <- rbindlist(list(
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 5), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 5), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = "pooled",
      V_DOSE = -1,
      RATIO = round(c(8/2, 7/3, 10/2, 10/3, 3/5, 3/10, 5/10), options$ratio_precision),
      CASE_COUNT =    c(1, 0, 1, 1, 0, 0, 1),
      CONTROL_COUNT = c(0, 1, 0, 0, 1, 1, 1)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 8), rep(period_start + 13, 3))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 8), rep(period_start + 19, 3))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = "pooled",
      V_DOSE = c(1, 1, 2, 2, 2, 1, 2, 3, 1, 1, 2, 3),
      RATIO = round(c(8/2, 4/3, 3/3, 9/2, 8/3, 1/5, 1/5, 1/5, 1/10, 1/10, 3/10, 1/10), options$ratio_precision),
      CASE_COUNT =    c(1, 0, 0, 1, 1, 0, 0, 0 + 0, 0, 0, 1, 0),
      CONTROL_COUNT = c(0, 1, 1, 0, 0, 1, 1, 1 + 1, 1, 1, 1, 1)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 6), rep(period_start + 13, 2))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 6), rep(period_start + 19, 2))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = c("PABC", "PABC", "PDEF", "PABC", "PDEF", "PABC", "PABC", "PABC", "PDEF"),
      V_DOSE = -1,
      RATIO = round(c(8/2, 5/3, 2/3, 5/2, 4/3, 3/5, 3/10, 2/10, 3/10), options$ratio_precision),
      CASE_COUNT =    c(1, 0, 0, 1, 1, 0, 0, 0, 1),
      CONTROL_COUNT = c(0, 1, 1, 0, 0, 1, 1, 1, 1)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 9), rep(period_start + 13, 3))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 9), rep(period_start + 19, 3))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = c("PABC", "PABC", "PABC", "PDEF", "PABC", "PDEF", "PABC", "PABC", "PABC", "PABC", "PABC", "PABC", "PDEF"),
      V_DOSE = c(1, 1, 2, 1, 2, 1, 1, 2, 1, 2, 1, 3, 1),
      RATIO = round(c(8/2, 4/3, 1/3, 2/3, 4/2, 4/3, 1/5, 2/5, 1/10, 2/10, 1/10, 1/10, 3/10), options$ratio_precision),
      CASE_COUNT =    c(1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1),
      CONTROL_COUNT = c(0, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 7), rep(period_start + 13, 2))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 7), rep(period_start + 19, 2))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE =     c("abc", "abc",  "def",  "abc",  "def",  "abc", "abc", "ghi",  "abc", "def"),
      V_TYPE = c("PABC", "PABC", "PDEF", "PABC", "PDEF", "PABC", "PABC", "PABC", "PABC", "PDEF"),
      V_DOSE = -1,
      RATIO = round(c(8/2, 5/3, 2/3, 5/2, 4/3, 3/5, 2/10, 1/10, 2/10, 3/10), options$ratio_precision),
      CASE_COUNT =    c(1, 0, 0, 1, 1, 0, 0, 0, 0, 1),
      CONTROL_COUNT = c(0, 1, 1, 0, 0, 1, 1, 1, 1, 1)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 10), rep(period_start + 13, 3))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 10), rep(period_start + 19, 3))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = c("abc",
                  "abc", "abc", "def",     "abc",     "def",     "abc", "abc",     "abc", "abc", "ghi",
                  "abc", "abc", "def"),
      V_TYPE = c("PABC",
                     "PABC", "PABC", "PDEF",      "PABC",     "PDEF",     "PABC", "PABC",     "PABC", "PABC", "PABC",
                     "PABC", "PABC", "PDEF"),
      V_DOSE = c(1,
                 1, 2, 1,     2,     1,     1, 2,     1, 2, 1,
                 1, 3, 1),
      RATIO = round(c(8/2,
                      4/3, 1/3, 2/3,      4/2,     4/3,     1/5, 2/5,     1/10, 1/10, 1/10,
                      1/10, 1/10, 3/10), options$ratio_precision),
      CASE_COUNT =    c(1,
                        0, 0, 0,      1,     1,     0, 0,     0, 0, 0,
                        0, 0, 1),
      CONTROL_COUNT = c(0,
                        1, 1, 1,      0,     0,     1, 1,     1, 1, 1,
                        1, 1, 1)
    )
  ))

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "RATIO", "CASE_COUNT", "CONTROL_COUNT")

  setorderv(expected_concurrent_vac, col_order)
  setcolorder(expected_concurrent_vac, col_order)
  setorderv(observed_concurrent_vac, col_order)
  setcolorder(observed_concurrent_vac, col_order)

  expect_equal(observed_concurrent_vac, expected_concurrent_vac)


  expected_concurrent_unvac <- rbindlist(list(
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 2), rep(period_start + 6, 2), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 2), rep(period_start + 12, 2), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = "pooled",
      V_DOSE = -1,
      RATIO = round(c(2/18,    8/8,
                      7/7,    10/7,
                      5/4
      ), options$ratio_precision),
      CASE_COUNT =    c(1,    1,
                        0,    1 + 1,
                        1),
      CONTROL_COUNT = c(1,    1,
                        2,    0 + 1,
                        0)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 2), rep(period_start + 6, 6), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 2), rep(period_start + 12, 6), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = "pooled",
      V_DOSE = c(1,    1,
                 1, 2,   2,   1, 2, 3,
                 2),
      RATIO = round(c(2/18,    8/8,
                      4/7, 3/7,    9/7,   1/7, 8/7, 1/7,
                      3/4
      ), options$ratio_precision),
      CASE_COUNT =    c(1,    1,
                        0, 0,    1,    0, 1, 0,
                        1),
      CONTROL_COUNT = c(1,    1,
                        2, 2,    0,    1, 1, 1,
                        0)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 2), rep(period_start + 6, 4), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 2), rep(period_start + 12, 4), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = c("PABC",    "PABC",
                     "PABC", "PDEF",     "PABC", "PDEF",
                     "PDEF"),
      V_DOSE = -1,
      RATIO = round(c(2/18,    8/8,
                      5/7, 2/7,   6/7, 4/7,
                      3/4
      ), options$ratio_precision),
      CASE_COUNT =    c(1,    1,
                        0 + 1, 0,    0, 1,
                        1),
      CONTROL_COUNT = c(1,    1,
                        2 + 0, 2,    1, 1,
                        0)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 2), rep(period_start + 6, 7), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 2), rep(period_start + 12, 7), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = c("PABC",    "PABC",
                     "PABC", "PABC", "PDEF",     "PABC",     "PABC", "PABC", "PDEF",
                     "PDEF"),
      V_DOSE = c(1,    1,
                 1, 2, 1,     2,     1, 2, 1,
                 1),
      RATIO = round(c(2/18,    8/8,
                      4/7, 1/7, 2/7,    4/7,   1/7, 5/7, 4/7,
                      3/4
      ), options$ratio_precision),
      CASE_COUNT =    c(1,    1,
                        0, 0, 0,   1,    0, 0, 1,
                        1),
      CONTROL_COUNT = c(1,    1,
                        2, 2, 2,    0,    1, 1, 1,
                        0)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 2), rep(period_start + 6, 4), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 2), rep(period_start + 12, 4), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE =     c("abc",      "abc",
                      "abc", "def",      "abc", "def",
                      "def"),
      V_TYPE = c("PABC",    "PABC",
                     "PABC", "PDEF",     "PABC", "PDEF",
                     "PDEF"),
      V_DOSE = -1,
      RATIO = round(c(2/18,    8/8,
                      5/7, 2/7,   6/7, 4/7,
                      3/4
      ), options$ratio_precision),
      CASE_COUNT =    c(1,    1,
                        0 + 1, 0,    0, 1,
                        1),
      CONTROL_COUNT = c(1,    1,
                        2 + 0, 2,    1, 1,
                        0)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 2), rep(period_start + 6, 7), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 2), rep(period_start + 12, 7), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = c("abc",      "abc",
                  "abc", "abc", "def",     "abc",     "abc", "abc", "def",
                  "def"),
      V_TYPE = c("PABC",    "PABC",
                     "PABC", "PABC", "PDEF",     "PABC",     "PABC", "PABC", "PDEF",
                     "PDEF"),
      V_DOSE = c(1,    1,
                 1, 2, 1,     2,     1, 2, 1,
                 1),
      RATIO = round(c(2/18,    8/8,
                      4/7, 1/7, 2/7,    4/7,   1/7, 5/7, 4/7,
                      3/4
      ), options$ratio_precision),
      CASE_COUNT =    c(1,    1,
                        0, 0, 0,   1,    0, 0, 1,
                        1),
      CONTROL_COUNT = c(1,    1,
                        2, 2, 2,    0,    1, 1, 1,
                        0)
    )
  ))

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "RATIO", "CASE_COUNT", "CONTROL_COUNT")

  setorderv(expected_concurrent_unvac, col_order)
  setcolorder(expected_concurrent_unvac, col_order)
  setorderv(observed_concurrent_unvac, col_order)
  setcolorder(observed_concurrent_unvac, col_order)

  expect_equal(observed_concurrent_unvac, expected_concurrent_unvac)


  expected_desc_vac <- rbindlist(list(
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 1), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 1), rep(period_start + 19, 1))),
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = "pooled",
      V_DOSE = -1,
      COUNT = c(12, 16, 9)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 3), rep(period_start + 13, 3))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 3), rep(period_start + 19, 3))),
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = "pooled",
      V_DOSE = c(1,   1, 2, 3,   1, 2, 3),
      COUNT = c(12,   3, 10, 3,     1, 5, 3)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 2), rep(period_start + 13, 2))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 2), rep(period_start + 19, 2))),
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = c("PABC",    "PABC", "PDEF",     "PABC", "PDEF"),
      V_DOSE = -1,
      COUNT = c(12,   11, 5,     4, 5)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 3), rep(period_start + 13, 4))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 3), rep(period_start + 19, 4))),
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = c("PABC",    "PABC", "PABC", "PDEF",     "PABC", "PABC", "PABC", "PDEF"),
      V_DOSE = c(1,   1, 2, 1,   1, 2, 3, 1),
      COUNT = c(12,   3, 8, 5,     1, 2, 1, 5)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 3), rep(period_start + 13, 2))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 3), rep(period_start + 19, 2))),
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = c("abc",     "abc", "def", "ghi",     "abc", "def"),
      V_TYPE = c("PABC",    "PABC", "PDEF", "PABC",     "PABC", "PDEF"),
      V_DOSE = -1,
      COUNT = c(12,   10, 5, 1,     4, 5)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 4), rep(period_start + 13, 4))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 4), rep(period_start + 19, 4))),
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = c("abc",     "abc", "abc", "def", "ghi",     "abc", "abc", "abc", "def"),
      V_TYPE = c("PABC",    "PABC", "PABC", "PDEF", "PABC",     "PABC", "PABC", "PABC", "PDEF"),
      V_DOSE = c(1,   1, 2, 1, 1,   1, 2, 3, 1),
      COUNT = c(12,   3, 7, 5, 1,   1, 2, 1, 5)
    )
  ))

  col_order <- c("PERIOD_START", "PERIOD_END", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "COUNT")

  setorderv(expected_desc_vac, col_order)
  setcolorder(expected_desc_vac, col_order)
  setorderv(observed_desc_vac, col_order)
  setcolorder(observed_desc_vac, col_order)

  expect_equal(observed_desc_vac, expected_desc_vac)


  expected_desc_outcome <- rbindlist(list(
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 1), rep(period_start + 13, 1), rep(period_start + 20, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 1), rep(period_start + 19, 1), rep(period_start + 26, 1))),
      AGE_GROUP = "80+",
      SEX = "m",
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      COUNT = c(4, 9, 2, 2)
    )
  ))

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "COUNT")

  setorderv(expected_desc_outcome, col_order)
  setcolorder(expected_desc_outcome, col_order)
  setorderv(observed_desc_outcome, col_order)
  setcolorder(observed_desc_outcome, col_order)

  expect_equal(observed_desc_outcome, expected_desc_outcome)


  expected_self_post <- rbindlist(list(
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start + 6, 3), rep(period_start + 13, 1), rep(period_start + 27, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 12, 3), rep(period_start + 19, 1), rep(period_start + 33, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = "pooled",
      V_DOSE = -1,
      WINDOW_RATIO = round(c(1/1, 3/1, 5/1,
                             1/1,
                             1/1
      ), options$ratio_precision),
      CASE_COUNT =    c(1, 0, 0,
                        1 + 0 + 0 + 1 + 1 + 1,
                        0),
      CONTROL_COUNT = c(0, 1, 1,
                        0 + 1 + 1 + 0 + 0 + 0,
                        1)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start + 6, 4), rep(period_start + 13, 1), rep(period_start + 20, 3))),
      PERIOD_END = as.IDate(c(rep(period_start + 12, 4), rep(period_start + 19, 1), rep(period_start + 26, 3))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = "pooled",
      V_DOSE = c(1,      1,     1, 2,
                 2,
                 1, 2, 3
      ),
      WINDOW_RATIO = round(c(1/1,    3/1,    2/1, 3/1,
                             1/1,
                             1/1, 1/1, 1/1
      ), options$ratio_precision),
      CASE_COUNT =    c(1 + 1 + 0,    0,    0, 0,
                        1 + 0 + 1 + 1,
                        0, 0, 0
      ),
      CONTROL_COUNT = c(0 + 0 + 1,    1,    1, 1,
                        0 + 1 + 0 + 0,
                        1, 1, 1
      )
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start + 6, 4), rep(period_start + 13, 2), rep(period_start + 20, 1), rep(period_start + 27, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 12, 4), rep(period_start + 19, 2), rep(period_start + 26, 1), rep(period_start + 33, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = c("PABC", "PABC", "PABC",    "PDEF",
                     "PABC",   "PDEF",
                     "PDEF",
                     "PABC"),
      V_DOSE = -1,
      WINDOW_RATIO = round(c(1/1, 3/1, 2/1,  3/1,
                             1/1,  1/1,
                             1/1,
                             1/1
      ), options$ratio_precision),
      CASE_COUNT =    c(1 + 0, 0, 0,  0,
                        1 + 1 + 0,  1 + 1 + 0,
                        0,
                        0),
      CONTROL_COUNT = c(0 + 1, 1, 1,  1,
                        0 + 0 + 1,  0 + 0 + 1,
                        1,
                        1)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start + 6, 4), rep(period_start + 13, 2), rep(period_start + 20, 3))),
      PERIOD_END = as.IDate(c(rep(period_start + 12, 4), rep(period_start + 19, 2), rep(period_start + 26, 3))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = c("PABC",    "PABC",    "PABC", "PDEF",
                     "PABC",   "PDEF",
                     "PABC", "PABC",  "PDEF"),
      V_DOSE = c(1,      1,     1,  1,
                 2,   1,
                 1, 2,   1),
      WINDOW_RATIO = round(c(1/1,    3/1,    2/1, 3/1,
                             1/1, 1/1,
                             1/1, 1/1, 1/1
      ), options$ratio_precision),
      CASE_COUNT =    c(1 + 1 + 0,    0,    0, 0,
                        1,   0 + 1 + 1,
                        0, 0, 0
      ),
      CONTROL_COUNT = c(0 + 0 + 1,    1,    1, 1,
                        0,  1 + 0 + 0,
                        1, 1, 1
      )
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start + 6, 4), rep(period_start + 13, 2), rep(period_start + 20, 1), rep(period_start + 27, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 12, 4), rep(period_start + 19, 2), rep(period_start + 26, 1), rep(period_start + 33, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE =     c("abc", "abc", "abc",   "def",
                      "abc", "def",
                      "def",
                      "abc"),
      V_TYPE = c("PABC", "PABC", "PABC",    "PDEF",
                     "PABC",   "PDEF",
                     "PDEF",
                     "PABC"),
      V_DOSE = -1,
      WINDOW_RATIO = round(c(1/1, 3/1, 2/1,  3/1,
                             1/1,  1/1,
                             1/1,
                             1/1
      ), options$ratio_precision),
      CASE_COUNT =    c(1 + 0, 0, 0,  0,
                        1 + 1 + 0,  1 + 1 + 0,
                        0,
                        0),
      CONTROL_COUNT = c(0 + 1, 1, 1,  1,
                        0 + 0 + 1,  0 + 0 + 1,
                        1,
                        1)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start + 6, 4), rep(period_start + 13, 2), rep(period_start + 20, 3))),
      PERIOD_END = as.IDate(c(rep(period_start + 12, 4), rep(period_start + 19, 2), rep(period_start + 26, 3))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = c("abc", "abc", "abc",   "def",
                  "abc", "def",
                  "abc", "abc", "def"),
      V_TYPE = c("PABC",    "PABC",    "PABC", "PDEF",
                     "PABC",   "PDEF",
                     "PABC", "PABC",  "PDEF"),
      V_DOSE = c(1,      1,     1,  1,
                 2,   1,
                 1, 2,   1),
      WINDOW_RATIO = round(c(1/1,    3/1,    2/1, 3/1,
                             1/1, 1/1,
                             1/1, 1/1, 1/1
      ), options$ratio_precision),
      CASE_COUNT =    c(1 + 1 + 0,    0,    0, 0,
                        1,   0 + 1 + 1,
                        0, 0, 0
      ),
      CONTROL_COUNT = c(0 + 0 + 1,    1,    1, 1,
                        0,  1 + 0 + 0,
                        1, 1, 1
      )
    )
  ))

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "WINDOW_RATIO", "CASE_COUNT", "CONTROL_COUNT")

  setorderv(expected_self_post, col_order)
  setcolorder(expected_self_post, col_order)
  setorderv(observed_self_post, col_order)
  setcolorder(observed_self_post, col_order)

  expect_equal(observed_self_post, expected_self_post)


  expected_self_pre <- rbindlist(list(
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 1), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 1), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = "pooled",
      V_DOSE = -1,
      WINDOW_RATIO = round(c(1/1,
                             1/1,
                             1/1
      ), options$ratio_precision),
      CASE_COUNT =    c(1 + 0,
                        1,
                        1),
      CONTROL_COUNT = c(0 + 1,
                        0,
                        0)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 2), rep(period_start + 13, 2))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 2), rep(period_start + 19, 2))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = "pooled",
      V_DOSE = c(1,
                 2, 1,
                 3, 2
      ),
      WINDOW_RATIO = round(c(1/1,
                             1/1, 1/1,
                             1/1, 1/1
      ), options$ratio_precision),
      CASE_COUNT =    c(1 + 0,
                        1 + 1 + 0, 1,
                        0, 1),
      CONTROL_COUNT = c(0 + 1,
                        0 + 0 + 1, 0,
                        1, 0)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 2), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 2), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = c("PABC",
                     "PABC", "PDEF",
                     "PDEF"
                     ),
      V_DOSE = -1,
      WINDOW_RATIO = round(c(1/1,
                             1/1, 1/1,
                             1/1
      ), options$ratio_precision),
      CASE_COUNT =    c(1 + 0,
                        1, 1 + 0,
                        1),
      CONTROL_COUNT = c(0 + 1,
                        0, 0 + 1,
                        0)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 3), rep(period_start + 13, 2))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 3), rep(period_start + 19, 2))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = "pooled",
      V_TYPE = c("PABC",
                     "PABC", "PABC", "PDEF",
                     "PABC", "PDEF"
      ),
      V_DOSE = c(1,
                 2, 1, 1,
                 2, 1),
      WINDOW_RATIO = round(c(1/1,
                             1/1, 1/1, 1/1,
                             1/1, 1/1
      ), options$ratio_precision),
      CASE_COUNT =    c(1 + 0,
                        1, 1, 1 + 0,
                        0, 1),
      CONTROL_COUNT = c(0 + 1,
                        0, 0, 0 + 1,
                        1, 0)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 2), rep(period_start + 13, 1))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 2), rep(period_start + 19, 1))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE =     c("abc",
                      "abc", "def",
                      "def"),
      V_TYPE = c("PABC",
                     "PABC", "PDEF",
                     "PDEF"
      ),
      V_DOSE = -1,
      WINDOW_RATIO = round(c(1/1,
                             1/1, 1/1,
                             1/1
      ), options$ratio_precision),
      CASE_COUNT =    c(1 + 0,
                        1, 1 + 0,
                        1),
      CONTROL_COUNT = c(0 + 1,
                        0, 0 + 1,
                        0)
    ),
    data.table(
      PERIOD_START = as.IDate(c(rep(period_start - 1, 1), rep(period_start + 6, 3), rep(period_start + 13, 2))),
      PERIOD_END = as.IDate(c(rep(period_start + 5, 1), rep(period_start + 12, 3), rep(period_start + 19, 2))),
      ENCOUNTER_TYPE = 1,
      AESI = "ADEM",
      AGE_GROUP = "80+",
      SEX = "m",
      V_SUBTYPE = c("abc",
                  "abc", "abc", "def",
                  "abc", "def"),
      V_TYPE = c("PABC",
                     "PABC", "PABC", "PDEF",
                     "PABC", "PDEF"
      ),
      V_DOSE = c(1,
                 2, 1, 1,
                 2, 1),
      WINDOW_RATIO = round(c(1/1,
                             1/1, 1/1, 1/1,
                             1/1, 1/1
      ), options$ratio_precision),
      CASE_COUNT =    c(1 + 0,
                        1, 1, 1 + 0,
                        0, 1),
      CONTROL_COUNT = c(0 + 1,
                        0, 0, 0 + 1,
                        1, 0)
    )
  ))

  col_order <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "WINDOW_RATIO", "CASE_COUNT", "CONTROL_COUNT")

  setorderv(expected_self_pre, col_order)
  setcolorder(expected_self_pre, col_order)
  setorderv(observed_self_pre, col_order)
  setcolorder(observed_self_pre, col_order)

  expect_equal(observed_self_pre, expected_self_pre)

  unlink("RCA_COVID_TEST_SITE_20201231-20210203", recursive = TRUE)

})


# ==============================================================================
# Golden master regression tests
#
# These tests compare aggregate_data() outputs against pre-generated reference
# files stored in tests/testthat/fixtures/golden_outputs/. Any change in output
# values will cause a failure, making unintended regressions visible.
#
# To intentionally update the golden files after a deliberate code change:
#   Rscript tests/testthat/fixtures/generate_golden.R
# then commit the updated fixture files alongside the code change.
# ==============================================================================

golden_inputs_dir  <- testthat::test_path("fixtures/golden_inputs")
golden_outputs_dir <- testthat::test_path("fixtures/golden_outputs")

test_that("golden master: regular aggregation outputs are unchanged", {
  skip_if_not(
    file.exists(file.path(golden_inputs_dir, "person_data.parquet")),
    "Golden input fixtures not found — run tests/testthat/fixtures/generate_golden.R"
  )

  options_obj <- read_options_file(testthat::test_path("fixtures"))

  pd <- as.data.table(arrow::read_parquet(file.path(golden_inputs_dir, "person_data.parquet")))
  vd <- as.data.table(arrow::read_parquet(file.path(golden_inputs_dir, "vaccination_data.parquet")))
  od <- as.data.table(arrow::read_parquet(file.path(golden_inputs_dir, "outcome_data.parquet")))

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  pd1 <- copy(pd); vd1 <- copy(vd); od1 <- copy(od)
  aggregate_data(
    pd1, vd1, od1,
    cycle_start_date      = as.Date("2022-01-01"),
    cycle_end_date        = as.Date("2022-01-31"),
    site_code             = "TEST",
    design_selection      = NULL,
    options_file_location = NULL,
    options               = options_obj,
    restore_input_data    = FALSE,
    working_directory     = tmp,
    skip_user_prompts     = TRUE,
    output_format         = "parquet"
  )

  result_folder <- list.files(tmp, full.names = TRUE)[1]
  golden_files  <- list.files(golden_outputs_dir, pattern = "\\.parquet$")
  regular_golden_files <- golden_files[!golden_files %in% c("data_historic_cases.parquet",
                                                             "data_historic_person_days.parquet")]

  for (fname in sort(regular_golden_files)) {
    actual   <- as.data.table(arrow::read_parquet(file.path(result_folder,    fname)))
    expected <- as.data.table(arrow::read_parquet(file.path(golden_outputs_dir, fname)))
    sort_cols <- names(expected)
    setorderv(actual,   sort_cols)
    setorderv(expected, sort_cols)
    expect_equal(actual, expected, label = fname)
  }
})

test_that("golden master: historical aggregation outputs are unchanged", {
  skip_if_not(
    file.exists(file.path(golden_inputs_dir, "person_data.parquet")),
    "Golden input fixtures not found — run tests/testthat/fixtures/generate_golden.R"
  )

  options_obj <- read_options_file(testthat::test_path("fixtures"))

  pd <- as.data.table(arrow::read_parquet(file.path(golden_inputs_dir, "person_data.parquet")))
  od <- as.data.table(arrow::read_parquet(file.path(golden_inputs_dir, "outcome_data.parquet")))

  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))

  pd2 <- copy(pd); od2 <- copy(od)
  aggregate_data(
    pd2, NULL, od2,
    cycle_start_date      = as.Date("2022-01-01"),
    cycle_end_date        = as.Date("2022-01-31"),
    site_code             = "TEST",
    options_file_location = NULL,
    options               = options_obj,
    restore_input_data    = FALSE,
    working_directory     = tmp,
    skip_user_prompts     = TRUE,
    output_format         = "parquet"
  )

  result_folder <- list.files(tmp, full.names = TRUE)[1]

  for (fname in c("data_historic_cases.parquet", "data_historic_person_days.parquet")) {
    actual   <- as.data.table(arrow::read_parquet(file.path(result_folder,    fname)))
    expected <- as.data.table(arrow::read_parquet(file.path(golden_outputs_dir, fname)))
    sort_cols <- names(expected)
    setorderv(actual,   sort_cols)
    setorderv(expected, sort_cols)
    expect_equal(actual, expected, label = fname)
  }
})
## Test without washout windows
