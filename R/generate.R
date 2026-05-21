
#' Generate synthetic input datasets
#'
#' @description
#' This function is a simplified wrapper around three more complex functions which generate synthetic
#' person, vaccination, and AESI outcome data (see `generate_person_data()`, `generate_vaccination_data()`,
#' and `generate_outcome_data()` respectively). Importantly, it also converts the date columns of all three datasets
#' from Date objects to the 8-digit number format required.
#'
#' For greater control over the datasets produced, use the 3 specific functions listed above.
#'
#' @param pop_size Number of people in population (defines size of person_data)
#' @param study_start_date Date object defining start of vaccination campaign and study
#' @param study_end_date Date object defining end of study
#' @param save_data Boolean whether or not to save the data, as well as return it (default is TRUE, will save)
#' @param save_location Location where to save the data, if `save_data = TRUE`. Defaults to current working directory
#'
#' @return Named list containing 3 datasets: "person_data", "vaccination_data", and "outcome_data", matching their respective templates.
#'
#' @examples
#' \dontrun{
#' # Generate synthetic data for 1000 people (without saving to disk)
#' data <- generate_synthetic_data(
#'   pop_size = 1000,
#'   study_start_date = as.Date("2021-01-01"),
#'   study_end_date = as.Date("2023-01-01"),
#'   save_data = FALSE
#' )
#'
#' head(data$person_data)
#' head(data$vaccination_data)
#' head(data$outcome_data)
#' }
#'
#' @export
generate_synthetic_data <- function(
    pop_size = 1000000,
    study_start_date = as.Date('2021/01/01'),
    study_end_date = as.Date('2024/01/01'),
    save_data = TRUE,
    save_location = getwd()) {

  person_data <- generate_person_data(pop_size, study_start_date, study_end_date)
  vaccination_data <- generate_vaccination_data(person_data, study_start_date, study_end_date)
  outcome_data <- generate_outcome_data(person_data, vaccination_data, study_start_date, study_end_date)

  date_cols <- names(person_data)[sapply(person_data, function(x) inherits(x, "Date"))]
  person_data[
    , c(date_cols) := lapply(.SD, convert_date_to_number),
    .SDcols = date_cols
  ]

  date_cols <- names(vaccination_data)[sapply(vaccination_data, function(x) inherits(x, "Date"))]
  vaccination_data[
    , c(date_cols) := lapply(.SD, convert_date_to_number),
    .SDcols = date_cols
  ]

  date_cols <- names(outcome_data)[sapply(outcome_data, function(x) inherits(x, "Date"))]
  outcome_data[
    , c(date_cols) := lapply(.SD, convert_date_to_number),
    .SDcols = date_cols
  ]

  if (save_data) {
    fwrite(person_data, file = paste0(save_location, "/person_data.csv"))
    fwrite(vaccination_data, file = paste0(save_location, "/vaccination_data.csv"))
    fwrite(outcome_data, file = paste0(save_location, "/outcome_data.csv"))
  }

  invisible(list(
    "person_data" = person_data,
    "vaccination_data" = vaccination_data,
    "outcome_data" = outcome_data
  ))
}

#' Generate synthetic person data
#'
#' @description
#' Generates a cohort of size `pop_size`, with dates of birth sampled uniform randomly
#' from between two specified dates (default is between 100 years before the study start and the study end).
#' Age at death is based on a beta distribution with specified parameters, which defines date of death.
#' Anyone who dies before the study start is removed, and the whole process is repeated until `pop_size` is reached.
#'
#' @param pop_size Number of people in population
#' @param study_start_date Date object defining start of the study
#' @param study_end_date Date object defining end of the study
#' @param dob_start_date Date object defining earliest possible date of birth in cohort (defaults to 100 years before start of study)
#' @param dob_end_date Date object defining latest possible date of birth in cohort (defaults to study end date)
#' @param death_age_beta1 Shape parameter 1 for beta distribution of age at death
#' @param death_age_beta2 Shape parameter 2 for beta distribution of age at death
#' @param death_age_max Maximum age at death
#' @param sexes Options for biological sex at birth (drawn uniformly)
#'
#' @return A data.table matching the person data template, with `pop_size` rows (BUT date columns are date objects, not in 8-digit number format)
generate_person_data <- function(
    pop_size = 1000000,
    study_start_date = as.Date('2021/01/01'),
    study_end_date = as.Date('2024/01/01'),
    dob_start_date = study_start_date - 100*365.25,
    dob_end_date = study_end_date,
    death_age_beta1 = 10,
    death_age_beta2 = 2,
    death_age_max = 100,
    sexes = c("m", "f")){

  person_data <- data.table()

  # Generate in batches, discarding anyone who dies before the study start.
  # Because a proportion of generated individuals are removed each iteration,
  # the loop repeats until exactly pop_size survivors remain.
  while (nrow(person_data) < pop_size) {

    current_size <- nrow(person_data)

    person_data <- dplyr::bind_rows(
      person_data,
      data.table(
        PID = (current_size + 1):(current_size + pop_size),
        DOB = sample(seq(dob_start_date, dob_end_date, by = "day"), pop_size, replace = TRUE),
        SEX = sample(sexes, pop_size, replace = TRUE)
      )[
        , ENROL_DATE := DOB
      ][
        , CENSOR_DATE := DOB + round(rbeta(pop_size, death_age_beta1, death_age_beta2)*death_age_max*365.25)
      ][
        , CENSOR_DATE := fifelse(CENSOR_DATE > study_end_date, NA, CENSOR_DATE)
      ][
        , CENSOR_TYPE := fifelse(is.na(CENSOR_DATE), NA_character_, "dead")
      ][
        is.na(CENSOR_DATE) | CENSOR_DATE > study_start_date
      ]
    )[, PID := 1:.N]
  }

  date_cols <- names(person_data)[sapply(person_data, function(x) inherits(x, "Date"))]

  person_data[1:pop_size][
    , PID := stringr::str_c("p", PID)
  ]
}

#' Generate synthetic vaccination data
#'
#' @description
#' Generates vaccination data for those people contained in `person_data`. The number of
#' vaccinations each person receives is drawn from a poisson distribution with specified mean
#' (can be 0). After the number of vaccinations is drawn, vaccination dates and brands are
#' uniform randomly sampled.
#'
#' @param person_data Person data input data.table (with date columns as Date objects, not 8-digit number format)
#' @param campaign_start_date Date object defining start of vaccination campaign (generally same as study start date)
#' @param campaign_end_date Date object defining end of vaccination campaign
#' @param vaccine_codes Character vector of options for vaccine brand codes
#' @param mean_doses Mean number of doses per person during the campaign, as parameter for Poisson distribution
#'
#' @return A data.table matching the vaccination data template (BUT date columns are date objects, not in 8-digit number format)
generate_vaccination_data <- function(
    person_data,
    campaign_start_date = as.Date('2021/01/01'),
    campaign_end_date = as.Date('2024/01/01'),
    vaccine_codes = c("ABC", "DEF", "GHI"),
    mean_doses = 3) {

  n_doses <- rpois(nrow(person_data), mean_doses)
  campaign_length <- as.numeric(campaign_end_date - campaign_start_date, "days")
  vaccination_date <- round(runif(sum(n_doses), min = 0, max = campaign_length))
  vaccine_code <- sample(vaccine_codes, sum(n_doses), replace = TRUE)

  merge(
    data.table(
      PID = rep(person_data$PID, n_doses),
      V_SUBTYPE = vaccine_code,
      V_DATE = campaign_start_date + vaccination_date
    ),
    person_data,
    by = "PID")[
      , CENSOR_DATE := fifelse(is.na(CENSOR_DATE), campaign_end_date, CENSOR_DATE)
    ][
      V_DATE %between% list(ENROL_DATE, CENSOR_DATE)
    ][
      , .(PID, V_DATE, V_SUBTYPE)
    ][
      order(PID, V_DATE)
    ]
}

#' Generate synthetic AESI outcome data
#'
#' @description
#' Generates synthetic AESI outcome data for those people contained in `person_data` with
#' vaccinations given in `vaccination_data`. It uses the vaccination data and specified `RR` to
#' appropriately adjust outcome rates during exposed periods.
#'
#' For each person, the amount of time spent in risk (exposed) periods and unexposed periods is calculated.
#' Then, the number of AESI outcomes experienced by each person is drawn from a poisson distribution,
#' for their total exposed and non-exposed periods independently, with the rate parameter being scaled by `RR`
#' for exposed cases. Finally, the cases that occur are uniform randomly assigned a date during the relevant period
#' (exposed cases to exposed period of time, non-exposed cases to non-exposed period of time), and encounter type is
#' randomly assigned from 1-4.
#'
#' @inheritParams generate_person_data
#' @inheritParams generate_vaccination_data
#' @param vaccination_data Vaccination data input data.table (with date columns as Date objects, not 8-digit number format)
#' @param outcomes Character vector of options for AESI codes.
#' @param outcome_daily_rates Numeric vector of daily outcome rates, with same length and in same order as `outcomes` vector.
#' @param RR Numeric value or vector defining increased risk due to vaccination. If a vector, must be same length and order as `outcomes`.
#' @param risk_window Length of risk window post-vaccination. Currently only accepts a single number (same for all outcomes),
#' and risk window is assumed to start on day of vaccination.
#'
#' @return A data.table matching the AESI data template (BUT date columns are date objects, not in 8-digit number format)
generate_outcome_data <- function(
    person_data, vaccination_data,
    study_start_date = as.Date('2021/01/01'),
    study_end_date = as.Date('2024/01/01'),
    outcomes = c("ADEM", "MYO", "ST"),
    outcome_daily_rates = rep(0.000001, length(outcomes)),
    RR = 5,
    risk_window = 42) {

  exposures <- person_data[
    vaccination_data, on = .(PID)
  ][
    , ':=' (risk_start = V_DATE,
            risk_end = V_DATE + risk_window)
  ][
    order(PID, V_DATE)
  ][
    , ':=' (risk_start_next = shift(risk_start, type = "lead"),
            PID_next = shift(PID, type = "lead"))
  ][
    # Truncate overlapping risk windows: if this dose's risk window extends into the next
    # dose's risk window for the same person, cap it one day before the next window starts.
    , risk_end := fifelse(!is.na(PID_next) & (PID_next == PID & risk_end >= risk_start_next),
                          pmax(risk_start, risk_start_next - 1, na.rm = TRUE), risk_end)
  ][
    , ':=' (risk_start_next = NULL, PID_next = NULL)
  ][
    , ':=' (risk_start = pmin(risk_start, CENSOR_DATE, study_end_date, na.rm = TRUE),
            risk_end = pmin(risk_end, CENSOR_DATE, study_end_date, na.rm = TRUE))
  ][
    , risk_length := as.numeric(risk_end - risk_start, "days")
  ]

  n_outcomes_risk <- lapply(outcome_daily_rates*(RR - 1), function(x) rpois(nrow(exposures), exposures$risk_length*x))

  exposures[
    , (outcomes) := n_outcomes_risk
  ]

  row_filter <- rowSums(exposures[, ..outcomes] >= 1) > 0

  exposed_outcomes <- melt(exposures[row_filter],
                           measure.vars = outcomes,
                           variable.name = "AESI",
                           value.name = "count")[
                             rep(1:.N, count)
                           ][
                             , EVENT_DATE := V_DATE + round(runif(.N, 0, risk_length))
                           ][
                             , ENCOUNTER_TYPE := round(runif(.N, min = 1, max = 2))
                           ]

  unexposures <- merge(person_data,
                       exposures[, .(risk_length = sum(risk_length)),
                                 by = "PID"],
                       by = "PID", all = TRUE
  )[
    , risk_length := pmax(risk_length, 0, na.rm = TRUE)
  ][
    , nonrisk_length := as.numeric(pmin(CENSOR_DATE, study_end_date, na.rm = TRUE) - ENROL_DATE, "days") - risk_length
  ]

  n_outcomes_nonrisk <- lapply(outcome_daily_rates, function(x) rpois(nrow(unexposures), unexposures$nonrisk_length*x))

  unexposures[
    , (outcomes) := n_outcomes_nonrisk
  ]

  row_filter <- rowSums(unexposures[, ..outcomes] >= 1) > 0

  unexposed_outcomes <- melt(unexposures[row_filter],
                             measure.vars = outcomes,
                             variable.name = "AESI",
                             value.name = "count")[
                               rep(1:.N, count)
                             ][
                               , EVENT_DATE := ENROL_DATE + round(runif(.N, 0, nonrisk_length))
                             ][
                               , ENCOUNTER_TYPE := round(runif(.N, min = 1, max = 4))
                             ]

  return(
    dplyr::bind_rows(
      exposed_outcomes[, .(PID, AESI, EVENT_DATE, ENCOUNTER_TYPE)],
      unexposed_outcomes[, .(PID, AESI, EVENT_DATE, ENCOUNTER_TYPE)]
    )
  )

}
