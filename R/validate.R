
#' Validate input datasets before aggregating
#'
#' @description
#' Checks input datasets for any obvious issues, including incorrect column names or types, duplicated data, date ranges etc.
#' This function is run automatically from within the `aggregate_data()` function, therefore does not need to be run manually
#' (however it is possible to do so if you wish to).
#'
#' If validating historical input data, set `vaccination_data = NULL`.
#'
#' @inheritParams aggregate_data
#' @param split_size Input data will be automatically split into pieces of this size (default 500,000). If memory is an issue, lower the split_size.
#'
#' @return A character vector of issues found. If no issues are found, the vector will be empty.
#'
#' @examples
#' \dontrun{
#' # Generate synthetic data and validate it
#' data <- generate_synthetic_data(pop_size = 1000, save_data = FALSE)
#'
#' issues <- validate_input_data(
#'   person_data = data$person_data,
#'   vaccination_data = data$vaccination_data,
#'   outcome_data = data$outcome_data,
#'   cycle_start_date = 20210101,
#'   cycle_end_date = 20230101,
#'   options_file_location = "path/to/options",
#'   skip_user_prompts = TRUE
#' )
#'
#' if (length(issues) == 0) message("No issues found!")
#' }
#'
#' @export
validate_input_data <- function(person_data = NULL, vaccination_data, outcome_data,
                                cycle_start_date, cycle_end_date,
                                options_file_location = getwd(),
                                options = NULL,
                                split_size = 500000,
                                skip_user_prompts = FALSE,
                                patient_data = NULL){

  if (!is.null(patient_data)) {
    warning("The `patient_data` argument is deprecated. Use `person_data` instead.", call. = FALSE)
    if (is.null(person_data)) person_data <- patient_data
  }
  if (is.null(person_data)) stop("You must provide person data.")

  if (!is.null(options) & !is.null(options_file_location)) {
    stop("You have provided both an options object and an options file location - only provide one.")
  }
  if (is.null(options) & is.null(options_file_location)) {
    stop("You must provide either an options object or an options file location.")
  }

  if (!is.null(options)) {
    check_options_object(options)
  }

  if (is.null(options)) {
    options <- read_options_file(options_file_location)
  }

  cycle_start_date <- check_date_format(cycle_start_date)
  cycle_end_date <- check_date_format(cycle_end_date)

  issues <- c()

  if (cycle_start_date >= cycle_end_date) {
    issues <- c(issues, "The cycle end date must be after the cycle start date.")
  }

  if (!is.data.table(person_data)) {
    setDT(person_data)
  }
  if (!is.data.table(outcome_data)) {
    setDT(outcome_data)
  }

  if (!is.null(vaccination_data) && !is.data.table(vaccination_data)) {
    setDT(vaccination_data)
  }

  # Handle deprecated V_BRAND column name in vaccination_data. Uses data.table reference
  # semantics so the rename is visible in the calling environment without reassignment.
  if (!is.null(vaccination_data) &&
      "V_BRAND" %in% names(vaccination_data) && !"V_SUBTYPE" %in% names(vaccination_data)) {
    warning("The `V_BRAND` column in vaccination_data is deprecated. Please rename to `V_SUBTYPE`.", call. = FALSE)
    setnames(vaccination_data, "V_BRAND", "V_SUBTYPE")
  }

  lookback_length <- if (!is.null(options$lookback_length)) options$lookback_length else 2
  # The check triggers when less than 75% of the required lookback period is covered.
  # This allows a small shortfall (e.g. data starting a few weeks late) without raising
  # a warning, while still flagging genuinely insufficient lookback coverage.
  lookback_threshold <- lookback_length * 0.75

  issues <- c(issues, validate_person_data(person_data, cycle_start_date, cycle_end_date))
  issues <- c(issues, validate_outcome_data(outcome_data, person_data, options, split_size))

  if (!is.null(vaccination_data)) {

    issues <- c(issues, validate_vaccination_data(vaccination_data, person_data, options, split_size))

    # Return early if structural issues were found. The date-range and lookback checks below
    # require that column names, types, and cross-dataset PIDs are already confirmed valid.
    if (length(issues) > 0) {
      return(issues)
    }

    if (as.numeric(convert_number_to_date(min(vaccination_data$V_DATE)) - convert_number_to_date(min(outcome_data$EVENT_DATE)), "days")/365.25 < lookback_threshold) {
      if (!skip_user_prompts) {
        lookback_check <- menu(
          choices = c("Continue (lookback data is included or not available)", "Stop (lookback data is missing)"),
          title = paste0("USER INPUT REQUIRED: Outcome data should include a lookback of ", lookback_length,
                         " year(s) before the start of the study period. Your outcome data does not contain any outcomes at least ",
                         round(lookback_threshold * 12), " months before the start of the study.",
                         " If this is a characteristic of your data, or you are unable to look back any further, you may continue.",
                         " Otherwise, please stop and include lookback data.")
        )
        if (lookback_check == 1) {
          lookback_start_date <- readline(prompt = "Please input the start date of the lookback period used (in yyyy-mm-dd format):")
          issues <- c(issues, paste0("Lookback checked and start date provided: ", lookback_start_date))
        } else if (lookback_check == 2) {
          issues <- c(issues, paste0("Outcome data should include a lookback of ", lookback_length,
                                     " year(s) before the start of the study period. To stop this error, ensure there is at least one outcome at least ",
                                     round(lookback_threshold * 12), " months before the first vaccination was given."))
        }
      } else {
        warning(paste0(
          "Lookback threshold not met: outcome data does not contain outcomes at least ",
          round(lookback_threshold * 12), " months before the first vaccination. ",
          "Continuing because skip_user_prompts = TRUE. ",
          "Review lookback coverage before relying on these results."
        ))
        issues <- c(issues, paste0(
          "Lookback warning (automated run): outcome data does not contain outcomes at least ",
          round(lookback_threshold * 12), " months before the first vaccination. ",
          "Review lookback coverage before relying on these results."
        ))
      }
    }

  } else {

    # Same early-return guard for the historical (no vaccination data) path.
    if (length(issues) > 0) {
      return(issues)
    }

    # For historical aggregation the lookback is measured from the cycle start date
    # rather than from the first vaccination date.
    if (as.numeric(cycle_start_date - convert_number_to_date(min(outcome_data$EVENT_DATE)), "days")/365.25 < lookback_threshold) {
      if (!skip_user_prompts) {
        lookback_check <- menu(
          choices = c("Continue (lookback data is included or not available)", "Stop (lookback data is missing)"),
          title = paste0("USER INPUT REQUIRED: Outcome data should include a lookback of ", lookback_length,
                         " year(s) before the start of the historical period. Your outcome data does not contain any outcomes at least ",
                         round(lookback_threshold * 12), " months before the start of the historical period.",
                         " If this is a characteristic of your data, or you are unable to look back any further, you may continue.",
                         " Otherwise, please stop and include lookback data.")
        )
        if (lookback_check == 1) {
          lookback_start_date <- readline(prompt = "Please input the start date of the lookback period used (in yyyy-mm-dd format):")
          issues <- c(issues, paste0("Lookback checked and start date provided: ", lookback_start_date))
        } else if (lookback_check == 2) {
          issues <- c(issues, paste0("Outcome data should include a lookback of ", lookback_length,
                                     " year(s) before the start of the historical period. To stop this error, ensure there is at least one outcome at least ",
                                     round(lookback_threshold * 12), " months before the start of the historical period."))
        }
      } else {
        warning(paste0(
          "Lookback threshold not met: outcome data does not contain outcomes at least ",
          round(lookback_threshold * 12), " months before the start of the historical period. ",
          "Continuing because skip_user_prompts = TRUE. ",
          "Review lookback coverage before relying on these results."
        ))
        issues <- c(issues, paste0(
          "Lookback warning (automated run): outcome data does not contain outcomes at least ",
          round(lookback_threshold * 12), " months before the start of the historical period. ",
          "Review lookback coverage before relying on these results."
        ))
      }
    }
  }

  return(issues)

}

#' Validate person data
#'
#' @inheritParams validate_input_data
#'
#' @return A character vector of issues found. If no issues are found, the vector will be empty.
validate_person_data <- function(person_data, cycle_start_date, cycle_end_date) {

  issues <- c()

  ## Check date formats, and other column formats, names etc

  # All columns exist and are named correctly
  issues <- c(issues,
              validate_column_names(c("PID", "DOB", "SEX", "ENROL_DATE", "CENSOR_DATE", "CENSOR_TYPE"), names(person_data), "Person data"))

  # Column types are correct
  issues <- c(issues,
              validate_column_types(person_data,
                                    list("PID" = "character", "DOB" = c("integer", "numeric"), "SEX" = "character",
                                      "ENROL_DATE" = c("integer", "numeric"), "CENSOR_DATE" = c("integer", "numeric"), "CENSOR_TYPE" = "character"),
                                    "Person data"))

  if (length(issues) > 0) {
    return(issues)
  }

  # PID are unique and complete
  if (any(duplicated(person_data[, .(PID)]))) {
    issues <- c(issues, "Person data contains duplicated PIDs. Each PID should only appear once.")
  }
  if (any(is.na(person_data$PID))) {
    issues <- c(issues, "Person data has missing PIDs.")
  }

  # Date columns
  if (any(is.na(person_data$DOB))) {
    issues <- c(issues, "The DOB column in person data has missing values.")
  } else if (!(inherits(person_data$DOB, "integer") | inherits(person_data$DOB, "numeric"))) {
    issues <- c(issues, "The DOB column in person data contains values that are not numeric.")
  } else if (!all(floor(log10(person_data$DOB)) + 1 == 8) | any(person_data$DOB %% 1 > 0)) {
    issues <- c(issues, "The DOB column in person data contains values that are not integers with exactly 8 digits.")
  }

  if (!(inherits(person_data$ENROL_DATE, "integer") | inherits(person_data$ENROL_DATE, "numeric"))){
    issues <- c(issues, "The ENROL_DATE column in person data contains values that are not numeric.")
  } else if (!all(is.na(person_data$ENROL_DATE) | floor(log10(person_data$ENROL_DATE)) + 1 == 8) |
      any(!is.na(person_data$ENROL_DATE) & person_data$ENROL_DATE %% 1 > 0)) {
    issues <- c(issues, "The ENROL_DATE column in person data contains values that are not integers with exactly 8 digits or NA.")
  }

  if (!(inherits(person_data$CENSOR_DATE, "integer") | inherits(person_data$CENSOR_DATE, "numeric"))){
    issues <- c(issues, "The CENSOR_DATE column in person data contains values that are not numeric.")
  } else if (!all(is.na(person_data$CENSOR_DATE) | floor(log10(person_data$CENSOR_DATE)) + 1 == 8) |
      any(!is.na(person_data$CENSOR_DATE) & person_data$CENSOR_DATE %% 1 > 0)) {
    issues <- c(issues, "The CENSOR_DATE column in person data contains values that are not integers with exactly 8 digits or NA.")
  }

  if ((inherits(person_data$DOB, "integer") | inherits(person_data$DOB, "numeric")) &
      (inherits(person_data$ENROL_DATE, "integer") | inherits(person_data$ENROL_DATE, "numeric")) &
      !all(is.na(person_data$ENROL_DATE) | person_data$ENROL_DATE >= person_data$DOB)) {
    issues <- c(issues, "The ENROL_DATE column in person data contains values that occur before the date of birth.")
  }

  if ((inherits(person_data$DOB, "integer") | inherits(person_data$DOB, "numeric")) &
      (inherits(person_data$ENROL_DATE, "integer") | inherits(person_data$ENROL_DATE, "numeric")) &
      (inherits(person_data$CENSOR_DATE, "integer") | inherits(person_data$CENSOR_DATE, "numeric")) &
      !all(is.na(person_data$CENSOR_DATE) | (person_data$CENSOR_DATE >= person_data$DOB & (is.na(person_data$ENROL_DATE) | person_data$CENSOR_DATE >= person_data$ENROL_DATE)))) {
    issues <- c(issues, "The CENSOR_DATE column in person data contains values that occur before the date of enrolment or birth.")
  }

  if ((inherits(person_data$CENSOR_DATE, "integer") | inherits(person_data$CENSOR_DATE, "numeric")) &
      (inherits(person_data$ENROL_DATE, "integer") | inherits(person_data$ENROL_DATE, "numeric")) &
      (any(!is.na(person_data$CENSOR_DATE) & person_data$CENSOR_DATE < convert_date_to_number(cycle_start_date)) |
      any(!is.na(person_data$ENROL_DATE) & person_data$ENROL_DATE > convert_date_to_number(cycle_end_date)))) {
    issues <- c(issues, "Your person data contains individuals that are not in the cohort for this cycle. Please remove anyone who exited before cycle start, or enters after cycle end.")
  }

  # Censor types are from expected list
  if (any(!is.na(person_data$CENSOR_TYPE) & !person_data$CENSOR_TYPE %in% c("", "missing", "dead", "travelled", "emigrated"))) {
    issues <- c(issues, "Your person data contains CENSOR_TYPEs that are not recognised. Please refer to the data dictionary.")
  }

  if (any(is.na(person_data$SEX) | !person_data$SEX %in% c("m", "f"))) {
    issues <- c(issues, "Your person data contains SEXs that are either missing or not recognised. Please refer to the data dictionary.")
  }

  return(issues)
}

#' Validate vaccination data
#'
#' @inheritParams validate_input_data
#'
#' @return A character vector of issues found. If no issues are found, the vector will be empty.
validate_vaccination_data <- function(vaccination_data, person_data, options, split_size){

  issues <- c()

  ## Check date formats, and other column formats, names etc

  # All columns exist and are named correctly
  issues <- c(issues,
              validate_column_names(c("PID", "V_DATE", "V_SUBTYPE"), names(vaccination_data), "Vaccination data"))

  # Column types are correct
  issues <- c(issues,
              validate_column_types(vaccination_data,
                                    list("PID" = "character", "V_DATE" = c("integer", "numeric"), "V_SUBTYPE" = "character"),
                                    "Vaccination data"))

  if (length(issues) > 0) {
    return(issues)
  }

  # PID are unique and complete
  if (any(!vaccination_data$PID %in% person_data$PID)) {
    issues <- c(issues, "Vaccination data contains PIDs that are not in the person dataset.")
  }

  # Date columns
  if (any(is.na(vaccination_data$V_DATE))) {
    issues <- c(issues, "The V_DATE column in vaccination data has missing values.")
  } else if (!(inherits(vaccination_data$V_DATE, "integer") | inherits(vaccination_data$V_DATE, "numeric"))){
    issues <- c(issues, "The V_DATE column in vaccination data contains values that are not numeric.")
  } else if (!all(floor(log10(vaccination_data$V_DATE)) + 1 == 8) | any(vaccination_data$V_DATE %% 1 > 0)) {
    issues <- c(issues, "The V_DATE column in vaccination data contains values that are not integers with exactly 8 digits.")
  }

  # Vaccine subtypes are from expected list
  if (any(!vaccination_data$V_SUBTYPE %in% options$vaccine_info$V_SUBTYPE)) {
    issues <- c(issues, "Your vaccination data contains vaccine subtypes that are not recognised. Please refer to the vaccine_info table in the options file.")
  }

  ## Check date ranges
  issues <- c(issues,
              validate_vaccination_dates(person_data, vaccination_data, split_size))

  return(issues)

}

#' Validate AESI outcome data
#'
#' @inheritParams validate_input_data
#'
#' @return A character vector of issues found. If no issues are found, the vector will be empty.
validate_outcome_data <- function(outcome_data, person_data, options, split_size){

  issues <- c()

  ## Check date formats, and other column formats, names etc

  # All columns exist and are named correctly
  issues <- c(issues,
              validate_column_names(c("PID", "AESI", "EVENT_DATE", "ENCOUNTER_TYPE"), names(outcome_data), "Outcome data"))

  # Column types are correct
  issues <- c(issues,
              validate_column_types(outcome_data,
                                    list("PID" = "character", "AESI" = "character", "EVENT_DATE" = c("integer", "numeric"), "ENCOUNTER_TYPE" = c("integer", "numeric")),
                                    "Outcome data"))

  if (length(issues) > 0) {
    return(issues)
  }

  # PID are unique and complete
  if (any(!outcome_data$PID %in% person_data$PID)) {
    issues <- c(issues, "Outcome data contains PIDs that are not in the person dataset.")
  }

  # Date columns
  if (any(is.na(outcome_data$EVENT_DATE))) {
    issues <- c(issues, "The EVENT_DATE column in outcome data has missing values.")
  } else if (!(inherits(outcome_data$EVENT_DATE, "integer") | inherits(outcome_data$EVENT_DATE, "numeric"))){
    issues <- c(issues, "The EVENT_DATE column in outcome data contains values that are not numeric.")
  } else if (!all(floor(log10(outcome_data$EVENT_DATE)) + 1 == 8) | any(outcome_data$EVENT_DATE %% 1 > 0)) {
    issues <- c(issues, "The EVENT_DATE column in outcome data contains values that are not integers with exactly 8 digits.")
  }

  # AESI are from expected list
  if (any(!outcome_data$AESI %in% options$outcome_info$AESI)) {
    issues <- c(issues, "Your outcome data contains AESI that are not recognised. Please refer to the outcome_info table in the options file.")
  }

  ## Check date ranges
  issues <- c(issues,
              validate_outcome_dates(person_data, outcome_data, split_size))

  return(issues)

}

#' Validate names of input data columns
#'
#' @param expected Character vector of expected column names
#' @param actual Character vector of actual column names
#' @param dataset Name of dataset (used in outputted message only)
#'
#' @return A string describing the issue found, or NULL if no issues is found.
validate_column_names <- function(expected, actual, dataset) {

  expected <- sort(expected)
  actual <- sort(actual)

  if (length(actual) > length(expected)) {
    extra_cols <- actual[which(!actual %in% expected)]
    return(paste0(dataset, " contains more columns than expected. The following columns are not expected: ", stringr::str_c(extra_cols, collapse = ", ")))
  }
  if (length(actual) < length(expected)) {
    missing_cols <- expected[which(!expected %in% actual)]
    return(paste0(dataset, " contains fewer columns than expected. The following columns are missing: ", stringr::str_c(missing_cols, collapse = ", ")))
  }
  if (!all(expected == actual)) {
    unmatched_cols <- actual[which(!actual %in% expected)]
    return(paste0(dataset, " columns are not named correctly. The following column names are not expected: ", stringr::str_c(unmatched_cols, collapse = ", ")))
  }
}

#' Validate names of input data columns
#'
#' @param data The dataset being checked
#' @param expected Named character vector of expected column types, in the form "COLUMN_NAME" = "COLUMN_TYPE"
#' @param data_name Name of dataset (used in outputted message only)
#'
#' @return A character vector of issues found. If no issues are found, the vector will be empty.
validate_column_types <- function(data, expected, data_name) {

  issues <- c()

  for (n in names(expected)) {
    if (!n %in% names(data)) {
      issues <- c(issues, paste0("Column ", n, " not found in ", tolower(data_name), " - unable to validate column type."))
    } else if (!inherits(data[[n]], expected[[n]])) {
      issues <- c(issues, paste0("Type of column ", n, " in ",  tolower(data_name), " not correct. Expected ", expected[[n]], " but got ", class(data[[n]])))
    }
  }
  return(issues)
}

#' Validate vaccination dates
#'
#' @inheritParams validate_input_data
#'
#' @return A character vector of issues found. If no issues are found, the vector will be empty.
validate_vaccination_dates <- function(person_data, vaccination_data, split_size) {

  issues <- c()

  total_splits <- ceiling(nrow(person_data)/split_size)
  for (sp in 1:total_splits) {
    split_ids <- person_data$PID[((sp-1)*split_size+1):(sp*split_size)]

    combined_vaccinations <- person_data[PID %in% split_ids][vaccination_data[PID %in% split_ids], nomatch = 0, on = .(PID)]

    if (any(!is.na(combined_vaccinations$V_DATE) &
            ((!is.na(combined_vaccinations$ENROL_DATE) & combined_vaccinations$V_DATE < combined_vaccinations$ENROL_DATE) |
             combined_vaccinations$V_DATE < combined_vaccinations$DOB |
             (!is.na(combined_vaccinations$CENSOR_DATE) & combined_vaccinations$V_DATE > combined_vaccinations$CENSOR_DATE)))) {
      issues <- c(issues, "There are vaccination dates in your data that occur before enrolment or after censorship.")
      return(issues)
    }
  }

  return(issues)
}

#' Validate outcome dates
#'
#' @inheritParams validate_input_data
#'
#' @return A character vector of issues found. If no issues are found, the vector will be empty.
validate_outcome_dates <- function(person_data, outcome_data, split_size) {

  issues <- c()

  total_splits <- ceiling(nrow(person_data)/split_size)
  for (sp in 1:total_splits) {
    split_ids <- person_data$PID[((sp-1)*split_size+1):(sp*split_size)]

    combined_outcomes <- person_data[PID %in% split_ids][outcome_data[PID %in% split_ids], nomatch = 0, on = .(PID)]

    if (any(!is.na(combined_outcomes$EVENT_DATE) &
            ((!is.na(combined_outcomes$ENROL_DATE) & combined_outcomes$EVENT_DATE < combined_outcomes$ENROL_DATE) |
             combined_outcomes$EVENT_DATE < combined_outcomes$DOB |
             (!is.na(combined_outcomes$CENSOR_DATE) & combined_outcomes$EVENT_DATE > combined_outcomes$CENSOR_DATE)))) {
      issues <- c(issues, "There are outcome dates in your data that occur before enrolment or after censorship.")
      return(issues)
    }
  }

  return(issues)
}
