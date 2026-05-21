
# Write a data.table to disk in the configured output format
write_output_file <- function(data, path, format) {
  if (format == "parquet") {
    arrow::write_parquet(data, path)
  } else {
    fwrite(data, path)
  }
}

# Read an output file back from disk
read_output_file <- function(path, format) {
  if (format == "parquet") {
    result <- as.data.table(arrow::read_parquet(path))
    # Normalize types to match fread() output:
    #   factors → character (fread has no type info from CSV, returns character)
    #   Date → IDate (data.table's integer-backed date subclass)
    factor_cols <- names(which(vapply(result, is.factor, logical(1))))
    for (col in factor_cols) set(result, j = col, value = as.character(result[[col]]))
    date_cols <- names(which(vapply(result, function(x) inherits(x, "Date") && !inherits(x, "IDate"), logical(1))))
    for (col in date_cols) set(result, j = col, value = as.IDate(result[[col]]))
    result
  } else {
    fread(path)
  }
}

#' Convert number to date
#'
#' @description
#' Converts an 8 digit integer 'date' into a date object.
#' WARNING: does not check that inputted number is in the correct format
#'
#' @param date_number 8 digit integer representing a date in format yyyymmdd
#'
#' @return Date object
convert_number_to_date <- function(date_number) {
  clock::date_parse(stringr::str_replace(as.character(date_number), pattern = "(.{4})(.{2})", replacement = "\\1-\\2-"))
}

#' Convert date to number
#'
#' @description
#' Converts a date object into an 8 digit integer representing a date in the format yyyymmdd.
#'
#' @param date Date object
#'
#' @return 8 digit integer representing a date in the format yyyymmdd
convert_date_to_number <- function(date) {
  as.numeric(stringr::str_remove_all(as.character(date), "-"))
}

#' Check format of date
#'
#' @description
#' Checks that the inputted date is of a recognisable format, and if so, converts
#' it to a date object, otherwise throws an error
#'
#' @param date Character, number, or date object, representing a date.
#'
#' @return Date object
check_date_format <- function(date) {
  if (inherits(date, "Date")) {
    return(date)
  } else if (is.integer(date) | is.numeric(date)) {
    if (floor(log10(date)) + 1 == 8) {
      return(convert_number_to_date(date))
    } else {
      stop("Incorrect date format - numbers must be exactly 8 digits.")
    }
  } else if (is.character(date)) {
    if (nchar(date) == 10 & substr(date, 5, 5) %in% c("-", "/") & substr(date, 8, 8) %in% c("-", "/")) {
      return(as.Date(date))
    } else {
      stop("Incorrect date format - strings must be of the form YYYY-MM-DD.")
    }
  } else {
    stop("Incorrect date format - dates must be either an 8 digit number, a string of the form YYYY-MM-DD, or a Date object.")
  }
}


#' Read and convert options file
#'
#' @description
#' Reads options in from txt file and converts to a list, for use in aggregation functions.
#'
#' @param options_file_location Location where options.txt file is stored
#'
#' @return Options object in list format
read_options_file <- function(options_file_location) {

  if (!file.exists(file.path(options_file_location, "options.txt"))) {
    stop("Options file (options.txt) not found at the location given.")
  }

  options_lines <- readLines(file.path(options_file_location, "options.txt"))

  options <- list(
    study_codename = extract_and_split_line("study_codename", options_lines),
    period_length = extract_and_split_line("period_length", options_lines),
    align_periods = as.logical(extract_and_split_line("align_periods", options_lines)),
    ratio_precision = as.numeric(extract_and_split_line("ratio_precision", options_lines)),
    included_analyses = extract_and_split_line("included_analyses", options_lines),
    age_groups = list(
      bounds = as.numeric(extract_and_split_line("age_groups.bounds", options_lines)),
      labels = extract_and_split_line("age_groups.labels", options_lines)
    ),
    outcome_info = data.table(
      AESI = extract_and_split_line("outcome_info.AESI", options_lines),
      clean_window = as.numeric({
        if (any(grepl("^outcome_info\\.clean_window:", options_lines))) {
          extract_and_split_line("outcome_info.clean_window", options_lines)
        } else if (any(grepl("^outcome_info\\.case_ascertainment:", options_lines))) {
          warning("The `case_ascertainment` key in options.txt is deprecated. Please rename to `clean_window`.", call. = FALSE)
          extract_and_split_line("outcome_info.case_ascertainment", options_lines)
        } else {
          stop("options.txt must contain either an `outcome_info.clean_window` or `outcome_info.case_ascertainment` entry.")
        }
      }),
      risk_lower = as.numeric(extract_and_split_line("outcome_info.risk_lower", options_lines)),
      risk_upper = as.numeric(extract_and_split_line("outcome_info.risk_upper", options_lines)),
      washout_post = as.numeric(extract_and_split_line("outcome_info.washout_post", options_lines)),
      control_post_target = as.numeric(extract_and_split_line("outcome_info.control_post_target", options_lines)),
      control_post_min = as.numeric(extract_and_split_line("outcome_info.control_post_min", options_lines)),
      washout_pre = as.numeric(extract_and_split_line("outcome_info.washout_pre", options_lines)),
      control_pre_target = as.numeric(extract_and_split_line("outcome_info.control_pre_target", options_lines)),
      control_pre_min = as.numeric(extract_and_split_line("outcome_info.control_pre_min", options_lines))
    ),
    lookback_length = {
      if (any(grepl("^lookback_length:", options_lines))) {
        as.numeric(extract_and_split_line("lookback_length", options_lines))
      } else {
        2
      }
    },
    vaccine_info = data.table(
      V_SUBTYPE = {
        if (any(grepl("^vaccine_info\\.vaccine_subtypes:", options_lines))) {
          extract_and_split_line("vaccine_info.vaccine_subtypes", options_lines)
        } else if (any(grepl("^vaccine_info\\.vaccine_brands:", options_lines))) {
          warning("The `vaccine_info.vaccine_brands` key in options.txt is deprecated. Please rename to `vaccine_info.vaccine_subtypes`.", call. = FALSE)
          extract_and_split_line("vaccine_info.vaccine_brands", options_lines)
        } else {
          stop("options.txt must contain either a `vaccine_info.vaccine_subtypes` or `vaccine_info.vaccine_brands` entry.")
        }
      },
      V_TYPE = {
        if (any(grepl("^vaccine_info\\.vaccine_types:", options_lines))) {
          extract_and_split_line("vaccine_info.vaccine_types", options_lines)
        } else if (any(grepl("^vaccine_info\\.vaccine_platforms:", options_lines))) {
          warning("The `vaccine_info.vaccine_platforms` key in options.txt is deprecated. Please rename to `vaccine_info.vaccine_types`.", call. = FALSE)
          extract_and_split_line("vaccine_info.vaccine_platforms", options_lines)
        } else {
          stop("options.txt must contain either a `vaccine_info.vaccine_types` or `vaccine_info.vaccine_platforms` entry.")
        }
      }
    )
  )

  return(options)
}

#' Check options object
#'
#' @description
#' Checks that the options object has the correct format and contains all the required information.
#' If not, throws an error.
#'
#' @param options Options object in list format
#'
#' @return NULL
check_options_object <- function(options) {

  # Handle deprecated column names in options$vaccine_info and options$outcome_info.
  # These are data.table objects so setnames() modifies them by reference, meaning the
  # rename is visible in the calling environment without any explicit reassignment.
  if (inherits(options$vaccine_info, "data.table")) {
    if ("V_BRAND" %in% names(options$vaccine_info) && !"V_SUBTYPE" %in% names(options$vaccine_info)) {
      warning("The `V_BRAND` column in options$vaccine_info is deprecated. Please rename to `V_SUBTYPE`.", call. = FALSE)
      setnames(options$vaccine_info, "V_BRAND", "V_SUBTYPE")
    }
    if ("V_PLATFORM" %in% names(options$vaccine_info) && !"V_TYPE" %in% names(options$vaccine_info)) {
      warning("The `V_PLATFORM` column in options$vaccine_info is deprecated. Please rename to `V_TYPE`.", call. = FALSE)
      setnames(options$vaccine_info, "V_PLATFORM", "V_TYPE")
    }
  }
  if (inherits(options$outcome_info, "data.table")) {
    if ("case_ascertainment" %in% names(options$outcome_info) && !"clean_window" %in% names(options$outcome_info)) {
      warning("The `case_ascertainment` column in options$outcome_info is deprecated. Please rename to `clean_window`.", call. = FALSE)
      setnames(options$outcome_info, "case_ascertainment", "clean_window")
    }
  }

  required_names <- c("age_groups", "period_length", "align_periods", "outcome_info",
                      "ratio_precision", "vaccine_info", "study_codename", "included_analyses")
  if (!all(required_names %in% names(options))) {
    stop("Options object not in the correct format.")
  }
  if (!is.null(options$lookback_length)) {
    if (!is.numeric(options$lookback_length) || options$lookback_length <= 0) {
      stop("`lookback_length` in options must be a positive number (in years).")
    }
  }
  if (!all(sort(names(options$age_groups)) == sort(c("bounds", "labels")))) {
    stop("Options object not in the correct format.")
  }
  if (!all(sort(names(options$outcome_info)) == sort(c("AESI", "clean_window", "risk_lower","risk_upper", "washout_post", "control_post_target",
                                                       "control_post_min", "washout_pre", "control_pre_target", "control_pre_min")))) {
    stop("Options object not in the correct format.")
  }
  if (!all(sort(names(options$vaccine_info)) == sort(c("V_SUBTYPE", "V_TYPE")))) {
    stop("Options object not in the correct format.")
  }
  if (!inherits(options, "list") | !inherits(options$period_length, "character") | !inherits(options$align_periods, "logical") | !inherits(options$ratio_precision, "numeric") |
      !inherits(options$included_analyses, "character") | !inherits(options$study_codename, "character") |
      !inherits(options$age_groups, "list") | !inherits(options$age_groups$bounds, "numeric") | !inherits(options$age_groups$labels, "character") |
      !inherits(options$vaccine_info, "data.table") |  !inherits(options$vaccine_info$V_SUBTYPE, "character") |  !inherits(options$vaccine_info$V_TYPE, "character") |
      !inherits(options$outcome_info, "data.table") | !inherits(options$outcome_info$AESI, "character") | !inherits(options$outcome_info$clean_window, "numeric") |
      !inherits(options$outcome_info$risk_lower, "numeric") | !inherits(options$outcome_info$risk_upper, "numeric") | !inherits(options$outcome_info$washout_post, "numeric") |
      !inherits(options$outcome_info$control_post_target, "numeric") | !inherits(options$outcome_info$control_post_min, "numeric") | !inherits(options$outcome_info$washout_pre, "numeric") |
      !inherits(options$outcome_info$control_pre_target, "numeric") | !inherits(options$outcome_info$control_pre_min, "numeric")) {
    stop("Options object not in the correct format.")
  }
}

#' Extract data from line of text
#'
#' @description
#' For use in reading data from options file.
#' Finds line starting with 'title', then extracts everything after the ':' and splits by spaces.
#' Results are stored in a vector.
#'
#' @param title Name or title of line in text, will search for 'title:' at the start of the line.
#' @param lines Character vector containing separate lines of text
#'
#' @return Character vector containing information from line of text
extract_and_split_line <- function(title, lines) {
  line <- grep(paste0(title, ":"), lines, value = TRUE, fixed = TRUE)
  start_id <- stringr::str_locate(line, ":")[1] + 1
  values <- stringr::str_split(substr(line, start_id, nchar(line)), " ")[[1]]
  values <- values[values != ""]
  return(values)
}


#' Calculate descriptive statistics
#'
#' @param person_data Person data input data.table. If NULL, descriptives for this dataset will be skipped.
#' @param vaccination_data Vaccination data input data.table. If NULL, descriptives for this dataset will be skipped.
#' @param outcome_data AESI outcome data input data.table. If NULL, descriptives for this dataset will be skipped.
#' @param age_reference_date Reference date to calculate age from (generally cycle start)
#'
#' @return Character vector of descriptive statistics
get_descriptives <- function(person_data, vaccination_data, outcome_data,
                             age_reference_date) {

  descriptives <- c()

  if (!is.null(person_data)) {
    dob_range <- convert_number_to_date(c(max(person_data$DOB), min(person_data$DOB)))
    age_range <- floor(as.numeric(age_reference_date - dob_range)/365.25)
    sex_dist <- table(person_data$SEX)
    censor_dist <- table(person_data$CENSOR_TYPE)

    descriptives <- c(descriptives,
                      "-------------",  "\n",
                      "Person data",  "\n",
                      "-------------",  "\n",
                      "Total number: ", nrow(person_data), "\n",
                      "Age range (at cycle start): ", paste0(age_range[1], " - ", age_range[2]), "\n",
                      "Sex distribution: ", paste(paste(names(sex_dist), sex_dist, sep = " = "), collapse = ", "), "\n",
                      "Censor type distribution: ", paste(paste(names(censor_dist), censor_dist, sep = " = "), collapse = ", "), "\n"
    )
  }

  if (!is.null(outcome_data)) {

    date_range <- convert_number_to_date(c(min(outcome_data$EVENT_DATE), max(outcome_data$EVENT_DATE)))
    aesi_dist <- table(outcome_data$AESI)
    encounter_dist <- table(outcome_data$ENCOUNTER_TYPE)

    descriptives <- c(descriptives,
                      "-------------",  "\n",
                      "Outcome data",  "\n",
                      "-------------",  "\n",
                      "Total number: ", nrow(outcome_data), "\n",
                      "Date range: ", paste0(date_range[1], " - ", date_range[2]), "\n",
                      "AESI distribution: ", paste(paste(names(aesi_dist), aesi_dist, sep = " = "), collapse = ", "), "\n",
                      "Encounter type distribution: ", paste(paste(names(encounter_dist), encounter_dist, sep = " = "), collapse = ", "), "\n"
    )
  }

  if (!is.null(vaccination_data)) {

    date_range <- convert_number_to_date(c(min(vaccination_data$V_DATE), max(vaccination_data$V_DATE)))
    subtype_dist <- table(vaccination_data$V_SUBTYPE)

    descriptives <- c(descriptives,
                      "-------------",  "\n",
                      "Vaccination data",  "\n",
                      "-------------",  "\n",
                      "Total number: ", nrow(vaccination_data), "\n",
                      "Date range: ", paste0(date_range[1], " - ", date_range[2]), "\n",
                      "Subtype distribution: ", paste(paste(names(subtype_dist), subtype_dist, sep = " = "), collapse = ", "), "\n"
    )
  }

  return(descriptives)
}

#' Split time periods
#'
#' @description
#' Given a period of time, it will split that interval into sub-periods based on the given length.
#'
#' @param start Start of time interval to split, as a date object
#' @param end End of time interval to split, as a date object
#' @param period_length Character representing desired length of sub-periods, e.g. 'month', 'day'
#' @param align_periods Boolean whether or not to adjust time periods so that they will align,
#' even if start dates differ. Currently only set up to handle monthly periods, but may be generalised in the future.
#'
#' @return A data.table containing start and end dates of sub-periods
split_time_periods <- function(start, end, period_length, align_periods = FALSE){

  if (align_periods & period_length != "month") {
    stop("Time period alignment is currently only set up to work for monthly periods. Either change the period length, or opt not to align periods.")
  }

  if (align_periods & clock::get_day(start) != 1) {
    # When aligning to calendar months and the start date is mid-month, the first period
    # runs only to the end of that calendar month so all subsequent periods are month-aligned.
    first_period_start <- start
    first_period_end <- clock::date_end(start, "month")
    start <- first_period_end + 1
  } else {
    first_period_start <- NULL
  }

  period_start <- seq(from = start, to = end - 1, by = period_length)

  if (length(period_start) > 1) {
    period_end <- c(period_start[2:length(period_start)] - 1, end)
  } else {
    period_end <- end
  }

  if (period_end[length(period_end)] < end) {
    period_start <- c(period_start, period_end[length(period_end)] + 1)
    period_end <- c(period_end, end)
  }

  if (!is.null(first_period_start)) {
    period_start <- c(first_period_start, period_start)
    period_end <- c(first_period_end, period_end)
  }

  return(data.table(period_start = period_start,
                    period_end = period_end))

}

#' Apply clean window rules
#'
#' @description
#' Given a clean window, only the first AESI outcome within that window will be kept.
#' This is to aid identification of incident outcomes.
#'
#'
#' @param data A data.table of AESI outcome data on which to apply the clean window
#' @param windows A data.table of AESI outcome codes and their corresponding clean windows (columns named 'AESI' and 'clean_window')
#'
#' @return A data.table of AESI outcome data with non-incident cases removed
apply_clean_window <- function(data, windows) {

  windows[
    data, on = .(AESI)
  ][
    order(PID, AESI, EVENT_DATE)
  ][
    # days_diff measures the gap between consecutive events for the same person/AESI.
    # It is NA for the first event of each person/AESI (no prior event to compare against).
    # A row is kept only when there is no prior event (NA) or the prior event occurred more
    # than clean_window days ago, retaining only incident cases within each clean window.
    , days_diff := as.numeric(EVENT_DATE - shift(EVENT_DATE, n = 1, type = "lag")), by = .(PID, AESI)
  ][
    is.na(days_diff) | days_diff > clean_window
  ][
    , .(PID, AESI, EVENT_DATE, ENCOUNTER_TYPE)
  ]
}

#' Map vaccine subtypes to types
#'
#' @param data A data.table containing at least a column called V_SUBTYPE
#' @param vaccine_mapping A data.table containing columns called V_SUBTYPE and V_TYPE
#'
#' @return A data.table identical to data input, but with an extra column called V_TYPE
map_subtypes_to_type <- function(data, vaccine_mapping) {

  if (!all(unique(data$V_SUBTYPE) %in% vaccine_mapping$V_SUBTYPE)) {
    stop(paste0("Your data contains unrecognised vaccine subtypes. Recognised subtypes are: ",
                stringr::str_c(vaccine_mapping$V_SUBTYPE, collapse = ", ")))
  }

  vaccine_mapping[
    data, on = .(V_SUBTYPE)
  ]
}

#' Find first vaccination
#'
#' @description
#' Finds the date of first vaccination for each person, based on the data provided.
#'
#' @param data A data.table of vaccination data, containing at least PID and V_DATE columns
#'
#' @return A data.table of vaccination data, with additional column 'first_vac' representing date of first vaccination for each person
find_first_vac <- function(data) {

  data[
    order(PID, V_DATE)
  ][
    # PID_prev is the PID of the preceding row. Where it differs from the current PID (or
    # is NA at the start of the table), this row is the first vaccination for that person.
    # first_vac is set to V_DATE on those rows, then forward-filled to cover all doses.
    , PID_prev := shift(PID, type = "lag")
  ][
    , first_vac := fifelse(is.na(PID_prev) | PID != PID_prev, V_DATE, NA)
  ][
    , first_vac := nafill(first_vac, "locf")
  ][
    , PID_prev := NULL
  ]
}

#' Calculate cumulative vaccine dose numbers
#'
#' @param data A data.table of vaccination data, containing at least PID and V_DATE columns, and possibly V_SUBTYPE and V_TYPE depending on specified reference
#' @param reference Defines how to accumulate vaccine doses: by 'dose' (all doses counted), 'subtype' (accumulates by subtype), or 'type' (accumulates by type).
#'
#' @return A data.table of vaccination data, with additional column 'V_DOSE' representing cumulative vaccine dose for each person
calc_vaccine_dose <- function(data, reference) {

  if (reference == "dose") {
    data[
      order(PID, V_DATE)
    ][
      , V_DOSE := 1:.N, by = .(PID)
    ]
  } else if (reference == "subtype") {
    data[
      order(PID, V_DATE)
    ][
      , V_DOSE := 1:.N, by = .(PID, V_SUBTYPE)
    ]
  } else if (reference == "type") {
    data[
      order(PID, V_DATE)
    ][
      , V_DOSE := 1:.N, by = .(PID, V_TYPE)
    ]
  } else {
    stop("Reference for accumulating doses not recognised. Must be 'dose', 'subtype', or 'type'.")
  }
}

#' Combine temporary results
#'
#' @description
#' Aggregation is not performed all in one go, but in sections, to manage memory use and allow stopping/restarting.
#' The results are saved as they are completed, in a 'temp' folder. Once all the sections have been aggregated, the
#' results are combined using this function, saved in the main results folder, and the 'temp' folder and its contents are removed.
#'
#' @param design_selection Character vector of specific study designs to run
#' @param results_file_path File path to results folder (the main folder created for this cycle)
#' @param options Options object, already converted from txt file.
#' @param historical_aggregation Boolean stating whether or not these results are for historical aggregation, so that it knows what files to expect.
#' @param output_format Output file format: `"parquet"` or `"csv"`.
#'
#' @return NULL
combine_temp_results <- function(design_selection, results_file_path, options, historical_aggregation,
                                 output_format = "parquet") {

  temp_file_path <- file.path(results_file_path, "temp")
  ext <- if (output_format == "parquet") ".parquet" else ".csv"

  if (!historical_aggregation) {

    write_output_file(
      load_combine_summarise("data_descriptive_outcomes", temp_file_path, "COUNT",
                             c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX")),
      path = file.path(results_file_path, paste0("data_descriptive_outcomes", ext)),
      format = output_format
    )

    write_output_file(
      load_combine_summarise("data_descriptive_vaccinations", temp_file_path, "COUNT",
                             c("PERIOD_START", "PERIOD_END", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE")),
      path = file.path(results_file_path, paste0("data_descriptive_vaccinations", ext)),
      format = output_format
    )

    if ("self_post" %in% design_selection) {
      write_output_file(
        load_combine_summarise("data_self_post", temp_file_path, c("CASE_COUNT", "CONTROL_COUNT"),
                               c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "WINDOW_RATIO")),
        path = file.path(results_file_path, paste0("data_self_post", ext)),
        format = output_format
      )
    }

    if ("self_pre" %in% design_selection) {
      write_output_file(
        load_combine_summarise("data_self_pre", temp_file_path, c("CASE_COUNT", "CONTROL_COUNT"),
                               c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "WINDOW_RATIO")),
        path = file.path(results_file_path, paste0("data_self_pre", ext)),
        format = output_format
      )
    }

    if ("historical" %in% design_selection) {
      write_output_file(
        load_combine_summarise("data_exposed_cases", temp_file_path, "CASE_COUNT",
                               c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE")),
        path = file.path(results_file_path, paste0("data_exposed_cases", ext)),
        format = output_format
      )

      write_output_file(
        load_combine_summarise("data_exposed_person_days", temp_file_path, "PERSON_DAYS",
                               c("PERIOD_START", "PERIOD_END", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE")),
        path = file.path(results_file_path, paste0("data_exposed_person_days", ext)),
        format = output_format
      )
    }

    if ("concurrent_vac" %in% design_selection) {
      write_output_file(
        concurrent_final_aggregation(
          control_group = rbindlist(lapply(grep("data_concurrent_vac_control_group_", list.files(temp_file_path), value = TRUE),
                                           function(x) safe_read_fst(file.path(temp_file_path, x)))),
          risk_group = rbindlist(lapply(grep("data_concurrent_vac_risk_group_", list.files(temp_file_path), value = TRUE),
                                        function(x) safe_read_fst(file.path(temp_file_path, x)))),
          control_counts = rbindlist(lapply(grep("data_concurrent_vac_control_counts_", list.files(temp_file_path), value = TRUE),
                                            function(x) safe_read_fst(file.path(temp_file_path, x)))),
          case_counts = rbindlist(lapply(grep("data_concurrent_vac_case_counts_", list.files(temp_file_path), value = TRUE),
                                         function(x) safe_read_fst(file.path(temp_file_path, x)))),
          output = data.table(),
          analysis_types = options$included_analyses,
          options = options
        ),
        path = file.path(results_file_path, paste0("data_concurrent_vac", ext)),
        format = output_format
      )
    }

    if ("concurrent_unvac" %in% design_selection) {
      write_output_file(
        concurrent_final_aggregation(
          control_group = rbindlist(lapply(grep("data_concurrent_unvac_control_group_", list.files(temp_file_path), value = TRUE),
                                           function(x) safe_read_fst(file.path(temp_file_path, x)))),
          risk_group = rbindlist(lapply(grep("data_concurrent_unvac_risk_group_", list.files(temp_file_path), value = TRUE),
                                        function(x) safe_read_fst(file.path(temp_file_path, x)))),
          control_counts = rbindlist(lapply(grep("data_concurrent_unvac_control_counts_", list.files(temp_file_path), value = TRUE),
                                            function(x) safe_read_fst(file.path(temp_file_path, x)))),
          case_counts = rbindlist(lapply(grep("data_concurrent_unvac_case_counts_", list.files(temp_file_path), value = TRUE),
                                         function(x) safe_read_fst(file.path(temp_file_path, x)))),
          output = data.table(),
          analysis_types = options$included_analyses,
          options = options
        ),
        path = file.path(results_file_path, paste0("data_concurrent_unvac", ext)),
        format = output_format
      )
    }

  } else {

    write_output_file(
      load_combine_summarise("data_historic_cases", temp_file_path, "COUNT",
                             c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX")),
      path = file.path(results_file_path, paste0("data_historic_cases", ext)),
      format = output_format
    )

    write_output_file(
      load_combine_summarise("data_historic_person_days", temp_file_path, "PERSON_DAYS",
                             c("PERIOD_START", "PERIOD_END", "AGE_GROUP", "SEX")),
      path = file.path(results_file_path, paste0("data_historic_person_days", ext)),
      format = output_format
    )
  }

  unlink(temp_file_path, recursive = TRUE)
  gc()

}


# Internal helper: read an fst file with a clear error if the file is corrupt.
# A corrupt file (e.g. from an interrupted write) causes fst to error; without
# this wrapper the error message gives no indication of what went wrong or how to
# recover. Users should delete the temp folder and restart aggregation.
safe_read_fst <- function(path) {
  tryCatch(
    fst::read_fst(path, as.data.table = TRUE),
    error = function(e) stop(paste0(
      "Failed to read temporary file '", basename(path), "'. ",
      "The file may be corrupt due to an interrupted write. ",
      "Delete the temp folder at '", dirname(path), "' and restart aggregation from scratch.\n",
      "Original error: ", conditionMessage(e)
    ))
  )
}


#' Load, combine, and summarise temp data
#'
#' @description
#' A helper function to load, combine, and summarise temp data, called from `combine_temp_results` function.
#' Incorporates a catch so that empty data does not cause an error.
#'
#' @param file_name_pattern Pattern to look for in file names. Files with matching pattern will be loaded and combined
#' @param file_path Path to where to look for files
#' @param summarise_value Name(s) of summary column(s), e.g. COUNT
#' @param summarise_columns Names of columns to summarise by
#'
#' @return Combined data table
load_combine_summarise <- function(file_name_pattern, file_path, summarise_value, summarise_columns){

  combined_data <- rbindlist(lapply(grep(file_name_pattern, list.files(file_path), value = TRUE),
                                    function(x) safe_read_fst(file.path(file_path, x))))

  if (nrow(combined_data) > 0) {
    combined_data <- combined_data[
      , lapply(.SD, sum),
      by = summarise_columns,
      .SDcols = summarise_value
    ]

  } else if (ncol(combined_data) == 0) {
    combined_data[, c(summarise_columns, summarise_value) := character()]
  }

  return(combined_data)
}

#' Resume data aggregation
#'
#' @description
#' If aggregation for a particular cycle has been started but not completed, this function can
#' be used to easily resume aggregation from where it was stopped.
#'
#' @param folder_name Name of main results folder for this cycle (not including path)
#' @param working_directory Location of the main results folder for this cycle (path only), defaults to `getwd()`.
#' @param stop_after_n_splits Use this to stop aggregation after a given number of splits. Aggregation can then be resumed at a later date (again).
#' @param input_data_in_chunks Option to pre-split input data. Only use this if your data is too large to load into R in one go (otherwise just use split_size).
#' This should match what was used for the initial call of `aggregate_data()`.
#' @param final_chunk If you have pre-split your input data, use this to indicate when you are running the last chunk of your input data.
#' This will trigger the function to combine all the temporary results at the end. This should match what was used for the initial call of `aggregate_data()`.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' # Resume a previously interrupted aggregation
#' resume_cycle_aggregation(
#'   folder_name = "RCA_COVID_TEST_SITE_20220101-20220131",
#'   working_directory = "~/Documents/RCA_COVID/"
#' )
#' }
#'
#' @export
resume_cycle_aggregation <- function(folder_name, working_directory = getwd(),
                                     stop_after_n_splits = NULL,
                                     input_data_in_chunks = FALSE,
                                     final_chunk = FALSE){

  results_file_path <- file.path(working_directory, folder_name)
  temp_file_path <- file.path(results_file_path, "temp")
  data_file_path <- file.path(results_file_path, "data")

  if (!dir.exists(temp_file_path)) {
    stop("There are no temporary results in the specified folder, this aggregation may already be complete.")
  }
  if (!dir.exists(data_file_path)) {
    stop("There is no input data stored in the specified folder. Aggregation can not be resumed.")
  }

  if (input_data_in_chunks) {
    cat("Resuming aggregation of current input chunk only - to continue to next chunk, new input data must be provided.\n")
  }

  stored_parameters <- readRDS(file.path(temp_file_path, "stored_parameters.rds"))

  # Backward compat: old stored_parameters have participation_level but no design_selection
  if (is.null(stored_parameters$design_selection) && !is.null(stored_parameters$participation_level)) {
    old_level <- stored_parameters$participation_level
    stored_parameters$design_selection <- c("self_post")
    if (old_level > 1) stored_parameters$design_selection <- c(stored_parameters$design_selection, "self_pre", "historical", "concurrent_vac")
    if (old_level > 2) stored_parameters$design_selection <- c(stored_parameters$design_selection, "concurrent_unvac")
  }

  aggregate_data(patient_data = NULL, vaccination_data = NULL, outcome_data = NULL,
                 cycle_start_date = stored_parameters$cycle_start_date,
                 cycle_end_date = stored_parameters$cycle_end_date,
                 site_code = stored_parameters$site_code,
                 design_selection = stored_parameters$design_selection,
                 options_file_location = NULL,
                 options = stored_parameters$options,
                 suppression_limit = stored_parameters$suppression_limit,
                 split_size = stored_parameters$split_size,
                 stop_after_n_splits = stop_after_n_splits,
                 working_directory = working_directory,
                 restore_input_data = stored_parameters$restore_input_data,
                 input_data_in_chunks = input_data_in_chunks,
                 final_chunk = final_chunk
  )

}

#' Recombine input datasets
#'
#' @description
#' When aggregating data, the input datasets are split up to make them more manageable.
#' The split datasets are stored in the 'data' folder, and the whole datasets are removed from
#' the global environment, until aggregation is complete. If you need to manually restore the original
#' input datasets to the global environment, you can do so using this function.
#'
#' @param data_file_path Path to 'data' folder where split datasets are stored
#' @param person_data_input_name Desired name of person_data object in global environment, defaults to "person_data". Set to NULL to skip this dataset.
#' @param vaccination_data_input_name Desired name of vaccination_data object in global environment, defaults to "vaccination_data". Set to NULL to skip this dataset.
#' @param outcome_data_input_name Desired name of outcome_data object in global environment, defaults to "outcome_data". Set to NULL to skip this dataset.
#' @param patient_data_input_name Deprecated. Use `person_data_input_name` instead.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' # Restore split input data back to the global environment
#' recombine_input_data(
#'   data_file_path = "path/to/results/data"
#' )
#' }
#'
#' @export
recombine_input_data <- function(data_file_path,
                                 person_data_input_name = "person_data",
                                 vaccination_data_input_name = "vaccination_data",
                                 outcome_data_input_name = "outcome_data",
                                 patient_data_input_name = NULL) {

  if (!is.null(patient_data_input_name)) {
    warning("The `patient_data_input_name` argument is deprecated. Use `person_data_input_name` instead.", call. = FALSE)
    if (person_data_input_name == "person_data") person_data_input_name <- patient_data_input_name
  }

  if (!is.null(person_data_input_name)) {
    person_files <- grep("person_data_split_", list.files(data_file_path), value = TRUE)
    if (length(person_files) == 0) {
      person_files <- grep("patient_data_split_", list.files(data_file_path), value = TRUE)
    }
    assign(person_data_input_name,
           rbindlist(lapply(person_files,
                            function(x) fst::read_fst(file.path(data_file_path, x), as.data.table = TRUE))),
           envir = .GlobalEnv)
  }

  if (!is.null(vaccination_data_input_name)) {
    assign(vaccination_data_input_name,
           rbindlist(lapply(grep("vaccination_data_split_", list.files(data_file_path), value = TRUE),
                            function(x) fst::read_fst(file.path(data_file_path, x), as.data.table = TRUE))),
           envir = .GlobalEnv)
  }

  if (!is.null(outcome_data_input_name)) {
    assign(outcome_data_input_name,
           rbindlist(lapply(grep("outcome_data_split_", list.files(data_file_path), value = TRUE),
                            function(x) fst::read_fst(file.path(data_file_path, x), as.data.table = TRUE))),
           envir = .GlobalEnv)
  }

}

#' Suppress low counts
#'
#' @param data A data.table containing data to suppress counts in (will only suppress columns called "COUNT", "CASE_COUNT", "CONTROL_COUNT", or "PERSON_DAYS")
#' @param min_count Minimum count allowed - counts less than this will be suppressed, except 0 counts
#'
#' @return A data.table containing data with low counts suppressed
suppress_low_counts <- function(data, min_count) {

  suppress_columns <- names(data)[names(data) %in% c("COUNT", "CASE_COUNT", "CONTROL_COUNT", "PERSON_DAYS")]

  data[, c(suppress_columns) := lapply(.SD, function(x) fifelse(x < min_count & x > 0, -1, x)), .SDcols = suppress_columns]
}

#' Create notes file
#'
#' @description
#' This creates a txt file containing notes regarding the data aggregation, including some basic
#' descriptive statistics (cohort size, total vaccinations, total outcomes) and the options that were used.
#'
#' @inheritParams aggregate_data
#'
#' @param options Options object, already converted from txt file
#' @param stored_parameters A list of parameters used in this aggregation, saved temporarily whilst aggregation is ongoing
#' @param save_location Location in which to save the created txt file
#'
#' @return NULL
create_notes_file <- function(cycle_start_date, cycle_end_date,
                              site_code,
                              design_selection,
                              options,
                              stored_parameters,
                              save_location) {

  cat(
    "-------------",
    "Analysis info",
    "-------------",
    paste0("Site: ", site_code),
    paste0("Study: ", options$study_codename),
    paste0("Study designs: ", paste(design_selection, collapse = " ")),
    paste0("Cycle period: ", cycle_start_date, " - ", cycle_end_date),
    paste0("Aggregation started on: ", stored_parameters$aggregation_start_date),
    paste0("Aggregation finished on: ", Sys.Date()),
    paste0("Cohort size: ", stored_parameters$cohort_size),
    paste0("Total vaccinations: ", stored_parameters$total_vaccinations),
    paste0("Vaccination period: ", convert_number_to_date(stored_parameters$first_vaccination_date), " - ", convert_number_to_date(stored_parameters$last_vaccination_date)),
    paste0("Total outcomes: ", stored_parameters$total_outcomes),
    paste0("Outcome period: ", convert_number_to_date(stored_parameters$first_outcome_date), " - ", convert_number_to_date(stored_parameters$last_outcome_date)),
    paste0("Suppression limit applied: ", stored_parameters$suppression_limit),
    paste0("Lookback length (years): ", if (!is.null(options$lookback_length)) options$lookback_length else 2),
    paste0("Lookback start date: ", stored_parameters$lookback_start_date),
    "-------------",
    "Options used",
    "-------------",
    "Age groups",
    paste0("\t Age group bounds: ", paste0(options$age_groups$bounds, collapse = " ")),
    paste0("\t Age group labels: ", paste0(options$age_groups$labels, collapse = " ")),
    "Outcomes",
    paste0("\t AESI: ", paste0(options$outcome_info$AESI, collapse = " ")),
    paste0("\t Clean windows: ", paste0(options$outcome_info$clean_window, collapse = " ")),
    paste0("\t Risk windows: ", paste0(stringr::str_c(options$outcome_info$risk_lower, options$outcome_info$risk_upper, sep = "-"), collapse = " ")),
    paste0("\t Washout windows (post): ", paste0(options$outcome_info$washout_post, collapse = " ")),
    paste0("\t Washout windows (pre): ", paste0(options$outcome_info$washout_pre, collapse = " ")),
    paste0("\t Target control ratio (post): ", paste0(options$outcome_info$control_post_target, collapse = " ")),
    paste0("\t Target control ratio (pre): ", paste0(options$outcome_info$control_pre_target, collapse = " ")),
    paste0("\t Minimum control length (post): ", paste0(options$outcome_info$control_post_min, collapse = " ")),
    paste0("\t Minimum control length (pre): ", paste0(options$outcome_info$control_pre_min, collapse = " ")),
    "Vaccines",
    paste0("\t Vaccine subtypes: ", paste0(options$vaccine_info$V_SUBTYPE, collapse = " ")),
    paste0("\t Vaccine types: ", paste0(options$vaccine_info$V_TYPE, collapse = " ")),
    'Primary and sub-analyses',
    paste0("\t Included analyses: ", paste0(options$included_analyses, collapse = " ")),
    'Others',
    paste0("\t Ratio precision: ", paste0(options$ratio_precision, collapse = " ")),
    paste0("\t Period length: ", paste0(options$period_length, collapse = " ")),
    sep = "\n",
    file = file.path(save_location, "notes.txt")
  )
}
