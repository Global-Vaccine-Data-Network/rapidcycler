
#' Aggregate line list datasets
#'
#' @description
#' This function will take the input datasets and create a folder containing the aggregated tables
#' as separate parquet files, depending on your design selection. A separate folder is created for each cycle period.
#'
#' Aggregation can be stopped and resumed (see `resume_cycle_aggregation()` function).
#'
#' To trigger historical data aggregation (i.e. before vaccinations began), set `vaccination_data = NULL`.
#'
#' Requires an 'options.txt' file to run - in a multi-site study this is provided by the coordinating centre; in a single-site study you create it yourself (see `vignette("options", "rapidcycler")`).
#'
#' Validation of the input datasets is automatically run as part of this function, and will throw an error if it finds any issues that require fixing.
#' If you would like to run validation separately, use the `validate_input_data()` function.
#'
#'
#' @param person_data Person data input data.table
#' @param patient_data Deprecated. Use `person_data` instead.
#' @param vaccination_data Vaccination data input data.table. Set to NULL to perform historical data aggregation
#' @param outcome_data AESI outcome data input data.table
#' @param cycle_start_date Start date of cycle, as a date object, an 8 digit number of the form yyyymmdd, or a string of the form yyyy-mm-dd
#' @param cycle_end_date End date of cycle, as a date object, an 8 digit number of the form yyyymmdd, or a string of the form yyyy-mm-dd
#' @param site_code Site code (e.g. AUS_MCRI), as a string
#' @param design_selection Character vector of analytical study designs to run. Valid values are:
#' `"self_post"`, `"self_pre"`, `"historical"`, `"concurrent_vac"`, `"concurrent_unvac"`.
#' Default is NULL, which runs all five designs.
#' Descriptive outcomes and vaccination datasets are always produced for non-historical aggregations
#' and cannot be excluded via this parameter.
#' @param participation_level Deprecated. Use `design_selection` instead.
#' Previously controlled which designs were available: 1 = minimal, 2 = partial, 3 = full.
#' @param options_file_location Location where options.txt file is stored. Supply either `options_file_location` or `options`, not both. Defaults to `getwd()`
#' @param options Options object, already converted from txt file. Supply either options_file_location or options, not both
#' @param suppression_limit If required, low numbers can be suppressed. Numbers less than the limit will be suppressed, except 0
#' @param split_size Input data will be automatically split into pieces of this size (default 500,000). If memory is an issue, lower the split_size.
#' Aggregation is done on the split data separately, and then combined once all splits are complete.
#' Aggregation can be stopped and restarted from the last completed split (see `resume_cycle_aggregation()` function)
#' @param stop_after_n_splits Use this to stop aggregation after a given number of splits. Aggregation can then be resumed at a later date (see `resume_cycle_aggregation()` function)
#' @param restore_input_data If TRUE (default) the input data will be restored from the data folder to the global environment once the aggregation is complete,
#' or stop_after_n_splits is reached. If FALSE, the input data will not be restored.
#' WARNING: the data folder is removed once aggregation is complete (not if stopped using `stop_after_n_splits`)
#' @param working_directory Location of working directory. This is where the folder for this cycle will be created. Defaults to `getwd()`.
#' @param input_data_in_chunks Option to pre-split input data. Only use this if your data is too large to load into R in one go (otherwise just use split_size)
#' @param final_chunk If you have pre-split your input data, use this to indicate when you are running the last chunk of your input data. This will trigger the function
#' to combine all the temporary results at the end
#' @param output_format Output file format: `"parquet"` (default, recommended for efficiency
#'   and cross-language compatibility) or `"csv"` (for legacy workflows).
#' @param skip_user_prompts Boolean allowing user prompts to be skipped, mainly for the purpose of testing for which prompts are not possible. Recommended to leave as FALSE.
#'
#' @return NULL
#'
#' @examples
#' \dontrun{
#' # Generate synthetic data and aggregate
#' data <- generate_synthetic_data(pop_size = 1000, save_data = FALSE)
#'
#' aggregate_data(
#'   person_data = data$person_data,
#'   vaccination_data = data$vaccination_data,
#'   outcome_data = data$outcome_data,
#'   cycle_start_date = "2021-01-01",
#'   cycle_end_date = "2023-01-01",
#'   site_code = "TEST_SITE",
#'   options_file_location = "path/to/options",
#'   skip_user_prompts = TRUE
#' )
#' }
#'
#' @export
aggregate_data <- function(person_data = NULL, vaccination_data, outcome_data,
                           cycle_start_date, cycle_end_date,
                           site_code,
                           design_selection = NULL,
                           options_file_location = getwd(),
                           options = NULL,
                           suppression_limit = NULL,
                           split_size = 500000,
                           stop_after_n_splits = NULL,
                           restore_input_data = TRUE,
                           working_directory = getwd(),
                           input_data_in_chunks = FALSE,
                           final_chunk = FALSE,
                           skip_user_prompts = FALSE,
                           output_format = "parquet",
                           patient_data = NULL,
                           participation_level = NULL) {

  if (!is.null(patient_data)) {
    warning("The `patient_data` argument is deprecated. Use `person_data` instead.", call. = FALSE)
    if (is.null(person_data)) person_data <- patient_data
  }

  time1 <- Sys.time()
  aggregation_start_date <- Sys.Date()

  # Capture the unevaluated argument expressions from the caller so that the original
  # variable names (e.g. "person_data", "my_vac_data") can be used when restoring the
  # input datasets to the global environment after aggregation completes.
  input_names <- rlang::call_args(sys.call())

  cycle_start_date <- check_date_format(cycle_start_date)
  cycle_end_date <- check_date_format(cycle_end_date)

  if (!is.character(site_code)) {
    stop("Site code must be a string or character.")
  }
  if (!is.null(participation_level)) {
    warning("The `participation_level` argument is deprecated. Use `design_selection` instead.", call. = FALSE)
    if (is.null(design_selection)) {
      design_selection <- c("self_post")
      if (participation_level > 1) design_selection <- c(design_selection, "self_pre", "historical", "concurrent_vac")
      if (participation_level > 2) design_selection <- c(design_selection, "concurrent_unvac")
    } else {
      warning("Both `participation_level` and `design_selection` were provided; `participation_level` is ignored.", call. = FALSE)
    }
  }
  if (is.null(design_selection)) {
    design_selection <- c("self_post", "self_pre", "historical", "concurrent_vac", "concurrent_unvac")
  }
  valid_designs <- c("self_post", "self_pre", "historical", "concurrent_vac", "concurrent_unvac")
  if (!all(design_selection %in% valid_designs)) {
    stop(paste0("Unknown design(s) in design_selection: ",
                paste(setdiff(design_selection, valid_designs), collapse = ", "),
                ". Valid options are: ", paste(valid_designs, collapse = ", ")))
  }
  if (!is.null(suppression_limit) && !is.numeric(suppression_limit)) {
    stop("If supplied, suppression limit must be a number.")
  }
  if (!(is.numeric(split_size) && split_size > 0)) {
    stop("Split size must be a number greater than 0.")
  }
  if (!is.null(stop_after_n_splits) && !is.numeric(stop_after_n_splits)) {
    stop("If supplied, stop_after_n_splits must be a number.")
  }
  if (!output_format %in% c("parquet", "csv")) {
    stop('`output_format` must be either "parquet" or "csv".')
  }
  if (!is.character(working_directory)) {
    stop("Working directory must be a string or character.")
  }
  if (is.null(restore_input_data)) {
    stop("restore_input_data must be TRUE or FALSE.")
  } else if (!(restore_input_data == TRUE | restore_input_data == FALSE)) {
    stop("restore_input_data must be TRUE or FALSE.")
  }
  if (is.null(input_data_in_chunks)) {
    stop("input_data_in_chunks must be TRUE or FALSE.")
  } else if (!(input_data_in_chunks == TRUE | input_data_in_chunks == FALSE)) {
    stop("input_data_in_chunks must be TRUE or FALSE.")
  }
  if (is.null(final_chunk)) {
    stop("final_chunk must be TRUE or FALSE.")
  } else if (!(final_chunk == TRUE | final_chunk == FALSE)) {
    stop("final_chunk must be TRUE or FALSE.")
  }

  if (is.null(options) & !is.character(options_file_location)) {
    stop("Options file location must be a string or character.")
  }
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


  # Create folder to store results

  results_file_path <- file.path(working_directory, paste0("RCA_", options$study_codename, "_", site_code, "_",
                                                           convert_date_to_number(cycle_start_date), "-",
                                                           convert_date_to_number(cycle_end_date)))
  temp_file_path <- file.path(results_file_path, "temp")
  data_file_path <- file.path(results_file_path, "data")

  historical_aggregation <- FALSE

  if (!dir.exists(results_file_path)){
    if (is.null(person_data)) {
      stop("You must provide person data.")
    }
    if (is.null(vaccination_data)) {
      cat("Vaccination data not provided - treating as historical data aggregation.\n")
      historical_aggregation <- TRUE
    }
    if (is.null(outcome_data)) {
      stop("You must provide outcome data.")
    }
    cat("No existing results found in the working directory for this cycle period; a new batch of results will be created.\n")
    dir.create(results_file_path)
    dir.create(temp_file_path)
    dir.create(data_file_path)
    resuming_aggregation <- FALSE
    current_chunk <- 1
    cohort_size <- 0
    total_vaccinations <- 0
    first_vaccination_date <- NA_integer_
    last_vaccination_date <- NA_integer_
    total_outcomes <- 0
    first_outcome_date <- NA_integer_
    last_outcome_date <- NA_integer_
  } else {
    if (!dir.exists(temp_file_path)) {
      stop("Existing, completed results found in the working directory for this cycle period. To overwrite existing results, you should manually delete the results you would like to overwrite before running this function.")
    }
    if (!dir.exists(data_file_path) & input_data_in_chunks == FALSE) {
      stop("There is no input data stored in the specified folder. Aggregation can not be resumed, as input data may have changed. Delete temporary results and restart aggregation.")
    }

    cat("Existing, incomplete results found in the working directory for this cycle period; the existing batch will be resumed and completed, but not overwritten.\n")

    stored_parameters <- readRDS(file.path(temp_file_path, "stored_parameters.rds"))

    # Backward compat: old stored_parameters have participation_level but no design_selection
    if (is.null(stored_parameters$design_selection) && !is.null(stored_parameters$participation_level)) {
      old_level <- stored_parameters$participation_level
      stored_parameters$design_selection <- c("self_post")
      if (old_level > 1) stored_parameters$design_selection <- c(stored_parameters$design_selection, "self_pre", "historical", "concurrent_vac")
      if (old_level > 2) stored_parameters$design_selection <- c(stored_parameters$design_selection, "concurrent_unvac")
    }

    design_selection <- stored_parameters$design_selection
    # Backward compat: old stored_parameters have no output_format (those runs wrote CSV files)
    output_format <- rlang::`%||%`(stored_parameters$output_format, "csv")
    options <- stored_parameters$options
    suppression_limit <- stored_parameters$suppression_limit
    split_size <- stored_parameters$split_size
    total_splits <- stored_parameters$total_splits
    restore_input_data <- stored_parameters$restore_input_data
    historical_aggregation <- stored_parameters$historical_aggregation
    new_chunk <- stored_parameters$new_chunk
    current_chunk <- stored_parameters$current_chunk
    cohort_size <- stored_parameters$cohort_size
    total_vaccinations <- stored_parameters$total_vaccinations
    first_vaccination_date <- stored_parameters$first_vaccination_date
    last_vaccination_date <- stored_parameters$last_vaccination_date
    total_outcomes <- stored_parameters$total_outcomes
    first_outcome_date <- stored_parameters$first_outcome_date
    last_outcome_date <- stored_parameters$last_outcome_date
    lookback_start_date <- stored_parameters$lookback_start_date

    if (input_data_in_chunks == FALSE | new_chunk == FALSE) {
      resuming_aggregation <- TRUE
    } else {
      resuming_aggregation <- FALSE
      dir.create(data_file_path)
    }

    if (!(is.null(person_data) & is.null(vaccination_data) & is.null(outcome_data)) & (resuming_aggregation == TRUE)) {
      cat("The input data you have provided will not be used, as resuming aggregation relies on saved input data.\n")
    }

  }

  if (!resuming_aggregation) {

    cat("Validating input datasets...\n")

    # validate line list data
    validation_issues <- validate_input_data(person_data, vaccination_data, outcome_data,
                                             cycle_start_date, cycle_end_date,
                                             options_file_location = NULL,
                                             options,
                                             split_size,
                                             skip_user_prompts = skip_user_prompts)

    lookback_date_provided <- grepl("Lookback checked and start date provided: ", validation_issues)
    lookback_auto_warning  <- grepl("^Lookback warning \\(automated run\\):", validation_issues)

    if (any(lookback_date_provided)) {
      lookback_start_date <- check_date_format(stringr::str_replace(validation_issues[lookback_date_provided], "Lookback checked and start date provided: ", ""))
      validation_issues <- validation_issues[!lookback_date_provided]
    } else {
      lookback_start_date <- NA
    }

    if (any(lookback_auto_warning)) {
      validation_issues <- validation_issues[!lookback_auto_warning]
    }

    if (length(validation_issues) > 0) {
      if (input_data_in_chunks == FALSE) {
        unlink(results_file_path, recursive = TRUE)
      }
      stop(paste0("Issues with input data found during validation, aggregation aborted. Fix the issues outlined below before rerunning aggregation:\n",
                  stringr::str_c(stringr::str_c(1:length(validation_issues), validation_issues, sep = ". "), collapse = "\n")))
    }

    cat("Input data is valid.\n")

    basic_descriptives <- get_descriptives(person_data, vaccination_data, outcome_data,
                                           age_reference_date = cycle_start_date)

    cat("USER INPUT REQUIRED: Please check the basic descriptive statistics shown below: \n", basic_descriptives)

    if (!skip_user_prompts) {
      descriptives_check <- menu(
        choices = c("Everything looks good, continue", "Something looks wrong, stop")
      )
    } else {
      descriptives_check <- 1
    }

    if (descriptives_check == 2) {
      delete_folder_check <- menu(
        choices = c("Yes, delete", "No, do not delete"),
        title = "Aggregation will be aborted. Do you want to delete the results folder created for this cycle?"
      )
      if (delete_folder_check == 1) {
        unlink(results_file_path, recursive = TRUE)
        stop("Aggregation aborted due to issue identified in descriptive statistics. Results folder deleted.")
      } else {
        stop("Aggregation aborted due to issue identified in descriptive statistics. Results folder not deleted, and may interfere with future aggregation for this cycle.")
      }
    }

    # Split and store datasets
    total_splits <- ceiling(nrow(person_data)/split_size)
    cat(paste0("The input data will be split into ", total_splits, " parts.\n"))
    cat("Splitting and storing input datasets ")
    for (sp in 1:total_splits) {
      split_ids <- person_data$PID[((sp-1)*split_size+1):(sp*split_size)]
      fst::write_fst(person_data[PID %in% split_ids], file.path(data_file_path, paste0("person_data_split_", sp, ".fst")))
      fst::write_fst(outcome_data[PID %in% split_ids], file.path(data_file_path, paste0("outcome_data_split_", sp, ".fst")))
      if (!historical_aggregation) {
        fst::write_fst(vaccination_data[PID %in% split_ids], file.path(data_file_path, paste0("vaccination_data_split_", sp, ".fst")))
      }
      cat("#")
    }
    cat("\n")

    # Resolve the caller's variable names from the captured call args so that
    # recombine_input_data() can assign results back under the same names.
    # Falls back to positional argument order if named arguments were not used.
    person_data_input_name <- if (!is.null(input_names$person_data)) {
      as.character(input_names$person_data)
    } else if (!is.null(input_names$patient_data)) {
      as.character(input_names$patient_data)
    } else {
      as.character(input_names[[1]])
    }
    outcome_data_input_name <- ifelse(!is.null(input_names$outcome_data), as.character(input_names$outcome_data), as.character(input_names[[3]]))
    if (!historical_aggregation) {
      vaccination_data_input_name <- ifelse(!is.null(input_names$vaccination_data), as.character(input_names$vaccination_data), as.character(input_names[[2]]))
    }

    stored_parameters <- list(
      cycle_start_date = cycle_start_date,
      cycle_end_date = cycle_end_date,
      site_code = site_code,
      design_selection = design_selection,
      suppression_limit = suppression_limit,
      options = options,
      output_format = output_format,
      split_size = split_size,
      total_splits = total_splits,
      restore_input_data = restore_input_data,
      historical_aggregation = historical_aggregation,
      person_data_input_name = person_data_input_name,
      outcome_data_input_name = outcome_data_input_name,
      new_chunk = FALSE,
      current_chunk = current_chunk,
      cohort_size = cohort_size + nrow(person_data),
      total_outcomes = total_outcomes + nrow(outcome_data),
      first_outcome_date = min(first_outcome_date, pmin(outcome_data$EVENT_DATE), na.rm = TRUE),
      last_outcome_date = max(last_outcome_date, pmax(outcome_data$EVENT_DATE), na.rm = TRUE),
      aggregation_start_date = aggregation_start_date,
      lookback_start_date = lookback_start_date
    )

    if (!historical_aggregation) {
      stored_parameters$vaccination_data_input_name <- vaccination_data_input_name
      stored_parameters$total_vaccinations <- total_vaccinations + nrow(vaccination_data)
      stored_parameters$first_vaccination_date <- min(first_vaccination_date, pmin(vaccination_data$V_DATE), na.rm = TRUE)
      stored_parameters$last_vaccination_date <- max(last_vaccination_date, pmax(vaccination_data$V_DATE), na.rm = TRUE)
    }

    saveRDS(stored_parameters, file = file.path(temp_file_path, "stored_parameters.rds"))

    rm(list = c("person_data", "outcome_data"))

    rm(list = c(person_data_input_name, outcome_data_input_name), envir = parent.frame())

    if (!historical_aggregation) {
      rm(list = c("vaccination_data"))

      vaccination_data_input_name <- ifelse(!is.null(input_names$vaccination_data), as.character(input_names$vaccination_data), as.character(input_names[[2]]))
      rm(list = c(vaccination_data_input_name), envir = parent.frame())
    }

    gc()
  }

  # Aggregated tables (validation of aggregated data needs to occur within aggregation functions)

  time_periods <- split_time_periods(cycle_start_date, cycle_end_date, options$period_length,
                                     align_periods = options$align_periods)

  cat(paste0("This cycle contains ", nrow(time_periods), " sub-period(s).\n"))

  if (is.null(stop_after_n_splits)) {
    max_split <- total_splits
  } else {
    max_split <- min(total_splits, stop_after_n_splits)
  }

  sp <- 1

  while (sp <= max_split) {

    cat(paste0("Aggregating split ", sp, " "))

    person_data <- fst::read_fst(file.path(data_file_path, paste0("person_data_split_", sp, ".fst")), as.data.table = TRUE)
    outcome_data <- fst::read_fst(file.path(data_file_path, paste0("outcome_data_split_", sp, ".fst")), as.data.table = TRUE)
    if (!historical_aggregation) {
      vaccination_data <- fst::read_fst(file.path(data_file_path, paste0("vaccination_data_split_", sp, ".fst")), as.data.table = TRUE)
    }

    # --- Prepare split data for aggregation ---

    # Convert stored 8-digit integer dates back to Date objects for all calculations.
    person_data[, c("DOB", "ENROL_DATE", "CENSOR_DATE") := lapply(.SD, convert_number_to_date), .SDcols = c("DOB", "ENROL_DATE", "CENSOR_DATE")]
    # NA enrolment dates default to date of birth (eligible from birth).
    person_data[, ENROL_DATE := fifelse(is.na(ENROL_DATE), DOB, ENROL_DATE)]

    if (!is.na(lookback_start_date)) {
      # If the user confirmed a lookback start date during validation, push each person's
      # effective enrolment date forward to that date so that outcomes before it are excluded
      # from person-days calculations.
      person_data[, ENROL_DATE := pmax(ENROL_DATE, lookback_start_date, na.rm = TRUE)]
    }

    outcome_data[, EVENT_DATE := convert_number_to_date(EVENT_DATE)]
    outcome_data <- apply_clean_window(outcome_data, options$outcome_info[, .(AESI, clean_window)])

    if (!historical_aggregation) {
      vaccination_data[, V_DATE := convert_number_to_date(V_DATE)]

      vaccination_data <- map_subtypes_to_type(vaccination_data, options$vaccine_info)
      vaccination_data[, ':=' (V_SUBTYPE_original = V_SUBTYPE, V_TYPE_original = V_TYPE)]
      setkey(vaccination_data, PID, V_DATE)

      # Pre-compute dose counters once per split rather than inside each period loop.
      # Each sub-analysis uses a different dose-numbering reference (all doses, by type, by subtype).
      if ("subgroup_dose" %in% options$included_analyses) {
        vaccination_data[, V_DOSE_disease := 1:.N, by = .(PID)]
      }
      if ("subgroup_platform_dose" %in% options$included_analyses) {
        vaccination_data[, V_DOSE_platform := 1:.N, by = .(PID, V_TYPE)]
      }
      if ("subgroup_brand_dose" %in% options$included_analyses) {
        vaccination_data[, V_DOSE_brand := 1:.N, by = .(PID, V_SUBTYPE)]
      }
    }

    setkey(person_data, PID)
    setkey(outcome_data, PID)

    for (period_id in 1:nrow(time_periods)) {

      period_start <- time_periods$period_start[period_id]
      period_end <- time_periods$period_end[period_id]

      person_data_period <- person_data[(is.na(CENSOR_DATE) | CENSOR_DATE >= period_start) &
                                            (is.na(ENROL_DATE) | ENROL_DATE <= period_end)]

      person_data_period[
        , age_years := pmax(floor(as.numeric(period_start - DOB)/365.25), 0)
      ][
        , AGE_GROUP := cut(age_years, breaks = options$age_groups$bounds, labels = options$age_groups$labels, right = FALSE)
      ][
        , age_years := NULL
      ]

      if (!historical_aggregation) {

        if (!file.exists(file.path(temp_file_path, paste0("data_descriptive_outcomes_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))) {

          data_descriptive_outcomes <- aggregate_data_descriptive_outcomes(person_data_period, outcome_data,
                                                                           period_start, period_end,
                                                                           options)

          fst::write_fst(data_descriptive_outcomes, file.path(temp_file_path, paste0("data_descriptive_outcomes_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
        }

        if (!file.exists(file.path(temp_file_path, paste0("data_descriptive_vaccinations_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))) {

          data_descriptive_vaccinations <- aggregate_data_descriptive_vaccinations(person_data_period, vaccination_data,
                                                                                   period_start, period_end,
                                                                                   options,
                                                                                   analysis_types = options$included_analyses)

          fst::write_fst(data_descriptive_vaccinations, file.path(temp_file_path, paste0("data_descriptive_vaccinations_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
        }

        if ("self_post" %in% design_selection) {

          if (!file.exists(file.path(temp_file_path, paste0("data_self_post_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))) {

            data_self_post <- aggregate_data_self(person_data_period, vaccination_data, outcome_data,
                                                  period_start, period_end,
                                                  options,
                                                  comparator = "post",
                                                  analysis_types = options$included_analyses)

            fst::write_fst(data_self_post, file.path(temp_file_path, paste0("data_self_post_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
          }
        }

        if ("self_pre" %in% design_selection) {

          if (!file.exists(file.path(temp_file_path, paste0("data_self_pre_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))) {

            data_self_pre <- aggregate_data_self(person_data_period, vaccination_data, outcome_data,
                                                 period_start, period_end,
                                                 options,
                                                 comparator = "pre",
                                                 analysis_types = options$included_analyses)

            fst::write_fst(data_self_pre, file.path(temp_file_path, paste0("data_self_pre_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
          }
        }

        if ("historical" %in% design_selection) {

          if (!file.exists(file.path(temp_file_path, paste0("data_exposed_cases_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst"))) |
              !file.exists(file.path(temp_file_path, paste0("data_exposed_person_days_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))) {

            data_exposed <- aggregate_data_exposed(person_data_period, vaccination_data, outcome_data,
                                                   period_start, period_end,
                                                   options,
                                                   analysis_types = options$included_analyses)

            fst::write_fst(data_exposed$cases, file.path(temp_file_path, paste0("data_exposed_cases_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
            fst::write_fst(data_exposed$person_days, file.path(temp_file_path, paste0("data_exposed_person_days_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
          }
        }

        if ("concurrent_vac" %in% design_selection) {

          if (!file.exists(file.path(temp_file_path, paste0("data_concurrent_vac_control_counts_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))) {

            data_concurrent_vac <- aggregate_data_concurrent(person_data_period, vaccination_data, outcome_data,
                                                             period_start, period_end,
                                                             options,
                                                             comparator = "vaccinated",
                                                             analysis_types = options$included_analyses,
                                                             return_preaggregated = TRUE)

            fst::write_fst(data_concurrent_vac$control_group, file.path(temp_file_path, paste0("data_concurrent_vac_control_group_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
            fst::write_fst(data_concurrent_vac$risk_group, file.path(temp_file_path, paste0("data_concurrent_vac_risk_group_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
            fst::write_fst(data_concurrent_vac$case_counts, file.path(temp_file_path, paste0("data_concurrent_vac_case_counts_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
            fst::write_fst(data_concurrent_vac$control_counts, file.path(temp_file_path, paste0("data_concurrent_vac_control_counts_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
          }

        }

        if ("concurrent_unvac" %in% design_selection) {

          if (!file.exists(file.path(temp_file_path, paste0("data_concurrent_unvac_control_counts_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))) {

            data_concurrent_unvac <- aggregate_data_concurrent(person_data_period, vaccination_data, outcome_data,
                                                               period_start, period_end,
                                                               options,
                                                               comparator = "unvaccinated",
                                                               analysis_types = options$included_analyses,
                                                               return_preaggregated = TRUE)

            fst::write_fst(data_concurrent_unvac$control_group, file.path(temp_file_path, paste0("data_concurrent_unvac_control_group_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
            fst::write_fst(data_concurrent_unvac$risk_group, file.path(temp_file_path, paste0("data_concurrent_unvac_risk_group_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
            fst::write_fst(data_concurrent_unvac$case_counts, file.path(temp_file_path, paste0("data_concurrent_unvac_case_counts_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
            fst::write_fst(data_concurrent_unvac$control_counts, file.path(temp_file_path, paste0("data_concurrent_unvac_control_counts_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
          }
        }

      } else {

        if (!file.exists(file.path(temp_file_path, paste0("data_historic_cases_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst"))) |
            !file.exists(file.path(temp_file_path, paste0("data_historic_person_days_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))) {

          data_historic <- aggregate_data_historic(person_data_period, outcome_data,
                                                   period_start, period_end,
                                                   options)

          fst::write_fst(data_historic$cases, file.path(temp_file_path, paste0("data_historic_cases_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
          fst::write_fst(data_historic$person_days, file.path(temp_file_path, paste0("data_historic_person_days_chunk_", stored_parameters$current_chunk, "_period_", period_id, "_split_", sp, ".fst")))
        }
      }

    }

    gc()

    cat("\n")

    if (sp == total_splits) {
      if (input_data_in_chunks == FALSE | final_chunk == TRUE) {

        cat(paste0("Combining sub-period and split data and removing temporary results.\n"))
        combine_temp_results(design_selection, results_file_path, options, historical_aggregation, output_format)

        # include notes sheet containing parameters used
        create_notes_file(cycle_start_date, cycle_end_date,
                          site_code,
                          design_selection,
                          options,
                          stored_parameters,
                          results_file_path)

        # suppression
        if (!is.null(suppression_limit)) {
          cat(paste0("Suppressing counts below ", suppression_limit, ".\n"))
          file_pattern <- if (output_format == "parquet") "\\.parquet$" else "\\.csv$"
          files_to_suppress <- grep(file_pattern, list.files(results_file_path), value = TRUE)
          for (file_name in files_to_suppress) {
            full_path <- file.path(results_file_path, file_name)
            data <- read_output_file(full_path, output_format)
            suppress_low_counts(data, suppression_limit)
            write_output_file(data, full_path, output_format)
          }
        }

      } else {
        stored_parameters <- readRDS(file.path(temp_file_path, "stored_parameters.rds"))
        stored_parameters$new_chunk <- TRUE
        stored_parameters$current_chunk <- stored_parameters$current_chunk + 1
        saveRDS(stored_parameters, file = file.path(temp_file_path, "stored_parameters.rds"))
      }
    }

    sp <- sp + 1
  }

  if (restore_input_data) {
    cat("Restoring input datasets to global environment.\n")
    # Fall back to old key name for stored_parameters created by previous package versions
    restore_person_name <- rlang::`%||%`(stored_parameters$person_data_input_name,
                                         stored_parameters$patient_data_input_name)
    if (!stored_parameters$historical_aggregation) {
      recombine_input_data(data_file_path,
                           person_data_input_name = restore_person_name,
                           vaccination_data_input_name = stored_parameters$vaccination_data_input_name,
                           outcome_data_input_name = stored_parameters$outcome_data_input_name)
    } else {
      recombine_input_data(data_file_path,
                           person_data_input_name = restore_person_name,
                           vaccination_data_input_name = NULL,
                           outcome_data_input_name = stored_parameters$outcome_data_input_name)
    }
  }


  if (is.null(stop_after_n_splits) | max_split == total_splits) {
    unlink(data_file_path, recursive = TRUE)
    cat("Data aggregation complete: ")
  } else {
    cat(paste0("Data aggregation stopped after ", stop_after_n_splits, " splits. Input datasets have been stored and are not required as inputs when resuming aggregation in the future.\n"))
  }
  time2 <- Sys.time()
  print(time2 - time1)

}

#' Aggregate AESI outcomes
#'
#' @description
#' Aggregate number of AESI outcomes in a given period of time, regardless of vaccination status.
#'
#' @inheritParams aggregate_data
#' @param period_start Start date of period, as a date object. Period lengths are defined in the options file, with each cycle possibly containing multiple periods. Only a single period is aggregated at a time.
#' @param period_end End date of period, as a date object. Period lengths are defined in the options file, with each cycle possibly containing multiple periods. Only a single period is aggregated at a time.
#' @param options Options object, already converted from txt file.
#' @param allow_side_effects Whether or not changes made to the original input data should be allowed to persist after the function has completed.
#'
#' @return A data.table object matching the 'Descriptive - AESI' template, describing the AESI outcome counts for this period.
aggregate_data_descriptive_outcomes <- function(person_data, outcome_data,
                                                period_start, period_end,
                                                options,
                                                allow_side_effects = FALSE) {

  side_effect_age <- 0

  if (!"AGE_GROUP" %in% names(person_data)) {
    person_data[
      , age_years := pmax(floor(as.numeric(period_start - DOB)/365.25), 0)
    ][
      , AGE_GROUP := cut(age_years, breaks = options$age_groups$bounds, labels = options$age_groups$labels, right = FALSE)
    ][
      , age_years := NULL
    ]
    side_effect_age <- 1
  }

  outcomes_period <- person_data[
    outcome_data[EVENT_DATE >= period_start & EVENT_DATE <= period_end], on = .(PID)
  ][
    , ':=' (PERIOD_START = period_start, PERIOD_END = period_end)
  ][
    , .(COUNT = .N), by = .(PERIOD_START, PERIOD_END, ENCOUNTER_TYPE, AESI, AGE_GROUP, SEX)
  ]

  if (!allow_side_effects & side_effect_age == 1) {
    person_data[, AGE_GROUP := NULL]
  }

  return(outcomes_period)

}

#' Aggregate vaccinations
#'
#' @description
#' Aggregate number of vaccinations in a given period of time
#'
#' @inheritParams aggregate_data
#' @inheritParams aggregate_data_descriptive_outcomes
#' @param analysis_types Character vector defining which of the primary and sub-analyses are included in this study (from options file)
#'
#' @return A data.table object matching the 'Descriptive - Vaccinations' template, describing the vaccination counts for this period.
aggregate_data_descriptive_vaccinations <- function(person_data, vaccination_data,
                                                    period_start, period_end,
                                                    options,
                                                    allow_side_effects = FALSE,
                                                    analysis_types = NULL) {

  side_effect_platform <- 0
  side_effect_dose <- 0
  side_effect_age <- 0

  dose_columns <- grep("V_DOSE", names(vaccination_data), value = TRUE)

  if (!"V_TYPE" %in% names(vaccination_data)) {
    vaccination_data <- map_subtypes_to_type(vaccination_data, options$vaccine_info)
    side_effect_platform <- 1
  }

  if (length(dose_columns) < 1) {
    vaccination_data <- calc_vaccine_dose(vaccination_data, reference = "dose")
    side_effect_dose <- 1
  }

  if (!"AGE_GROUP" %in% names(person_data)) {
    person_data[
      , age_years := pmax(floor(as.numeric(period_start - DOB)/365.25), 0)
    ][
      , AGE_GROUP := cut(age_years, breaks = options$age_groups$bounds, labels = options$age_groups$labels, right = FALSE)
    ][
      , age_years := NULL
    ]
    side_effect_age <- 1
  }

  vaccinations_period_all_analyses <- person_data[
    vaccination_data[V_DATE >= period_start & V_DATE <= period_end], on = .(PID)
  ][
    , ':=' (PERIOD_START = period_start, PERIOD_END = period_end)
  ][
    , .(COUNT = .N), by = c("PERIOD_START", "PERIOD_END", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", dose_columns)
  ]

  if (is.null(analysis_types)) {

    vaccinations_period_out <- vaccinations_period_all_analyses

  } else {

    vaccinations_period_out <- rbindlist(lapply(analysis_types, function(analysis_type) {
      if (analysis_type == "primary") {
        brand_val <- "pooled"; platform_val <- "pooled"; dose_col <- NULL
      } else if (analysis_type == "subgroup_dose") {
        brand_val <- "pooled"; platform_val <- "pooled"; dose_col <- "V_DOSE_disease"
      } else if (analysis_type == "subgroup_platform") {
        brand_val <- "pooled"; platform_val <- NULL; dose_col <- NULL
      } else if (analysis_type == "subgroup_platform_dose") {
        brand_val <- "pooled"; platform_val <- NULL; dose_col <- "V_DOSE_platform"
      } else if (analysis_type == "subgroup_brand") {
        brand_val <- NULL; platform_val <- NULL; dose_col <- NULL
      } else if (analysis_type == "subgroup_brand_dose") {
        brand_val <- NULL; platform_val <- NULL; dose_col <- "V_DOSE_brand"
      } else {
        stop("Analysis type not recognised.")
      }

      dt <- vaccinations_period_all_analyses[, .(
        PERIOD_START = PERIOD_START,
        PERIOD_END = PERIOD_END,
        AGE_GROUP = AGE_GROUP,
        SEX = SEX,
        V_SUBTYPE = if (!is.null(brand_val)) brand_val else V_SUBTYPE,
        V_TYPE = if (!is.null(platform_val)) platform_val else V_TYPE,
        V_DOSE = if (!is.null(dose_col)) get(dose_col) else -1L,
        COUNT = COUNT
      )]
      dt[, .(COUNT = sum(COUNT)), by = c("PERIOD_START", "PERIOD_END", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE")]
    }))
  }

  if (!allow_side_effects) {
    if (side_effect_age == 1) {
      person_data[, AGE_GROUP := NULL]
    }
    if (side_effect_platform == 1) {
      vaccination_data[, V_TYPE := NULL]
    }
    if (side_effect_dose == 1) {
      vaccination_data[, V_DOSE := NULL]
    }
  }

  return(vaccinations_period_out)

}

#' Aggregate historic data
#'
#' @description
#' Aggregate historic AESI counts and person days in a given period of time, not dependent on vaccination status.
#'
#'
#' @inheritParams aggregate_data_descriptive_outcomes
#'
#' @return A list of two data.table objects, named 'cases' and person_days', matching the 'Historic cases' and 'Historic person days' templates respectively.
aggregate_data_historic <- function(person_data, outcome_data,
                                    period_start, period_end,
                                    options,
                                    allow_side_effects = FALSE) {

  side_effect_age <- 0

  if (!"AGE_GROUP" %in% names(person_data)) {
    person_data[
      , age_years := pmax(floor(as.numeric(period_start - DOB)/365.25), 0)
    ][
      , AGE_GROUP := cut(age_years, breaks = options$age_groups$bounds, labels = options$age_groups$labels, right = FALSE)
    ][
      , age_years := NULL
    ]
    side_effect_age <- 1
  }

  historic_cases <- person_data[
    outcome_data[EVENT_DATE >= period_start & EVENT_DATE <= period_end], on = .(PID)
  ][
    , ':=' (PERIOD_START = period_start, PERIOD_END = period_end)
  ][
    , .(COUNT = .N), by = .(PERIOD_START, PERIOD_END, ENCOUNTER_TYPE, AESI, AGE_GROUP, SEX)
  ]

  historic_person_days <- copy(person_data)[
    , PERSON_DAYS := pmax(as.numeric(period_end - period_start) + 1 - pmax(as.numeric(ENROL_DATE - period_start), 0) - pmax(as.numeric(period_end - CENSOR_DATE), 0, na.rm = TRUE), 0)
  ][
    , ':=' (PERIOD_START = period_start, PERIOD_END = period_end)
  ][
    , .(PERSON_DAYS = sum(PERSON_DAYS)), by = .(PERIOD_START, PERIOD_END, AGE_GROUP, SEX)
  ]

  if (!allow_side_effects & side_effect_age == 1) {
    person_data[, AGE_GROUP := NULL]
  }

  return(list(cases = historic_cases, person_days = historic_person_days))

}

#' Aggregate post-vaccination exposure data
#'
#' @description
#' Aggregate AESI counts during post-vaccination risk intervals and person days spent in those intervals, within a given period of time.
#'
#' @inheritParams aggregate_data
#' @inheritParams aggregate_data_descriptive_vaccinations
#'
#' @return A list of two data.table objects, named 'cases' and person_days', matching the 'Exposed cases' and 'Exposed person days' templates respectively.
aggregate_data_exposed <- function(person_data, vaccination_data, outcome_data,
                                   period_start, period_end,
                                   options,
                                   allow_side_effects = FALSE,
                                   analysis_types = NULL) {

  side_effect_platform <- 0
  side_effect_dose <- 0
  side_effect_age <- 0

  # These columns may already exist if the main aggregation loop pre-computed them for
  # this split. The existence check avoids redundant computation across period iterations.
  if (!"V_TYPE" %in% names(vaccination_data)) {
    vaccination_data <- map_subtypes_to_type(vaccination_data, options$vaccine_info)
    side_effect_platform <- 1
  }

  if (!"V_DOSE" %in% names(vaccination_data)) {
    vaccination_data <- calc_vaccine_dose(vaccination_data, reference = "dose")
    side_effect_dose <- 1
  }

  if (!"AGE_GROUP" %in% names(person_data)) {
    person_data[
      , age_years := pmax(floor(as.numeric(period_start - DOB)/365.25), 0)
    ][
      , AGE_GROUP := cut(age_years, breaks = options$age_groups$bounds, labels = options$age_groups$labels, right = FALSE)
    ][
      , age_years := NULL
    ]
    side_effect_age <- 1
  }

  risk_periods <- options$outcome_info[, .(risk_period = stringr::str_c(risk_lower, risk_upper, sep = ":")),
                                       by = .(AESI)]

  unique_risk_periods <- unique(risk_periods$risk_period)

  vaccinations_current_period <- risk_periods[
    rbindlist(
      lapply(unique_risk_periods,
             function(x) {
               risk_values <- as.numeric(stringr::str_split(x, ":", simplify = TRUE))

               person_data[
                 vaccination_data[PID %in% person_data$PID & V_DATE %between% c(period_start - risk_values[2], period_end)],
                 on = .(PID)
               ][
                 order(PID, V_DATE)
               ][
                 , ':=' (risk_period = x,
                         risk_start = V_DATE + risk_values[1],
                         risk_end = V_DATE + risk_values[2])
               ][
                 , ':=' (risk_start_next = shift(risk_start, type = "lead"),
                         PID_next = shift(PID, type = "lead"))
               ][
                 , risk_end := fifelse(!is.na(PID_next) & (PID_next == PID & risk_end >= risk_start_next),
                                       pmax(risk_start, risk_start_next - 1, na.rm = TRUE), risk_end)
               ][
                 , ':=' (risk_start_next = NULL, PID_next = NULL)
               ][
                 , ':=' (risk_start = pmin(risk_start, CENSOR_DATE, period_end, na.rm = TRUE),
                         risk_end = pmin(risk_end, CENSOR_DATE, period_end, na.rm = TRUE))
               ]
             }
      )
    ), on = .(risk_period), allow.cartesian = TRUE
  ]

  dose_columns <- grep("V_DOSE", names(vaccinations_current_period), value = TRUE)

  exposed_cases_all_analyses <- vaccinations_current_period[
    outcome_data[EVENT_DATE >= period_start & EVENT_DATE <= period_end], on = .(PID, AESI)
  ][
    !is.na(V_DATE) & between(EVENT_DATE, risk_start, risk_end, NAbounds = FALSE)
  ][
    , ':=' (PERIOD_START = period_start, PERIOD_END = period_end)
  ][
    , .(CASE_COUNT = .N), by = c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", dose_columns)
  ]

  exposed_person_days_all_analyses <- vaccinations_current_period[
    , PERSON_DAYS := pmax(as.numeric(period_end - period_start) + 1 - pmax(as.numeric(risk_start - period_start), 0) - pmax(as.numeric(period_end - risk_end), 0), 0)
  ][
    , ':=' (PERIOD_START = period_start, PERIOD_END = period_end)
  ][
    , .(PERSON_DAYS = sum(PERSON_DAYS)), by = c("PERIOD_START", "PERIOD_END", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", dose_columns)
  ][
    PERSON_DAYS > 0
  ]

  if (is.null(analysis_types)) {

    exposed_cases_out <- exposed_cases_all_analyses
    exposed_person_days_out <- exposed_person_days_all_analyses

  } else {

    remap_analysis <- function(dt, analysis_type, value_col, by_cols) {
      if (analysis_type == "primary") {
        brand_val <- "pooled"; platform_val <- "pooled"; dose_col <- NULL
      } else if (analysis_type == "subgroup_dose") {
        brand_val <- "pooled"; platform_val <- "pooled"; dose_col <- "V_DOSE_disease"
      } else if (analysis_type == "subgroup_platform") {
        brand_val <- "pooled"; platform_val <- NULL; dose_col <- NULL
      } else if (analysis_type == "subgroup_platform_dose") {
        brand_val <- "pooled"; platform_val <- NULL; dose_col <- "V_DOSE_platform"
      } else if (analysis_type == "subgroup_brand") {
        brand_val <- NULL; platform_val <- NULL; dose_col <- NULL
      } else if (analysis_type == "subgroup_brand_dose") {
        brand_val <- NULL; platform_val <- NULL; dose_col <- "V_DOSE_brand"
      } else {
        stop("Analysis type not recognised.")
      }
      remapped <- dt[, c(by_cols, value_col), with = FALSE]
      if (!is.null(brand_val)) set(remapped, j = "V_SUBTYPE", value = brand_val)
      if (!is.null(platform_val)) set(remapped, j = "V_TYPE", value = platform_val)
      if (!is.null(dose_col)) {
        set(remapped, j = "V_DOSE", value = dt[[dose_col]])
      } else {
        set(remapped, j = "V_DOSE", value = -1L)
      }
      remapped[, lapply(.SD, sum), by = by_cols, .SDcols = value_col]
    }

    cases_by_cols <- c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE")
    pd_by_cols <- c("PERIOD_START", "PERIOD_END", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE")

    exposed_cases_out <- rbindlist(lapply(analysis_types, function(at) {
      remap_analysis(exposed_cases_all_analyses, at, "CASE_COUNT", cases_by_cols)
    }))

    exposed_person_days_out <- rbindlist(lapply(analysis_types, function(at) {
      remap_analysis(exposed_person_days_all_analyses, at, "PERSON_DAYS", pd_by_cols)[PERSON_DAYS > 0]
    }))
  }


  if (!allow_side_effects) {
    if (side_effect_age == 1) {
      person_data[, AGE_GROUP := NULL]
    }
    if (side_effect_platform == 1) {
      vaccination_data[, V_TYPE := NULL]
    }
    if (side_effect_dose == 1) {
      vaccination_data[, V_DOSE := NULL]
    }
  }

  cat("#")

  return(list(cases = exposed_cases_out, person_days = exposed_person_days_out))

}

#' Aggregate data with concurrent comparators
#'
#' @description
#' Aggregate risk/control AESI counts and ratio according to concurrent study designs, within a given period of time.
#'
#' @inheritParams aggregate_data_exposed
#' @param comparator Specify whether a 'vaccinated' or 'unvaccinated' comparator group is used
#' @param return_preaggregated Due to the calculation of ratios and assignment of controls, two fully aggregated tables can not be combined under this study design.
#' When TRUE, the function will not combine risk/control groups and calculate the ratio, but instead return these objects separately in a list. This allows the majority
#' of the aggregation to be performed in stages, before combining and calculating the ratio at the end (using `concurrent_final_aggregation()` function)
#'
#' @return If `return_preaggregated = FALSE`, returns a data.table object matching the 'Concurrent - Vac' or 'Concurrent - Unvac' templates.
#' If `return_preaggregated = TRUE`, returns a list of four data.table objects corresponding to the control/risk counts and control/risk groups, which are required to produce the final table.
aggregate_data_concurrent <- function(person_data, vaccination_data, outcome_data,
                                      period_start, period_end,
                                      options,
                                      comparator = "vaccinated",
                                      allow_side_effects = FALSE,
                                      analysis_types = NULL,
                                      return_preaggregated = FALSE) {

  stopifnot("Input `comparator` must either be 'vaccinated' or 'unvaccinated'" = comparator %in% c("vaccinated", "unvaccinated"))

  side_effect_first_vac <- 0
  side_effect_platform <- 0
  side_effect_dose <- 0
  side_effect_age <- 0

  # These columns may already exist if the main aggregation loop pre-computed them for
  # this split. The existence check avoids redundant computation across period iterations.
  if (!"first_vac" %in% names(vaccination_data)) {
    vaccination_data <- find_first_vac(vaccination_data)
    side_effect_first_vac <- 1
  }

  if (!"V_TYPE" %in% names(vaccination_data)) {
    vaccination_data <- map_subtypes_to_type(vaccination_data, options$vaccine_info)
    side_effect_platform <- 1
  }

  if (!"V_DOSE" %in% names(vaccination_data)) {
    vaccination_data <- calc_vaccine_dose(vaccination_data, reference = "dose")
    side_effect_dose <- 1
  }

  if (!"AGE_GROUP" %in% names(person_data)) {
    person_data[
      , age_years := pmax(floor(as.numeric(period_start - DOB)/365.25), 0)
    ][
      , AGE_GROUP := cut(age_years, breaks = options$age_groups$bounds, labels = options$age_groups$labels, right = FALSE)
    ][
      , age_years := NULL
    ]
    side_effect_age <- 1
  }

  risk_washout_periods <- options$outcome_info[, .(risk_washout = stringr::str_c(risk_lower, risk_upper, washout_post, sep = ":")),
                                               by = .(AESI)]

  unique_risk_washout <- unique(risk_washout_periods$risk_washout)

  if (return_preaggregated) {
    output <- list(combined_groups = data.table(), control_counts = data.table(), case_counts = data.table())
  } else {
    output <- data.table()
  }

  vaccinations_period <- person_data[
    vaccination_data[PID %in% person_data$PID &
                       V_DATE >= period_start - max(options$outcome_info$risk_upper) - max(options$outcome_info$washout_post) &
                       V_DATE <= period_end],
    on = .(PID)
  ][
    , ':=' (PERIOD_START = period_start,
            PERIOD_END = period_end)
  ][
    order(PID, V_DATE)
  ]

  if (comparator == "vaccinated") {

    already_vaccinated_period <- unique(person_data[
      vaccination_data[PID %in% person_data$PID & !PID %in% vaccinations_period$PID & first_vac < period_start],
      on = .(PID)
    ][
      , ':=' (PERIOD_START = period_start,
              PERIOD_END = period_end)
    ][
      , .(PID, DOB, SEX, ENROL_DATE, CENSOR_DATE, CENSOR_TYPE, AGE_GROUP, first_vac, PERIOD_START, PERIOD_END)
    ][
      order(PID)
    ])

    not_yet_vaccinated_period <- data.table()

  } else {

    not_yet_vaccinated_period <- unique(merge(person_data, vaccination_data, by = "PID", all = TRUE, allow.cartesian = TRUE)[
      PID %in% person_data$PID & (is.na(V_DATE) | first_vac > period_end)
    ][
      , ':=' (PERIOD_START = period_start,
              PERIOD_END = period_end)
    ][
      , .(PID, DOB, SEX, ENROL_DATE, CENSOR_DATE, CENSOR_TYPE, AGE_GROUP, first_vac, PERIOD_START, PERIOD_END)
    ][
      order(PID)
    ])

    already_vaccinated_period <- data.table()
  }

  for (risk_washout_label in unique_risk_washout) {

    risk_washout_values <- as.numeric(stringr::str_split(risk_washout_label, ":", simplify = TRUE))

    applicable_aesi <- risk_washout_periods[risk_washout == risk_washout_label]$AESI

    vaccinations_period[
      , ':=' (risk_washout = risk_washout_label,
              risk_start = V_DATE + risk_washout_values[1],
              risk_end = V_DATE + risk_washout_values[2],
              washout_end = V_DATE + risk_washout_values[2] + risk_washout_values[3])
    ][
      , ':=' (risk_start_next = shift(risk_start, type = "lead"),
              PID_next = shift(PID, type = "lead"),
              PID_prev = shift(PID, type = "lag"))
    ][
      , risk_end := fifelse(!is.na(PID_next) & (PID_next == PID & risk_end >= risk_start_next),
                            pmax(risk_start, risk_start_next - 1, na.rm = TRUE), risk_end),
    ][
      , washout_end := fifelse(!is.na(PID_next) & (PID_next == PID & washout_end >= risk_start_next),
                               pmax(risk_end, risk_start_next - 1, na.rm = TRUE), washout_end),
    ][
      , ':=' (risk_start = pmin(risk_start, CENSOR_DATE, period_end, na.rm = TRUE),
              risk_end = pmin(risk_end, CENSOR_DATE, period_end, na.rm = TRUE),
              washout_end = pmin(washout_end, CENSOR_DATE, period_end, na.rm = TRUE))
    ]

    already_vaccinated_period[, risk_washout := risk_washout_label]
    not_yet_vaccinated_period[, risk_washout := risk_washout_label]

    combined_period <- rbindlist(list(vaccinations_period, already_vaccinated_period, not_yet_vaccinated_period),
                                 fill = TRUE)[!is.na(PID)]

    case_controls <- outcome_data[EVENT_DATE >= period_start & EVENT_DATE <= period_end][
      AESI %in% applicable_aesi
    ][
      combined_period, on = .(PID)
    ][
      !is.na(EVENT_DATE)
    ][
      , flag_risk := !is.na(risk_start) & between(EVENT_DATE, risk_start, risk_end, NAbounds = FALSE)
    ][
      , flag_control := (comparator == "vaccinated" & !flag_risk &
                           (is.na(risk_start) |
                              (first_vac < period_start & (is.na(PID_prev) | PID_prev != PID) & EVENT_DATE < risk_start) |
                              (EVENT_DATE > washout_end & (is.na(PID_next) | PID_next != PID | EVENT_DATE < risk_start_next)))) |
        (comparator == "unvaccinated" & !flag_risk & (is.na(PID_prev) | PID_prev != PID) & (is.na(first_vac) | EVENT_DATE < first_vac))
    ][
      , slice_date := EVENT_DATE
    ]

    dose_columns <- grep("V_DOSE", names(case_controls), value = TRUE)

    case_counts <- case_controls[
      !is.na(V_DATE)
    ][
      , .(CASE_COUNT = sum(flag_risk)),
      by = c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", dose_columns, "slice_date", "risk_washout")
    ][
      CASE_COUNT > 0
    ]

    control_counts <- case_controls[
      , .(CONTROL_COUNT = sum(flag_control)),
      by = .(PERIOD_START, PERIOD_END, ENCOUNTER_TYPE, AESI, AGE_GROUP, SEX, slice_date, risk_washout)
    ][
      CONTROL_COUNT > 0
    ]

    # Enumerate every day in the period so the risk/control group sizes are
    # calculated on each calendar day, enabling daily-matched ratio computation.
    slice_dates <- seq(from = period_start, to = period_end, by = "day")

    risk_group <- rbindlist(
      lapply(slice_dates,
             function(x) {
               vaccinations_period[
                 , slice_date := x
               ][
                 , flag_risk := between(x, risk_start, risk_end, NAbounds = FALSE)
               ][
                 , .(n_at_risk = sum(flag_risk)),
                 by = c("PERIOD_START", "PERIOD_END", "risk_washout", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", dose_columns, "slice_date")
               ][
                 n_at_risk > 0
               ]
             })
    )

    control_group <- rbindlist(
      lapply(slice_dates,
             function(x) {
               combined_period[
                 , slice_date := x
               ][
                 , flag_control := x >= ENROL_DATE & (is.na(CENSOR_DATE) | x <= CENSOR_DATE) &
                   ((comparator == "vaccinated" &
                       (is.na(risk_start) |
                          (first_vac < period_start & (is.na(PID_prev) | PID_prev != PID) & x < risk_start) |
                          (x > washout_end & (is.na(PID_next) | PID_next != PID | x < risk_start_next)))) |
                      (comparator == "unvaccinated" & (is.na(PID_prev) | PID_prev != PID) & (is.na(first_vac) | x < first_vac)))
               ][
                 , .(n_in_control = sum(flag_control)),
                 by = .(PERIOD_START, PERIOD_END, risk_washout, AGE_GROUP, SEX, slice_date)
               ][
                 n_in_control > 0
               ]
             })
    )

    if (return_preaggregated) {

      output$control_group <- rbindlist(list(output$control_group, control_group))
      output$risk_group <- rbindlist(list(output$risk_group, risk_group))
      output$control_counts <- rbindlist(list(output$control_counts, control_counts))
      output$case_counts <- rbindlist(list(output$case_counts, case_counts))

    } else {

      output <- concurrent_final_aggregation(control_group, risk_group, control_counts, case_counts, output, analysis_types, options)
    }
  }

  if (!allow_side_effects) {
    if (side_effect_age == 1) {
      person_data[, AGE_GROUP := NULL]
    }
    if (side_effect_platform == 1) {
      vaccination_data[, V_TYPE := NULL]
    }
    if (side_effect_dose == 1) {
      vaccination_data[, V_DOSE := NULL]
    }
    if (side_effect_first_vac == 1) {
      vaccination_data[, first_vac := NULL]
    }
  }

  cat("#")

  return(output)

}

#' Complete aggregation of data with concurrent comparators
#'
#' @description
#' Helper function to combine risk/control AESI counts and risk/control groups and calculate ratio, for concurrent study designs.
#'
#' @inheritParams aggregate_data_concurrent
#' @param control_group Table describing people in control group on each day, obtained from `aggregate_data_concurrent()` function.
#' @param risk_group Table describing people in risk group on each day, obtained from `aggregate_data_concurrent()` function.
#' @param control_counts Table describing AESI counts from control group on each day, obtained from `aggregate_data_concurrent()` function.
#' @param case_counts Table describing AESI counts from risk group on each day, obtained from `aggregate_data_concurrent()` function.
#' @param output A data.table containing already aggregated results, to which these results will be added
#'
#' @return A data.table object matching the 'Concurrent - Vac' or 'Concurrent - Unvac' templates.
concurrent_final_aggregation <- function(control_group, risk_group, control_counts, case_counts, output, analysis_types, options){

  if (nrow(risk_group) > 0 & nrow(control_group) > 0) {

    dose_columns <- grep("V_DOSE", names(risk_group), value = TRUE)

    risk_group <- risk_group[
      , .(n_at_risk = sum(n_at_risk, na.rm = TRUE)),
      by = c("PERIOD_START", "PERIOD_END", "risk_washout", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", dose_columns, "slice_date")
    ][
      n_at_risk > 0
    ]

    control_group <- control_group[
      , .(n_in_control = sum(n_in_control, na.rm = TRUE)),
      by = .(PERIOD_START, PERIOD_END, risk_washout, AGE_GROUP, SEX, slice_date)
    ][
      n_in_control > 0
    ]

    case_counts <- case_counts[
      , .(CASE_COUNT = sum(CASE_COUNT, na.rm = TRUE)),
      by = c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", dose_columns, "slice_date", "risk_washout")
    ][
      CASE_COUNT > 0
    ]

    control_counts <- control_counts[
      , .(CONTROL_COUNT = sum(CONTROL_COUNT, na.rm = TRUE)),
      by = .(PERIOD_START, PERIOD_END, ENCOUNTER_TYPE, AESI, AGE_GROUP, SEX, slice_date, risk_washout)
    ][
      CONTROL_COUNT > 0
    ]

    if (is.null(analysis_types)) {

      combined_groups <- merge(risk_group, control_group,
                               by = c("PERIOD_START", "PERIOD_END", "risk_washout", "AGE_GROUP", "SEX", "slice_date"),
                               all = TRUE, allow.cartesian = TRUE)

      output <- rbindlist(list(output,
                               merge(combined_groups[control_counts, on = .(PERIOD_START, PERIOD_END, risk_washout, AGE_GROUP, SEX, slice_date), allow.cartesian = TRUE],
                                     combined_groups[case_counts, on = .(PERIOD_START, PERIOD_END, risk_washout, AGE_GROUP, SEX, V_SUBTYPE, V_TYPE, V_DOSE, slice_date), allow.cartesian = TRUE],
                                     by = c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE",
                                            "slice_date", "risk_washout", "n_at_risk", "n_in_control"),
                                     all = TRUE, allow.cartesian = TRUE)[
                                       !is.na(n_in_control) & !is.na(n_at_risk) & !(is.na(CASE_COUNT) & is.na(CONTROL_COUNT))
                                     ][
                                       , RATIO := round(n_at_risk/n_in_control, options$ratio_precision)
                                     ][
                                       , .(CASE_COUNT = sum(CASE_COUNT, na.rm = TRUE),
                                           CONTROL_COUNT = sum(CONTROL_COUNT, na.rm = TRUE)),
                                       by = .(PERIOD_START, PERIOD_END, ENCOUNTER_TYPE, AESI, AGE_GROUP, SEX, V_SUBTYPE, V_TYPE, V_DOSE, RATIO)
                                     ]),
                          fill = TRUE
      )

    } else {

      remap_concurrent <- function(dt, analysis_type, value_col, by_cols) {
        if (analysis_type == "primary") {
          brand_val <- "pooled"; platform_val <- "pooled"; dose_col <- NULL
        } else if (analysis_type == "subgroup_dose") {
          brand_val <- "pooled"; platform_val <- "pooled"; dose_col <- "V_DOSE_disease"
        } else if (analysis_type == "subgroup_platform") {
          brand_val <- "pooled"; platform_val <- NULL; dose_col <- NULL
        } else if (analysis_type == "subgroup_platform_dose") {
          brand_val <- "pooled"; platform_val <- NULL; dose_col <- "V_DOSE_platform"
        } else if (analysis_type == "subgroup_brand") {
          brand_val <- NULL; platform_val <- NULL; dose_col <- NULL
        } else if (analysis_type == "subgroup_brand_dose") {
          brand_val <- NULL; platform_val <- NULL; dose_col <- "V_DOSE_brand"
        } else {
          stop("Analysis type not recognised.")
        }
        remapped <- dt[, c(by_cols, value_col), with = FALSE]
        if (!is.null(brand_val)) set(remapped, j = "V_SUBTYPE", value = brand_val)
        if (!is.null(platform_val)) set(remapped, j = "V_TYPE", value = platform_val)
        if (!is.null(dose_col)) {
          set(remapped, j = "V_DOSE", value = dt[[dose_col]])
        } else {
          set(remapped, j = "V_DOSE", value = -1L)
        }
        remapped[, lapply(.SD, sum), by = by_cols, .SDcols = value_col]
      }

      for (analysis_type in analysis_types) {

        risk_group_at <- remap_concurrent(risk_group, analysis_type, "n_at_risk",
          c("PERIOD_START", "PERIOD_END", "risk_washout", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "slice_date"))[n_at_risk > 0]

        case_counts_at <- remap_concurrent(case_counts, analysis_type, "CASE_COUNT",
          c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE", "slice_date", "risk_washout"))[CASE_COUNT > 0]

        combined_groups <- merge(risk_group_at, control_group,
                                 by = c("PERIOD_START", "PERIOD_END", "risk_washout", "AGE_GROUP", "SEX", "slice_date"),
                                 all = TRUE, allow.cartesian = TRUE)

        output <- rbindlist(list(output,
                                 merge(combined_groups[control_counts, on = .(PERIOD_START, PERIOD_END, risk_washout, AGE_GROUP, SEX, slice_date), allow.cartesian = TRUE],
                                       combined_groups[case_counts_at, on = .(PERIOD_START, PERIOD_END, risk_washout, AGE_GROUP, SEX, V_SUBTYPE, V_TYPE, V_DOSE, slice_date), allow.cartesian = TRUE],
                                       by = c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", "V_SUBTYPE", "V_TYPE", "V_DOSE",
                                              "slice_date", "risk_washout", "n_at_risk", "n_in_control"),
                                       all = TRUE, allow.cartesian = TRUE)[
                                         !is.na(n_in_control) & !is.na(n_at_risk) & !(is.na(CASE_COUNT) & is.na(CONTROL_COUNT))
                                       ][
                                         , RATIO := round(n_at_risk/n_in_control, options$ratio_precision)
                                       ][
                                         , .(CASE_COUNT = sum(CASE_COUNT, na.rm = TRUE),
                                             CONTROL_COUNT = sum(CONTROL_COUNT, na.rm = TRUE)),
                                         by = .(PERIOD_START, PERIOD_END, ENCOUNTER_TYPE, AESI, AGE_GROUP, SEX, V_SUBTYPE, V_TYPE, V_DOSE, RATIO)
                                       ]),
                            fill = TRUE
        )
      }
    }
  }

  if (nrow(output) == 0 & ncol(output) == 0) {
    output[, c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX",
               "V_SUBTYPE", "V_TYPE", "V_DOSE", "RATIO", "CASE_COUNT", "CONTROL_COUNT") := character()]
  }

  return(output)

}

#' Aggregate data with self controls
#'
#' @description
#' Aggregate risk/control AESI counts and window ratios according to self-controlled study designs, within a given period of time.
#'
#' @inheritParams aggregate_data_concurrent
#' @param comparator Specify whether a 'post' or 'pre' vaccination control window is used.
#'
#' @return A data.table object matching the 'SCRI - post' or 'SCRI - pre' templates.
aggregate_data_self <- function(person_data, vaccination_data, outcome_data,
                                period_start, period_end,
                                options,
                                comparator = "post",
                                allow_side_effects = FALSE,
                                analysis_types = NULL) {

  stopifnot("Input `comparator` must either be 'post' or 'pre'" = comparator %in% c("post", "pre"))

  side_effect_first_vac <- 0
  side_effect_platform <- 0
  side_effect_dose <- 0
  side_effect_age <- 0

  # These columns may already exist if the main aggregation loop pre-computed them for
  # this split. The existence check avoids redundant computation across period iterations.
  if (!"first_vac" %in% names(vaccination_data)) {
    vaccination_data <- find_first_vac(vaccination_data)
    side_effect_first_vac <- 1
  }

  if (!"V_TYPE" %in% names(vaccination_data)) {
    vaccination_data <- map_subtypes_to_type(vaccination_data, options$vaccine_info)
    side_effect_platform <- 1
  }

  if (!"V_DOSE" %in% names(vaccination_data)) {
    vaccination_data <- calc_vaccine_dose(vaccination_data, reference = "dose")
    side_effect_dose <- 1
  }

  if (!"AGE_GROUP" %in% names(person_data)) {
    person_data[
      , age_years := pmax(floor(as.numeric(period_start - DOB)/365.25), 0)
    ][
      , AGE_GROUP := cut(age_years, breaks = options$age_groups$bounds, labels = options$age_groups$labels, right = FALSE)
    ][
      , age_years := NULL
    ]
    side_effect_age <- 1
  }

  if (is.null(analysis_types)) {
    analysis_types <- c("unchanged")
  } else {
    vaccination_data[, ':=' (V_SUBTYPE_pooled = "pooled", V_TYPE_pooled = "pooled", V_DOSE_pooled = -1)]
  }

  output <- data.table()

  for (analysis_type in analysis_types) {

    if (analysis_type == "primary") {
      vaccine_columns <- c("V_SUBTYPE_pooled", "V_TYPE_pooled", "V_DOSE_pooled")
    } else if (analysis_type == "subgroup_dose") {
      vaccine_columns <- c("V_SUBTYPE_pooled", "V_TYPE_pooled", "V_DOSE_disease")
    } else if (analysis_type == "subgroup_platform") {
      vaccine_columns <- c("V_SUBTYPE_pooled", "V_TYPE", "V_DOSE_pooled")
    } else if (analysis_type == "subgroup_platform_dose") {
      vaccine_columns <- c("V_SUBTYPE_pooled", "V_TYPE", "V_DOSE_platform")
    } else if (analysis_type == "subgroup_brand") {
      vaccine_columns <- c("V_SUBTYPE", "V_TYPE", "V_DOSE_pooled")
    } else if (analysis_type == "subgroup_brand_dose") {
      vaccine_columns <- c("V_SUBTYPE", "V_TYPE", "V_DOSE_brand")
    } else if (analysis_type == "unchanged") {
      vaccine_columns <- c("V_SUBTYPE", "V_TYPE", "V_DOSE")
    } else {
      stop("Analysis type not recognised.")
    }

    combined_data <- person_data[vaccination_data[outcome_data, nomatch = 0, on = .(PID)], nomatch = 0, on = .(PID)]

    if (comparator == "post") {
      combined_data <- combined_data[
        EVENT_DATE >= first_vac & V_DATE <= period_end
      ]
      if (nrow(combined_data) > 0) {
        combined_data <- options$outcome_info[
          combined_data, on = .(AESI)
        ][
          order(AESI, PID, V_DATE)
        ][
          , ':=' (risk_start = V_DATE + risk_lower,
                  risk_end = V_DATE + risk_upper,
                  washout_end = V_DATE + risk_upper + washout_post,
                  control_start = V_DATE + risk_upper + washout_post + 1)
        ][
          , ':=' (risk_start_next = shift(risk_start, type = "lead"),
                  control_start_next = shift(control_start, type = "lead"),
                  PID_next = shift(PID, type = "lead"),
                  PID_prev = shift(PID, type = "lag"))
        ][
          , overlapping_risk := !is.na(PID_next) & (PID_next == PID & risk_start_next %between% list(risk_start, washout_end + 1))
        ][
          , risk_end := fifelse(!is.na(PID_next) & (PID_next == PID & risk_end >= risk_start_next),
                                pmax(risk_start, risk_start_next - 1, na.rm = TRUE), risk_end),
        ][
          , washout_end := fifelse(!is.na(PID_next) & (PID_next == PID & washout_end >= risk_start_next),
                                   pmax(risk_end, risk_start_next - 1, na.rm = TRUE), washout_end),
        ][
          , ':=' (risk_start = pmin(risk_start, CENSOR_DATE, na.rm = TRUE),
                  risk_end = pmin(risk_end, CENSOR_DATE, na.rm = TRUE),
                  washout_end = pmin(washout_end, CENSOR_DATE, na.rm = TRUE))
        ]

        # When a person has overlapping risk windows across multiple doses, the control
        # window start must be propagated forward through the chain of overlapping doses.
        # A single pass is insufficient because each correction can affect the next row,
        # so the update is repeated until the values stabilise (i.e. no further changes).
        new_control <- combined_data$control_start
        old_control <- c(1)
        while(!all(old_control == new_control)) {
          combined_data[
            , control_start_next := shift(control_start, type = "lead")
          ][
            , control_start := fifelse(!is.na(PID_next) & (PID_next == PID & overlapping_risk == TRUE),
                                       control_start_next, control_start)
          ]
          old_control <- new_control
          new_control <- combined_data$control_start
        }

        # Collapse multiple event rows sharing the same subgroup and control window
        # to a single representative row before calculating risk/control lengths.
        combined_data <- combined_data[
          , subgroup := .GRP, by = c("PID", "AESI", vaccine_columns)
        ][
          , risk_length := sum(as.numeric(risk_end - risk_start, "days") + 1), by = .(subgroup, control_start)
        ][
          risk_length > 0
        ][
          , ':=' (risk_start_next = tail(risk_start_next, 1),
                  PID_next = tail(PID_next, 1)),
          by = .(PID, AESI, control_start)
        ][
          , control_target_length := pmax(risk_length*control_post_target, control_post_min - risk_length)
        ][
          , control_end_pretruncation := control_start + control_target_length - 1
        ][
          , control_end := fifelse(!is.na(PID_next) & (PID_next == PID & control_end_pretruncation >= risk_start_next),
                                   pmax(control_start, risk_start_next - 1, na.rm = TRUE), control_end_pretruncation),
        ][
          , control_end := pmin(control_end, CENSOR_DATE, na.rm = TRUE)
        ][
          (control_end %between% c(period_start, period_end) |
             (control_end_pretruncation >= period_start & control_end < period_start &
                (CENSOR_DATE %between% c(period_start, period_end) |
                   (!is.na(PID_next) & (PID_next == PID & risk_start_next %between% c(period_start, period_end)))))) &
            (EVENT_DATE %between% list(risk_start, risk_end) | EVENT_DATE %between% list(control_start, control_end))
        ][
          , control_length := as.numeric(control_end - control_start, "days") + 1
        ][
          control_length > 0
        ]

        combined_data <- unique(combined_data, by = c("subgroup", "EVENT_DATE"))

      }

    } else {
      combined_data <- combined_data[
        V_DATE <= period_end
      ]
      if (nrow(combined_data) > 0) {
        combined_data <- options$outcome_info[
          combined_data, on = .(AESI)
        ][
          order(AESI, PID, V_DATE)
        ][
          , ':=' (subgroup = .GRP, n_in_subgroup = 1:.N),
          by = c("PID", "AESI", vaccine_columns)
        ][
          , ':=' (risk_start = V_DATE + risk_lower,
                  risk_end = V_DATE + risk_upper,
                  washout_post_end = V_DATE + risk_upper + washout_post,
                  washout_pre_start = V_DATE - washout_pre)
        ][
          , ':=' (risk_start_next = shift(risk_start, type = "lead"),
                  risk_end_prev = shift(risk_end, type = "lag"),
                  subgroup_prev = shift(subgroup, type = "lag"),
                  washout_post_end_prev = shift(washout_post_end, type = "lag"),
                  PID_next = shift(PID, type = "lead"),
                  PID_prev = shift(PID, type = "lag"),
                  V_DATE_prev = shift(V_DATE, type = "lag"))
        ][
          , ':=' (overlapping_washout = !is.na(PID_prev) & (PID_prev == PID & washout_post_end_prev %between% list(washout_pre_start - 1, risk_start + washout_post)),
                  overlapping_risk = !is.na(PID_prev) & (PID_prev == PID & risk_end_prev >= risk_start - 1))
        ][
          , risk_end := fifelse(!is.na(PID_next) & (PID_next == PID & risk_end >= risk_start_next),
                                pmax(risk_start, risk_start_next - 1, na.rm = TRUE), risk_end),
        ][
          , washout_post_end := fifelse(!is.na(PID_next) & (PID_next == PID & washout_post_end >= risk_start_next),
                                        pmax(risk_end, risk_start_next - 1, na.rm = TRUE), washout_post_end),
        ][
          , control_end := fcase(n_in_subgroup == 1 & overlapping_risk == TRUE, first_vac - washout_pre - 1,
                                 overlapping_risk == FALSE & overlapping_washout == FALSE, V_DATE - washout_pre - 1)
        ][
          , contains_overlaps := any(is.na(control_end)), by = .(PID)
        ][
          , ':=' (risk_start = pmin(risk_start, CENSOR_DATE, na.rm = TRUE),
                  risk_end = pmin(risk_end, CENSOR_DATE, na.rm = TRUE),
                  washout_post_end = pmin(washout_post_end, CENSOR_DATE, na.rm = TRUE))
        ]

        overlapping_data <- combined_data[
          contains_overlaps == TRUE
        ][
          , control_end := fifelse(is.na(control_end), as.Date("0000-01-01"), control_end)
        ]

        if (nrow(overlapping_data) > 0) {

          # Resolve control_end for rows with overlapping washout or risk windows.
          # The correct control_end for a dose may depend on the corrected value of the
          # preceding dose, so the update must be iterated until no further changes occur.
          new_control <- overlapping_data$control_end
          old_control <- c(1)
          while(!all(old_control == new_control)) {
            overlapping_data[
              , ':=' (control_end_prev = shift(control_end, type = "lag"),
                      overlapping_risk_prev = shift(overlapping_risk, type = "lag"),
                      overlapping_washout_prev = shift(overlapping_washout, type = "lag"))
            ][
              , control_end := fcase(n_in_subgroup == 1 | (overlapping_risk == FALSE & overlapping_washout == FALSE) |
                                       is.na(PID_prev) | PID_prev != PID, control_end,
                                     overlapping_risk == TRUE & (subgroup == subgroup_prev | overlapping_risk_prev == TRUE), control_end_prev,
                                     overlapping_risk == TRUE & overlapping_risk_prev == FALSE & overlapping_washout_prev == FALSE, control_end_prev,
                                     overlapping_risk == FALSE & overlapping_washout == TRUE & subgroup != subgroup_prev & overlapping_washout_prev == FALSE, control_end_prev,
                                     default = as.Date("0000-01-01"))
            ]
            old_control <- new_control
            new_control <- overlapping_data$control_end
          }

          combined_data <- rbindlist(list(
            combined_data[contains_overlaps == FALSE],
            overlapping_data[, ':=' (control_end_prev = NULL,
                                     overlapping_risk_prev = NULL,
                                     overlapping_washout_prev = NULL)]
          ))
        }

        # Calculate risk and control window lengths, then filter to cases/controls
        # that fall within the current period and have a valid control window.
        combined_data <- combined_data[
          order(AESI, PID, V_DATE)
        ][
          , risk_group := cumsum(overlapping_risk == FALSE)
        ][
          , keep_id := tail(risk_end, 1) %between% c(period_start, period_end), by = .(risk_group)
        ][
          , ':=' (washout_post_end_prev = head(washout_post_end_prev, 1),
                  PID_prev = head(PID_prev, 1)),
          by = .(PID, AESI, control_end)
        ][
          keep_id == TRUE
        ][
          , control_end := fifelse(n_in_subgroup == 1, first_vac - washout_pre - 1, control_end)
        ][
          control_end != as.Date("0000-01-01")
        ][
          , risk_length := sum(as.numeric(risk_end - risk_start, "days") + 1), by = .(subgroup, control_end)
        ][
          risk_length > 0
        ][
          , control_target_length := pmax(risk_length*control_pre_target, control_pre_min - risk_length)
        ][
          , control_start := control_end - control_target_length + 1
        ][
          , control_start := fifelse(!is.na(PID_prev) & n_in_subgroup > 1 & (PID_prev == PID & control_start <= washout_post_end_prev),
                                     washout_post_end_prev + 1, control_start)
        ][
          , control_start := pmax(control_start, ENROL_DATE, na.rm = TRUE)
        ][
          , control_length := as.numeric(control_end - control_start, "days") + 1
        ][
          control_length > 0
        ][
          EVENT_DATE %between% list(risk_start, risk_end) | EVENT_DATE %between% list(control_start, control_end)
        ]

        combined_data <- unique(combined_data, by = c("subgroup", "EVENT_DATE"))

      }
    }

    if (nrow(combined_data) > 0) {
      combined_data <- combined_data[
        , ':=' (PERIOD_START = period_start,
                PERIOD_END = period_end)
      ][
        , ':=' (CASE_COUNT = (EVENT_DATE %between% list(risk_start, risk_end))*1,
                CONTROL_COUNT = (EVENT_DATE %between% list(control_start, control_end))*1,
                WINDOW_RATIO = round(risk_length/control_length, digits = options$ratio_precision))
      ][
        , .(CASE_COUNT = sum(CASE_COUNT),
            CONTROL_COUNT = sum(CONTROL_COUNT)),
        by = c("PERIOD_START", "PERIOD_END", "ENCOUNTER_TYPE", "AESI", "AGE_GROUP", "SEX", vaccine_columns, "WINDOW_RATIO")
      ]
      setnames(combined_data, old = sort(vaccine_columns), new = sort(c("V_SUBTYPE", "V_TYPE", "V_DOSE")))
      output <- rbindlist(list(output, combined_data))
    } else {
      output <- rbindlist(list(output, data.table()))
    }
  }

  if (!allow_side_effects) {
    if (side_effect_age == 1) {
      person_data[, AGE_GROUP := NULL]
    }
    if (side_effect_platform == 1) {
      vaccination_data[, V_TYPE := NULL]
    }
    if (side_effect_dose == 1) {
      vaccination_data[, V_DOSE := NULL]
    }
    if (side_effect_first_vac == 1) {
      vaccination_data[, first_vac := NULL]
    }
  }

  cat("#")

  return(output)
}
