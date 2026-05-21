#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import data.table
#' @importFrom magrittr %>%
#' @importFrom stats rbeta rpois runif
#' @importFrom utils head menu tail
## usethis namespace: end
NULL

# Suppress R CMD check NOTEs for data.table column names used in NSE
utils::globalVariables(c(
  ".", "..outcomes",
  "AESI", "AGE_GROUP", "CASE_COUNT", "CENSOR_DATE", "CENSOR_TYPE",
  "CONTROL_COUNT", "COUNT", "DOB", "ENCOUNTER_TYPE", "ENROL_DATE",
  "EVENT_DATE", "PERIOD_END", "PERIOD_START", "PERSON_DAYS", "PID",
  "PID_next", "PID_prev", "RATIO", "SEX", "V_DATE", "V_DOSE",
  "V_DOSE_brand", "V_DOSE_disease", "V_DOSE_platform",
  "V_SUBTYPE", "V_SUBTYPE_original", "V_SUBTYPE_pooled",
  "V_TYPE", "V_TYPE_original", "V_TYPE_pooled",
  "age_years", "clean_window", "contains_overlaps", "control_end",
  "control_end_pretruncation", "control_end_prev", "control_length",
  "control_post_min", "control_post_target", "control_pre_min",
  "control_pre_target", "control_start", "control_start_next",
  "control_target_length", "count", "days_diff", "first_vac",
  "flag_control", "flag_risk", "keep_id", "n_at_risk", "n_in_control",
  "n_in_subgroup", "nonrisk_length", "overlapping_risk",
  "overlapping_risk_prev", "overlapping_washout", "overlapping_washout_prev",
  "risk_end", "risk_end_prev", "risk_group", "risk_length", "risk_lower",
  "risk_period", "risk_start", "risk_start_next", "risk_upper",
  "risk_washout", "slice_date", "subgroup", "subgroup_prev",
  "washout_end", "washout_post", "washout_post_end", "washout_post_end_prev",
  "washout_pre", "washout_pre_start"
))
