# Script to regenerate golden fixture files for regression tests.
#
# Run this script manually whenever a deliberate, expected change to aggregation
# outputs is made. Commit the updated fixtures alongside the code change.
#
# Usage (from package root):
#   Rscript tests/testthat/fixtures/generate_golden.R

devtools::load_all(quiet = TRUE)

set.seed(42)

options_obj <- read_options_file("tests/testthat/fixtures")

cat("Generating synthetic data (pop_size = 10000)...\n")
synth <- generate_synthetic_data(pop_size = 10000, save_data = FALSE)
pd <- synth$person_data
vd <- synth$vaccination_data
od <- synth$outcome_data
od[, AESI := as.character(AESI)]

# Filter to cycle-eligible persons (2022-01-01 to 2022-01-31)
cycle_start_int <- 20220101L
cycle_end_int   <- 20220131L
pd <- pd[ENROL_DATE <= cycle_end_int & (is.na(CENSOR_DATE) | CENSOR_DATE >= cycle_start_int)]
vd <- vd[PID %in% pd$PID]
od <- od[PID %in% pd$PID]

cat("Eligible persons:", nrow(pd), "| vaccinations:", nrow(vd), "| outcomes:", nrow(od), "\n")

# Save input fixtures so tests use identical data (not re-generated)
input_dir <- "tests/testthat/fixtures/golden_inputs"
dir.create(input_dir, showWarnings = FALSE, recursive = TRUE)
arrow::write_parquet(pd, file.path(input_dir, "person_data.parquet"))
arrow::write_parquet(vd, file.path(input_dir, "vaccination_data.parquet"))
arrow::write_parquet(od, file.path(input_dir, "outcome_data.parquet"))
cat("Input fixtures saved to", input_dir, "\n")

tmp_reg  <- tempfile()
tmp_hist <- tempfile()
dir.create(tmp_reg)
dir.create(tmp_hist)

# --- Regular aggregation ---
cat("Running regular aggregation...\n")
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
  working_directory     = tmp_reg,
  skip_user_prompts     = TRUE,
  output_format         = "parquet"
)

# --- Historical aggregation (vaccination_data = NULL) ---
cat("Running historical aggregation...\n")
pd2 <- copy(pd); od2 <- copy(od)
aggregate_data(
  pd2, NULL, od2,
  cycle_start_date      = as.Date("2022-01-01"),
  cycle_end_date        = as.Date("2022-01-31"),
  site_code             = "TEST",
  options_file_location = NULL,
  options               = options_obj,
  restore_input_data    = FALSE,
  working_directory     = tmp_hist,
  skip_user_prompts     = TRUE,
  output_format         = "parquet"
)

# Copy outputs to golden fixture directory
output_dir <- "tests/testthat/fixtures/golden_outputs"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

reg_folder  <- list.files(tmp_reg,  full.names = TRUE)[1]
hist_folder <- list.files(tmp_hist, full.names = TRUE)[1]

for (f in c(list.files(reg_folder,  pattern = "\\.parquet$", full.names = TRUE),
            list.files(hist_folder, pattern = "\\.parquet$", full.names = TRUE))) {
  file.copy(f, file.path(output_dir, basename(f)), overwrite = TRUE)
}

cat("Golden outputs saved to", output_dir, "\n")
cat("Files:\n")
cat(paste(" -", list.files(output_dir)), sep = "\n")
cat("\nDone. Commit tests/testthat/fixtures/golden_inputs/ and tests/testthat/fixtures/golden_outputs/\n")
