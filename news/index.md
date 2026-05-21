# Changelog

## rapidcycler 1.3

### Parquet output format

[`aggregate_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data.md)
gains a new `output_format` parameter (default `"parquet"`, option
`"csv"`) that controls the format of the output data files written to
the results folder.

Parquet’s columnar dictionary encoding is particularly efficient for the
categorical-heavy output tables (repeated age groups, AESI codes,
vaccine subtypes, etc.), typically achieving substantially smaller file
sizes than the equivalent CSV. Parquet files are readable natively in R
([`arrow::read_parquet()`](https://arrow.apache.org/docs/r/reference/read_parquet.html)),
Python (`pyarrow`/`pandas`), and SQL engines (DuckDB, BigQuery, Athena,
Spark), making them well-suited for inter-organisational data transfer.

The `"csv"` option is provided for legacy workflows that require
plain-text output.

When resuming an interrupted aggregation that was started with an older
version of the package (before this change), `output_format` defaults to
`"csv"` so that the suppression step correctly operates on the
already-written CSV files.

### Configurable lookback period

A new optional `lookback_length` key can be added to `options.txt` to
specify the required lookback period (in years) before the start of
vaccination (or the cycle start date for historical aggregation). For
example:

    lookback_length: 2

If the key is absent, the default of 2 years is used — existing options
files continue to work without modification.

The validation check that prompts users about insufficient lookback
coverage now uses `lookback_length * 0.75` as its trigger threshold
(previously hard-coded as 1.5 years), and all user-facing messages
reference the configured length dynamically. The notes file written
alongside results now records `Lookback length (years)`.

### `participation_level` deprecated

The `participation_level` argument to
[`aggregate_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data.md)
is deprecated. Use `design_selection` instead. `participation_level`
will continue to work with a warning for backward compatibility,
translating to the equivalent `design_selection` vector:

| Old value | Equivalent `design_selection` |
|----|----|
| 1 | `c("self_post")` |
| 2 | adds `"self_pre"`, `"historical"`, `"concurrent_vac"` |
| 3 | all 5 analytical designs (equivalent to `design_selection = NULL`) |

When `design_selection = NULL` (the new default), all five analytical
designs are run. `design_selection` is now fully validated: unknown
design names raise an error.

The notes file written alongside results now records `Study designs:` in
place of `Participation level:`.

### `design_selection` API changes

Two changes to the `design_selection` parameter refine the API:

- **Descriptive outputs are always produced.**
  `data_descriptive_outcomes` and `data_descriptive_vaccinations` are
  now produced unconditionally for all non-historical aggregations. They
  can no longer be excluded via `design_selection`. The values
  `"descriptive_outcomes"` and `"descriptive_vaccinations"` are no
  longer valid `design_selection` entries and will raise an error if
  passed.

### `data_historic_cases` column rename

The outcome count column in `data_historic_cases` has been renamed from
`CASE_COUNT` to `COUNT` for consistency with
`data_descriptive_outcomes`, which uses `COUNT` for the same
aggregation. Any downstream code reading `data_historic_cases` that
references `$CASE_COUNT` must be updated to `$COUNT`.

### Lookback check behaviour with `skip_user_prompts = TRUE`

Previously, `skip_user_prompts = TRUE` always caused the lookback
coverage check to abort with an error, making automated/pipeline runs
impossible when outcome data did not reach the lookback threshold. This
has been fixed: `skip_user_prompts = TRUE` now issues an R warning and
records it in the console, then continues with aggregation. Interactive
runs are unaffected.

### Terminology changes

Several terms have been renamed for clarity. Old names are deprecated in
exported functions and will be removed in a future version — they
currently trigger a warning but continue to work. Output column names
are renamed immediately with no backward compatibility layer; update any
downstream code that reads these columns.

- **`patient_data` → `person_data`**: The `patient_data` argument to
  [`aggregate_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data.md)
  and
  [`validate_input_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/validate_input_data.md)
  is now called `person_data`. The return value key from
  [`generate_synthetic_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/generate_synthetic_data.md)
  has also changed from `"patient_data"` to `"person_data"` (no
  deprecation — update code that accesses `result$patient_data`).

- **`V_BRAND` → `V_SUBTYPE`**: The `V_BRAND` column in
  `vaccination_data` is now called `V_SUBTYPE`. Passing a data frame
  with `V_BRAND` to
  [`validate_input_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/validate_input_data.md)
  or
  [`aggregate_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/aggregate_data.md)
  triggers a deprecation warning but the column is renamed
  automatically. The `V_BRAND` column in `options$vaccine_info` passed
  to
  [`check_options_object()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/check_options_object.md)
  is similarly renamed in-place. Output files that previously contained
  a `V_BRAND` column now contain `V_SUBTYPE`.

- **`V_PLATFORM` → `V_TYPE`**: The derived vaccine platform column is
  now called `V_TYPE`. The `V_PLATFORM` column in `options$vaccine_info`
  is renamed in-place by
  [`check_options_object()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/check_options_object.md)
  with a deprecation warning. Output files that previously contained
  `V_PLATFORM` now contain `V_TYPE`.

- **`case_ascertainment` → `clean_window`**: The
  `outcome_info.case_ascertainment` key in `options.txt` is now
  `outcome_info.clean_window`. The `case_ascertainment` column in
  `options$outcome_info` is renamed in-place by
  [`check_options_object()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/check_options_object.md)
  with a deprecation warning. Both the old and new key names are
  accepted when reading an options file; old names trigger a deprecation
  warning. Output files that previously contained a `CASE_ASCERTAINMENT`
  column now contain `CLEAN_WINDOW`.

- Added `importFrom` declarations for stats and utils functions.

- Declared data.table NSE column names via
  [`globalVariables()`](https://rdrr.io/r/utils/globalVariables.html) to
  suppress R CMD check NOTEs.

- Added `@examples` to all exported functions.

- Added GitHub Actions CI workflow for R CMD check.

- Removed empty placeholder files (`analyse.R`, `configure.R`,
  `visualise.R`).

- Updated minimum R version to \>= 3.5.0.

- Cleaned up `.Rbuildignore` and removed stray files.
