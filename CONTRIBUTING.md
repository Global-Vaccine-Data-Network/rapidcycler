# Contributing to rapidcycler

Thank you for your interest in contributing to *rapidcycler*. This
package is developed for the [Global Vaccine Data Network
(GVDN)](https://globalvaccinedatanetwork.org/) to support vaccine safety
surveillance, and contributions that improve its reliability, usability,
and correctness are very welcome.

## How to Contribute

### Reporting bugs

If you find a bug, please open an issue on
[GitHub](https://github.com/Global-Vaccine-Data-Network/rapidcycler/issues)
and include:

- A minimal reproducible example (use
  [`generate_synthetic_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/generate_synthetic_data.md)
  where possible to avoid sharing real data)
- The output of
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html)
- A description of the expected vs. actual behaviour

### Suggesting enhancements

Feature requests can also be submitted as GitHub Issues. Please describe
the use case clearly and, where relevant, relate it to the RCA
methodology or GVDN framework requirements.

### Submitting a pull request

1.  Fork the repository and create a new branch from `main`.
2.  Make your changes, following the code style of the existing codebase
    (primarily `data.table` conventions).
3.  Add or update tests in `tests/testthat/` to cover your changes. Run
    `devtools::test()` to confirm all tests pass.
4.  Update documentation as needed using roxygen2 comments, then run
    `devtools::document()`.
5.  Run `devtools::check()` and resolve any errors or warnings before
    submitting.
6.  Open a pull request with a clear description of the changes and the
    problem they address.

## Development Setup

``` r

# Clone and install development dependencies
devtools::install_deps(dependencies = TRUE)

# Load the package locally
devtools::load_all()

# Run tests
devtools::test()

# Regenerate documentation
devtools::document()

# Full check
devtools::check()
```

## Important Notes

- **No real patient data**: Do not include any real or identifiable
  patient, vaccination, or outcome data in examples, tests, or issues.
  Use
  [`generate_synthetic_data()`](https://global-vaccine-data-network.github.io/rapidcycler/reference/generate_synthetic_data.md)
  instead.
- **Sensitive context**: This package is used in public health
  surveillance. Please take care that changes do not introduce incorrect
  statistical behaviour or alter aggregation outputs in unexpected ways.
- **Snapshot tests**: Many tests use `testthat` snapshots. If your
  change intentionally modifies output, update snapshots with
  [`testthat::snapshot_review()`](https://testthat.r-lib.org/reference/snapshot_accept.html).

## Code of Conduct

Please note that this project is released with a [Code of
Conduct](https://global-vaccine-data-network.github.io/rapidcycler/CODE_OF_CONDUCT.md).
By contributing, you agree to abide by its terms.
