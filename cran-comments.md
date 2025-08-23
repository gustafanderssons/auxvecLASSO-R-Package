## Test environments
- macOS Ventura 13.7.6 (gitHub Actions)
- Windows Server 2022 x64 (build 20348)-R version 4.6.0 (GitHub Actions)
- Ubuntu 24.04.2 LTS-R version 4.6.0 (GitHub Actions)
- R-hub GitHub workflow: Linux/Windows/macOS

## R CMD check results
0 errors | 0 warnings | 1 note

* This is a new release.

## Notes to CRAN
- Examples and tests avoid network access and are designed to be run in under 60s.

## Resubmission
This is a re-submission. I have added references in the DESCRIPTION file
and changed from \dontrun{} to \donttest{} in the example for
generate_population_totals().
