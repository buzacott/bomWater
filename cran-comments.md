Updating as requested prior to 2019-09-17

## Test environments

* local: macOS 10.5.6, R 4.0.2
* local: Ubuntu 18.04, R 4.0.2
* rhub::check_for_cran()

── R CMD check results ─────────────────────────────────────────────────────────────────────────────────────────────────────────────── bomWater 0.4.1 ────
Duration: 18.2s

> checking for future file timestamps ... NOTE
  unable to verify current time

0 errors ✓ | 0 warnings ✓ | 1 note x

worldclockapi seems to be down

## Package changes

* Examples are now not run due to possible loss of internet connectivity or Water Data Online going down for maintenance (which happens often in the evenings)
* Uses httptest in tests on CRAN with mock api calls due to avoid errors to due Water Data Online being down. tests also check for internet connectivity
* Uses httptest with vignette building for the same reason
* Fixed url in timeseries docs to fix a note during CRAN tests
* Improved docs for return_fields in all functions
* New function to return a parameter list by station id added
