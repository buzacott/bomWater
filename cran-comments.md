## Test environments

* local: macOS 10.5.6, R 4.0.2
* local: Ubuntu 18.04, R 4.0.2
* devtools::check_rhub()

## R CMD check results

0 errors ✓ | 0 warnings ✓ | 0 note ✓

## Package changes

* Fixed url in timeseries docs to fix a note during CRAN tests
* Examples are now not run due to possible loss of internet connectivity or Water Data Online going down for maintenance (which happens often in the evenings)
* Uses httptest in tests with mock api calls due to avoid errors to due Water Data Online being down
* Uses httptest with vignette building for the same reason
* Improved docs for return_fields in all functions
* New function to return a parameter list by station id added
