## bomWater (0.4.1)

* New function to return a parameter list by station id added
* Improved docs for return_fields in all functions
* Examples are now not run due to possible loss of internet connectivity or Water Data Online going down for maintenance (which happens often in the evenings)
* Uses httptest in tests on CRAN with mock api calls due to avoid errors to due Water Data Online being down. tests also check for internet connectivity
* Uses httptest with vignette building for the same reason
* Fixed url in timeseries docs to fix a note during CRAN tests

## bomWater (0.4.0)

* Converted style of functions to `tidyverse` style
* Added vignette
* Added tests for the core request functions

## bomWater (0.3.0)

* Fixed an error that could occur from multiple tsIDs
* Fixed dependencies
* Fixed licence
* Fixed documentation and internal functions
* Added `parameters()` function to retrieve BoM parameters from `bomWater`

## bomWater (0.2.0)

* First push to github
* Converted docs to roxygen
* Added var options for daily flow
