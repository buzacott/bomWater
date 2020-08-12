# Resubmission

* removed examples from unexported functions
* exported `get_timeseries` and added an example
* tweaked the title to 'Download Australian Bureau of Meteorology Water Data'
* followed devtools::release() instructions again*

# Review

You have examples for unexported functions.
Please either omit these examples or export these functions.

Please fix and resubmit.

# Original submission

## Test environments

* local macOS install, R 4.0.2
* local ubuntu 18.04 install, R 3.6.3
* followed devtools::release() instructions
* devtools::check_rhub() failed due to `Error in loadNamespace(name) : there is no package called 'utf8'`
* Known error ^^ https://github.com/r-hub/rhub/issues/374
* The following manual check passed with out error as per the raised issue

```
rhub::check(
  platform="windows-x86_64-devel",
  env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")
)
```
* rhub::check_cran() passed
* devtools::check_win_devel() passed

## R CMD check results

bomWater 0.4.0
Duration: 23.5s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓
