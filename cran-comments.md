## Test environments

* local macOS install, R 4.0.2
* local ubuntu 18.04 install, R 3.6.3
* checked on windows with devtools::check_win_release()
* followed devtools::release() instructions
* devtools::check_rhub() failed due to `Error in loadNamespace(name) : there is no package called 'utf8'`
* Known error ^^ https://github.com/r-hub/rhub/issues/374
* The following manual check passed witht out error as per the raised issue

```
rhub::check(
  platform="windows-x86_64-devel",
  env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always")
)
```

* devtools::check_win_devel() passed

## R CMD check results

bomWater 0.3.2
Duration: 23.5s

0 errors ✓ | 0 warnings ✓ | 0 notes ✓
