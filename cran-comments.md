## Test environments

- Local Windows 11 installation (R 4.4.2)
- GitHub Actions:
  - Windows (release)
  - MacOS (release)
  - Ubuntu 22.04 (devel, release, oldrel)
- win-builder (devel, release, oldrel)
- rhub:
  - linux (devel)
  - macos (devel)
  - macos-arm64 (devel)
  - windows (devel)
  - rchk (devel)

## R CMD check results

0 errors | 0 warnings | 1 note
  
  Found the following (possibly) invalid URLs:
    URL: https://www.sptrans.com.br/desenvolvedores/
      From: inst/doc/gtfstools.html
      Status: 403
      Message: Forbidden
      
This is a resubmission.

The package was flagged for having examples and vignettes with CPU time
considerably higher than elapsed time. This was caused by the heavy reliance on
{data.table}, which uses multiple threads by default. We have restricted the
number of threads to 1 in the example and vignettes. We're still receiving the
note regarding the possibly invalid URL above, but we have no issues accessing
it.
