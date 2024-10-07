## Test environments

- Local Windows 11 installation (R 4.2.2)
- GitHub Actions:
  - Windows (release, oldrel)
  - MacOS (release, oldrel)
  - Ubuntu 22.04 (devel, release, oldrel)
- win-builder (release, oldrel)

## R CMD check results

0 errors | 0 warnings | 1 note

  New submission
  
  Package was archived on CRAN
  
  Possibly misspelled words in DESCRIPTION:
    GTFS (3:44, 21:62)
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2024-09-01 as issues were not corrected
      in time.
  
  Found the following (possibly) invalid URLs:
    URL: https://www.sptrans.com.br/desenvolvedores/
      From: inst/doc/gtfstools.html
      Status: 403
      Message: Forbidden

The package was archived due to underlying problems with the cpp11 package. We
were not able to fix the issue on time because the maintainer was off in a
course, but we have now fixed it. The "GTFS" word is an acronym correctly
spelled, and we have not had any issues accessing the URL above.
