## Resubmission

This is a resubmission. I have changed an invalid URL in the README.md file to follow moved content appropriately.

## Test environments

- Local Ubuntu 20.04 installation (R 4.0.4)
- GitHub Actions:
  - Windows (release, oldrel)
  - MacOS (release, oldrel)
  - Ubuntu 20.04 (devel, release, oldrel)
- win-builder (devel, release, oldrel)
- r-hub:
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Maintainer: ‘Daniel Herszenhut <dhersz@gmail.com>’
  New submission
  Possibly mis-spelled words in DESCRIPTION:
    GTFS (3:44, 15:68)

This is gtfstools first submission. GTFS is an acronym for General Transit Feed Specification. The full specification name is provided both in the package title and description.
