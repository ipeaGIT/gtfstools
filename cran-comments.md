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

We believe this note is a false positive, seince we have not had any issues
accessing the URL above.
