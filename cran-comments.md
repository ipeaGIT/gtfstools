## Test environments

- Local Ubuntu 20.04 installation (R 4.2.0)
- GitHub Actions:
  - Windows (release, oldrel)
  - MacOS (release, oldrel)
  - Ubuntu 20.04 (devel, release, oldrel)
- r-hub:
  - Windows Server 2022, R-devel, 64 bit
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
- win-builder (devel, release, oldrel)

## R CMD check results

0 errors | 0 warnings | 1 note

> Found the following (possibly) invalid URLs:
>   URL: https://doi.org/10.5281/zenodo.6577028
>     From: inst/CITATION
>     Status: 404
>     Message: Not Found
> 
> Found the following (possibly) invalid DOIs:
>   DOI: 10.5281/zenodo.6577028
>     From: inst/CITATION
>     Status: Not Found
>     Message: 404

I have reserved a DOI in Zenodo to use in the CITATION file, but I haven't yet finished the submission because I'm awaiting for the approval on CRAN to upload the package `.tar.gz`. As soon as the package gets approved I'll upload the file and finish the submission, thus creating the DOI for good and getting rid of this note.
