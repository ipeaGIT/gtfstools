## Test environments

- Local Ubuntu 20.04 installation (R 4.1.2)
- GitHub Actions:
  - Windows (release, oldrel)
  - MacOS (release, oldrel)
  - Ubuntu 20.04 (devel, release, oldrel)
- r-hub:
  - Windows Server 2008 R2 SP1, R-devel, 32/64 bit
  - Ubuntu Linux 20.04.1 LTS, R-release, GCC
  - Fedora Linux, R-devel, clang, gfortran
  - Oracle Solaris 10, x86, 32 bit, R-release

## R CMD check results

0 errors | 0 warnings | 1 note

> Found the following (possibly) invalid URLs:
>   URL: https://doi.org/10.5281/zenodo.5703285
>     From: inst/CITATION
>     Status: 404
>     Message: Not Found
> 
> Found the following (possibly) invalid DOIs:
>   DOI: 10.5281/zenodo.5703285
>     From: inst/CITATION
>     Status: Not Found
>     Message: 404

I have reserved a DOI in Zenodo to use in the CITATION file, but I haven't yet finished the submission because I'm awaiting for the approval on CRAN to upload the package `.tar.gz`. As soon as the package gets approved I'll upload the file and finish the submission, thus creating the DOI for good and getting rid of this note.

Also, when running the checks on Solaris I get:

> * checking dependencies in R code ... NOTE
> Namespace in Imports field not imported from: ‘utils’
>   All declared Imports should be used.

I'm using utils as a build-time dependency due to data.table's non-standard evaluation via utils::globalVariables (which is why I guess it doesn't recognize the function call as being used). I have started importing this function via '@importFrom utils globalVariables' in R/gtfstools.R.
