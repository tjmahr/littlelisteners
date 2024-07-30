#' littlelisteners.
#'
#' @name littlelisteners
#' @docType package
#' @import dplyr rlang
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

#' Example data from a Visual World experiment
#'
#' @format A data frame with 20,910 rows and 25 variables
"four_image_data"



#' Locate the path of example eyetracking files
#'
#' @param which index of the batch of example files to load
#' @return Paths to a banch of examples files bundled with the
#'   `littlelisteners` package.
#'
#' @details This function is a wrapper over [`system.file()`]  to locate the
#' paths to bundled eyetracking data. These files are used to test or demonstrate
#' functionality of the package.
#'
#' The following sets of files are included:
#'
#' 1. `Coartic_Block1_001P00XS1` - Data for a block of trials from an eyetracking
#'    performed with a Tobii Eyetracker in an Eprime experiment.
#' 2. `Coartic_Block2_001P00XS1` - Data for a second block of trials from the
#'    above experiment.
#' @export
example_files <- function(which = 1) {
  files <- list(
    c(
      "coartic-tobii/Coartic_Block1_001P00XS1.gazedata",
      "coartic-tobii/Coartic_Block1_001P00XS1.txt",
      "coartic-tobii/Coartic.yaml"
    ),
    c(
      "coartic-tobii/Coartic_Block2_001P00XS1.gazedata",
      "coartic-tobii/Coartic_Block2_001P00XS1.txt",
      "coartic-tobii/Coartic.yaml"
    )
  )

  system.file(files[[which]], package = "littlelisteners")
}
