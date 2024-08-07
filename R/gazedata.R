#' Load a \code{.gazedata} file for an experiment
#'
#' Loads \code{.gazedata} file created by an Eprime experiment running on a
#' Tobii eyetracker, and performs typical data reduction on that file.
#'
#' @param gazedata_path path to the \code{.gazedata} file that is to be parsed.
#' @param eyes string describing which eye(s) should be selected for the
#'   \code{Mean} columns. Valid options are \code{"both"}, \code{"left"}, and
#'   \code{"right"}. Defaults to \code{"both"}. If \code{"left"} is selected,
#'   then only the left eye is used to calculate the \code{XMean}, \code{YMean},
#'   etc. columns.
#' @param means_need_both logical value indicating if both eyes are required to
#'   compute \code{Mean} columns. Defaults to \code{FALSE}. If \code{FALSE},
#'   \code{NA} values are ignored, so for example, \code{XMean} could be
#'   computed from an \code{XLeft} of .25 and an \code{XRight} of \code{NA}.
#' @param apply_corrections whether to do low-level adjustments like coding
#'   offscreen looks as NA, negative pupil diameters to NA, negative distances
#'   to NA, and flip y-axis so the origin is the lower-left corner. Defaults to
#'   \code{TRUE}. Only used as \code{FALSE} in case "raw" data is needed.
#' @return A dataframe containing the parsed gazedata. Each row of the dataframe
#'   contains the eye-tracking data for a single frame of time recorded during
#'   the experiment.
#' @details
#' We extract the columns the following columns: \code{TrialId}, \code{RTTime},
#' \code{XGazePosLeftEye}, \code{XGazePosRightEye}, \code{YGazePosLeftEye},
#' \code{YGazePosRightEye}, \code{DistanceLeftEye}, \code{DistanceRightEye},
#' \code{DiameterPupilLeftEye} and \code{DiameterPupilRightEye}.
#'
#' Once these column values are loaded, we make three modifications to the
#' gazedata (when \code{apply_corrections} is TRUE).
#'
#' \enumerate{
#'   \item Gaze measurements with \code{Validity} codes greater than or equal to
#'   1 are replaced with NA values.
#'
#'   \item X,Y gaze values are defined in screen proportions. Values that fall
#'   outside [0,1] are outside of the boundaries of the screen and therefore are
#'   nonsensical. Replace them with \code{NA}. We perform a similar correction
#'   on pupil diameters and eye-distances by replacing negative
#'   values with \code{NA}.
#'
#'   \item The origin of the screen is the upper-left-hand corner of the screen.
#'   Flip the y-values so that the origin is in a more familiar position in the
#'   lower-left-hand corner of the screen. This way, low y values are closer to
#'   the bottom of the screen.
#'
#'   \item Compute the mean x, y, distance and diameter values for the left and
#'   right eyes. \code{NA} values are ignored when computing the mean, so the
#'   pair \code{(XLeft = NA, XRight = .5)} yields \code{XMean = .5}.
#' }
#'
#' @references \href{http://bit.ly/1AtKyhR}{Tobii Toolbox for Matlab: Product
#'   Description & User Guide}
#' @export
#' @noMd
#' @examples
#' gazedata_path <- example_files(1)[1]
#' read_gazedata(gazedata_path)
read_gazedata <- function(
    gazedata_path,
    eyes = "both",
    means_need_both = FALSE,
    apply_corrections = TRUE
) {
  stopifnot(
    length(gazedata_path) == 1,
    length(eyes) == 1,
    length(means_need_both) == 1,
    length(apply_corrections) == 1
  )

  if (eyes != "both" & means_need_both == TRUE) {
    stop("Cannot average across both eyes if only one eye selected")
  }

  if (!(eyes %in% c("both", "left", "right"))) {
    stop("`eyes` argument should be 'both', 'left' or 'right'")
  }

  gazedata <- gazedata_path |>
    utils::read.delim(
      na.strings = c('-1.#INF', '1.#INF'),
      stringsAsFactors = FALSE
    ) |>
    tibble::as_tibble()


  # Select/rename columns with experiment information (timing and trial
  # number) and gaze measurements from each eye
  cols_to_keep <- c(
    TrialNo = "TrialId", Time = "RTTime", TobiiTime = "TETTime",
    XLeft = "XGazePosLeftEye", XRight = "XGazePosRightEye",
    YLeft = "YGazePosLeftEye", YRight = "YGazePosRightEye",
    ZLeft = "DistanceLeftEye", ZRight = "DistanceRightEye",
    ValidityLeft = "ValidityLeftEye", ValidityRight = "ValidityRightEye",
    DiameterLeft = "DiameterPupilLeftEye",
    DiameterRight = "DiameterPupilRightEye"
  )

  gazedata <- gazedata |>
    dplyr::select(tidyselect::any_of(cols_to_keep)) |>
    dplyr::mutate(
      Origin = "UpperLeft",
      Basename = tools::file_path_sans_ext(basename(gazedata_path))
    )

  # Set some shortcuts
  measures <- c("X", "Y", "Z", "Diameter")
  measures_L <- paste0(measures, "Left")
  measures_R <- paste0(measures, "Right")

  if (apply_corrections) {
    # From the Tobii manual, "Validity codes should be used for data filtering to
    # remove data points that are obviously incorrect. If you export the raw data
    # file, we recommend removing all data points with a validity code of 2 or
    # higher."
    invalid_L <- which(2 <= gazedata$ValidityLeft)
    invalid_R <- which(2 <= gazedata$ValidityRight)
    gazedata[invalid_L, measures_L] <- NA
    gazedata[invalid_R, measures_R] <- NA

    screen_cols <- c("XLeft", "XRight", "YLeft", "YRight")
    distances <- c("ZLeft", "ZRight", "DiameterLeft", "DiameterRight")

    gazedata <- gazedata |>
      dplyr::mutate(
        dplyr::across(tidyselect::all_of(screen_cols), correct_offscreen),
        dplyr::across(tidyselect::all_of(distances), correct_distances),
        YLeft = 1 - .data$YLeft,
        YRight = 1 - .data$YRight,
        Origin = "LowerLeft"
      )
  }

  if (eyes == "left") {
    gazedata <- gazedata |>
      mutate(
        XMean = .data$XLeft,
        YMean = .data$YLeft,
        ZMean = .data$ZLeft,
        DiameterMean = .data$DiameterLeft
      )
  }

  if (eyes == "right") {
    gazedata <- gazedata |>
      mutate(
        XMean = .data$XRight,
        YMean = .data$YRight,
        ZMean = .data$ZRight,
        DiameterMean = .data$DiameterRight
      )
  }

  if (eyes == "both") {
    # A "monocular mean" averages both eyes together. If data is available in just
    # one eye, use the available value as mean, unless we need_both is TRUE.
    compute_monocular_mean <- function(x1, x2, need_both = means_need_both) {
      xm <- rowMeans(cbind(x1, x2), na.rm = !need_both)
      # NaN -> NA
      ifelse(is.nan(xm), NA, xm)
    }

    gazedata <- gazedata |>
      dplyr::mutate(
        XMean = compute_monocular_mean(.data$XLeft, .data$XRight),
        YMean = compute_monocular_mean(.data$YLeft, .data$YRight),
        ZMean = compute_monocular_mean(.data$ZLeft, .data$ZRight),
        DiameterMean = compute_monocular_mean(
          .data$DiameterLeft,
          .data$DiameterRight
        )
      )
  }
  # Re-order the columns of gazedata.
  cols_in_order <- c(
    "Basename", "TrialNo", "TobiiTime", "Time", "Origin",
    "XLeft", "XRight", "XMean",
    "YLeft", "YRight", "YMean",
    "ZLeft", "ZRight", "ZMean",
    "DiameterLeft", "DiameterRight", "DiameterMean")
  gazedata <- gazedata[cols_in_order]
  gazedata
}



# Replace all values of gazedata that fall beyond [0, 1] (offscreen) with NA.
correct_offscreen <- function(gaze) {
  ifelse(gaze < 0 | 1 < gaze, NA, gaze)
}

# Correct values of gazedata that cannot be negative (distances, diameters)
correct_distances <- function(gaze) {
  ifelse(gaze < 0, NA, gaze)
}
