#' AOI-based gaze interpolation
#'
#' @description Fills in windows of missing data if the same AOI is fixated at
#'   beginning and end of the missing data window. For example, the sequence
#'   `"Target", NA, NA, "Target"` would be interpolated to be `"Target",
#'   "Target", "Target", "Target"`.
#'
#' @param x a dataframe of grouped eyetracking data. Each row should be a single
#'   frame of eyetracking. Use \code{dplyr::group_by()} to set the grouping
#'   columns for the data. The groups should specify a single trial of
#'   eyetracking data.
#' @param window maximum amount of missing data (milliseconds) that can be
#'   interpolated. Only spans of missing data with less than or equal to this
#'   duration will be interpolated
#' @param fps number of eyetracking frames (dataframe rows) per second
#' @param response_col name of the column with the eyetracking response data
#' @param interp_col name of a column to add to the dataframe. This column
#'   records whether each frame was interpolated (TRUE) or not (FALSE)
#' @param fillable values in the response column where interpolation is legal.
#'   These would typically be AOI locations.
#' @param missing_looks values that can be imputed
#'
#' @details Use `window` to constrain the duration of missing data windows that
#'   can be filled. We conventionally use 150ms because we would not expect
#'   someone to shift their gaze from Image A to Image B to Image A in that
#'   amount of time.
#' @export
interpolate_looks <- function(x, window, fps, response_col, interp_col,
                                  fillable, missing_looks) {
  if (!inherits(x, "grouped_df")) {
    stop("Please use group_by to set grouping columns.\n",
         "  Grouping variables should select a single eyetracking trial.")
  }

   do_(x, ~interpolate_looks_one(
    x = .,
    window = window,
    fps = fps,
    response_col = response_col,
    interp_col = interp_col,
    fillable = fillable,
    missing_looks = missing_looks))
}


interpolate_looks_one <- function(x, window, fps, response_col, interp_col,
                              fillable, missing_looks) {
  is_missing_look <- function(xs) xs %in% missing_looks
  trial <- x

  # Convert window duration (ms) into the number of frames, rounded down.
  ms_per_frame <- 1000 / fps
  frames_in_window <- floor(window / ms_per_frame)

  # Extract the gazes from the trial. Record how many missing frames there are.
  gazes <- trial[[response_col]]

  missing <- sum(is_missing_look(gazes))

  # Grab all the non-NA gaze frames.
  tracked <- which(!is_missing_look(gazes))

  # The lag in frame numbers of non-NA gazes tells us how many NA frames were
  # skipped when we extracted all the non-NA gazes. Include the 0 at front
  # because diff(1:n) returns n-1 values
  differences <- diff(c(0, tracked))

  ## Find starts and ends of each NA gap

  # Locations from `which` are not accurate because they don't take into account
  # earlier missing frames. Use the cumulative sum of missing frames to correct
  # these start locations.
  gap_start <- which(1 < differences)
  gap_size <- differences[gap_start] - 1
  total_gap_sizes <- cumsum(gap_size)

  # First gap doesn't need to be offset
  start_offsets <- c(0, total_gap_sizes[-length(total_gap_sizes)])
  gap_start <- gap_start + start_offsets - 1
  gap_end <- gap_start + gap_size + 1

  # Enforce valid windows! Margins need to be non-NA and next to an NA value
  stopifnot(
    is_missing_look(gazes[c(gap_start + 1, gap_end - 1)]),
    !is_missing_look(gazes[c(gap_start, gap_end)])
  )

  # Make a set of Gap objects from these start/end/size descriptions
  gaps <- Map(Gap, gap_start, gap_end, gap_size)

  # Only fill gaps no bigger than the interpolation window, gaps that don't
  # involve first frame and gaps with the gaze location on both sides of window
  has_legal_length <- function(gap) gap$na_size <= frames_in_window
  is_not_first_frame <- function(gap) gap$start != 0
  is_fillable <- function(gap) gazes[gap$start] == gazes[gap$end]
  has_legal_aois <- function(gap) gazes[gap$start] %in% fillable

  gaps <- Filter(has_legal_length, gaps)
  gaps <- Filter(is_not_first_frame, gaps)
  gaps <- Filter(is_fillable, gaps)
  gaps <- Filter(has_legal_aois, gaps)

  # Fill each gap
  for (gap in gaps) {
    filler <- gazes[gap$start]
    trial[gap$na_seq, response_col] <- filler
  }

  # Record whether each frame was imputed
  imputed <- unlist(lapply(gaps, getElement, "na_seq"))
  trial[[interp_col]] <- FALSE
  trial[imputed, interp_col] <- TRUE

  # Update the AOI Data using the interpolated values
  trial
}

# Simple container for the information we care about when interpolating a gap
Gap <- function(start, end, na_size) {
  structure(list(
    start = start, end = end, na_size = na_size,
    seq = seq(start, end), na_seq = seq(start + 1, end - 1)),
    class = c("Gap", "list"))
}
