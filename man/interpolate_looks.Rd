% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/interpolation.R
\name{interpolate_looks}
\alias{interpolate_looks}
\title{AOI-based gaze interpolation}
\usage{
interpolate_looks(
  x,
  window,
  fps,
  response_col,
  interp_col,
  fillable,
  missing_looks
)
}
\arguments{
\item{x}{a dataframe of grouped eyetracking data. Each row should be a single
frame of eyetracking. Use \code{dplyr::group_by()} to set the grouping
columns for the data. The groups should specify a single trial of
eyetracking data.}

\item{window}{maximum amount of missing data (milliseconds) that can be
interpolated. Only spans of missing data with less than or equal to this
duration will be interpolated}

\item{fps}{number of eyetracking frames (dataframe rows) per second}

\item{response_col}{(character) name of the column with the eyetracking response data}

\item{interp_col}{(character) name of a column to add to the dataframe. This column
records whether each frame was interpolated (TRUE) or not (FALSE)}

\item{fillable}{values in the response column where interpolation is legal.
These would typically be AOI locations.}

\item{missing_looks}{values that can be imputed.}
}
\description{
Fills in windows of missing data if the same AOI is fixated at
beginning and end of the missing data window. For example, the sequence
\verb{"Target", NA, NA, "Target"} would be interpolated to be \verb{"Target", "Target", "Target", "Target"}.
}
\details{
Use \code{window} to constrain the duration of missing data windows that
can be filled. We conventionally use 150ms because we would not expect
someone to shift their gaze from Image A to Image B to Image A in that
amount of time.
}
\examples{
# We have time in ms, measured at 60 fps, and
# we want to fill in gaps of 100 ms.
looks <- tibble::tribble(
  ~Subject, ~Trial, ~Time,    ~AOI,         ~Hint,
       "A",      1,  1000,  "Left",     "present",
       "A",      1,  1017,  "Left",     "present",
       "A",      1,  1034,      NA,   "legal gap",
       "A",      1,  1051,      NA,   "legal gap",
       "A",      1,  1068,      NA,   "legal gap",
       "A",      1,  1084,  "Left",     "present",
       "A",      1,  1100,      NA, "illegal gap",
       "A",      2,   983,  "Left",     "present",
       "A",      2,  1000, "Right",     "present",
       "A",      2,  1017,      NA, "illegal gap",
       "A",      2,  1034,      NA, "illegal gap",
       "A",      2,  1051,      NA, "illegal gap",
       "A",      2,  1068,      NA, "illegal gap",
       "A",      2,  1084,      NA, "illegal gap",
       "A",      2,  1100,      NA, "illegal gap",
       "A",      2,  1118,      NA, "illegal gap",
       "A",      2,  1135, "Right",     "present",
)

# Note that only the "legal gap" rows were interpolated
looks |>
  dplyr::group_by(Trial) |>
  interpolate_looks(
    window = 100,
    fps = 60,
    response_col = "AOI",
    interp_col = "Interpolated",
    fillable = c("Left", "Right"),
    missing_looks = NA
  )
}
