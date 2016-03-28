
#' @export
adjust_times_around_zero <- function(x, time_col = "Time", fps = 60, ties = "first") {
  if (!inherits(x, "grouped_df")) {
    stop("Please use group_by to set grouping columns.\n",
         "  Grouping variables should select a single eyetracking trial.")
  }

  do_(x, ~ adjust_times_around_zero_one(
    x = .,
    time_col = time_col,
    fps = fps,
    ties = ties))
}


adjust_times_around_zero_one <- function(x, time_col = "Time", fps = 60, ties = "first") {
  times <- x[[time_col]]
  zero_frame <- find_nearest_zero_frame(times, ties)
  centered_frames <- seq_along(times) - zero_frame
  time_rate <- 1000 / fps

  x[[time_col]] <- centered_frames * time_rate
  x
}


find_nearest_zero_frame <- function(times, ties = "first") {
  tie_func <- if (ties == "first") which.min else which.max
  time_distance <- abs(times)
  closest_rows <- which(time_distance == min(time_distance))
  # Break ties: Select the (first) row that contains the smallest Time value
  closest_rows[tie_func(times)]
}
