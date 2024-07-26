#' Find the most frequent interval of values
#'
#' When we have trials of varying lengths, it can be helpful to find the
#' interval of time shared by most of the trials.
#' @param xs some set of values (times)
#' @param min_freq the minimum frequency of times to keep
#' @return a list with the minimum and maximum values that have a frequency of
#' at least 80%.
#' @keywords internal
#' @export
#' @examples
#' times <- c(
#'   -5:10, -10:10, -8:10, -8:10, -8:11, -8:13, -8:4,
#'   -5:12, -10:9
#' )
#' find_frequent_interval(times)
find_frequent_interval <- function(xs, min_freq = .80) {
  df <- tibble::tibble(x = xs)
  most_frequent_values <- df |>
    dplyr::filter(!is.na(.data$x)) |>
    dplyr::count(.data$x) |>
    dplyr::mutate(frequency = .data$n / max(.data$n)) |>
    dplyr::filter(min_freq <= .data$frequency)

  list(
    lower = min(most_frequent_values$x),
    upper = max(most_frequent_values$x)
  )
}

#' Adjust looking times relative to some event
#'
#' This function is useful if some critical event occurs each trial, and we
#' would like to adjust the timestamps so that they are relative to that event
#' time.
#'
#' @param data a long data frame of looking data
#' @param time_var a column in `data` with looking times (assumed to be
#'   milliseconds).
#' @param event_var a column in `data` with the time of some event
#' @param ... grouping variables. The grouping variables should uniquely
#'   specify a trial of eyetracking data.
#' @param align whether to align the eyetracking times so that the frame closest
#'   to the event time gets time = 0.
#' @param fps the eyetracking sampling rate. Defaults to 60 frames per second.
#' @param ties how to break ties when the smallest times are equally close to
#'   zero. Default is `"first"` so that the tie `c(-1, 1)` is aligned to
#'   `c(0, 2)`.
#' @return the looking data with the times adjusted by event times. By default,
#'   these times are aligned so that the frame closest to the event time gets
#'   value 0.
#' @export
#' @examples
#' # Consider some raw tims from an eyetrack. For each trial, some critical
#' # event occurs and we have a column with the time of that event for each
#' # trial.
#' trial1 <- data.frame(trial = 1, time_ms = 1:5, event = 2)
#' trial2 <- data.frame(trial = 2, time_ms = 6:10, event = 8.5)
#' trial_times <- dplyr::bind_rows(trial1, trial2)
#' trial_times
#'
#' # We want to adjust the times so that time 0 is time of the critical event.
#' adjust_times(trial_times, time_ms, event, trial, fps = 1000)
#'
#' # The times are adjusted so that the frame closest to the event time gets
#' # the time zero. Setting `align` to `FALSE` skips this behavior.
#' adjust_times(trial_times, time_ms, event, trial, align = FALSE, fps = 1000)
#'
#' # In the second trial there is a tie. Two frames are equally close to 0. By
#' # default the first frame is chosen to be zero, but setting `ties` to
#' # `"last"` will break ties with the later frame.
#' adjust_times(trial_times, time_ms, event, trial, ties = "last", fps = 1000)
adjust_times <- function(
    data,
    time_var = quote(Time),
    event_var = NULL,
    ...,
    align = TRUE,
    fps = 60,
    ties = "first"
) {
  time_var <- enquo(time_var)
  event_var <- enquo(event_var)
  dots <- quos(...)

  adjusted_times <-
    dplyr::pull(data, !! time_var) - dplyr::pull(data, !! event_var)
  data[[quo_name(time_var)]] <- adjusted_times

  if (!align) {
    data
  } else {
    data |>
      dplyr::group_by(!!! dots) |>
      adjust_times_around_zero(
        time_col = quo_name(time_var),
        fps = fps,
        ties = ties
      ) |>
      ungroup()
  }
}

#' Adjust time value around 0
#'
#' @param data a dataframe of eyetracking data for a single trial or a grouped
#'   dataframe where the groups define a single trial.
#' @param time_col name (string) of the column with time value. Defaults to
#'   `"Time"`.
#' @return the dataframe with updated time values
#' @inheritParams adjust_times
#' @export
adjust_times_around_zero <- function(
    data,
    time_col = "Time",
    fps = 60,
    ties = "first"
) {
  if (!inherits(data, "grouped_df")) {
    stop("Use `group_by()` or set `...` to name grouping columns.\n",
         "  Grouping variables should select a single eyetracking trial.")
  }

  dplyr::group_modify(
    data,
    ~ adjust_times_around_zero_one(
      x = .,
      time_col = time_col,
      fps = fps,
      ties = ties
    )
  )
}


adjust_times_around_zero_one <- function(
    x,
    time_col = "Time",
    fps = 60,
    ties = "first"
) {
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
  closest_rows[tie_func(closest_rows)]
}

