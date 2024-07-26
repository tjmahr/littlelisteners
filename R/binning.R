
#' Assign bins for downsampling looking data
#'
#' @param data a dataframe of looking data
#' @param bin_width the number of items to put in each bin. Default is 3.
#' @param time_var the name of the column representing time
#' @param ... grouping variables
#' @param bin_col name of the column to add. Defaults to `".bin"`.
#' @param na_location Where to assign `NA` bin numbers. `"head"` and `"tail"`
#'   respectively put the `NA` elements at the head and tail of the vector;
#'   `"split"` alternates between `"tail"` and `"head"`.
#' @param partial whether to exclude values that don't fit evenly into bins.
#'   Defaults to `FALSE`, so that the user is warned if a bin is incomplete.
#' @return the original dataframe with an added column of bin numbers. The
#'   dataframe will be sorted by the grouping and time variables.
#' @export
assign_bins <- function(data, bin_width = 3, time_var, ..., bin_col = ".bin",
                        na_location = "tail", partial = FALSE) {
  dots <- quos(...)
  time_var <- enquo(time_var)

  minimal_vars <- c(dots, time_var)
  minimal_data <- data |> distinct(!!! minimal_vars)

  if (bin_col %in% names(data)) {
    msg <- paste0(
      "There is already a column named ", expr_label(bin_col),
      ".\n  Please set `bin_col` to a different name.")
    stop(call. = FALSE, msg)
  }

  if (nrow(data) != nrow(minimal_data)) {
    names <- as.character(lapply(dots, f_rhs))
    msg <- paste0(
      "Grouping variables do not uniquely specify data: \n  Groups: ",
      expr_text(names),
      "\n  Are you missing a grouping variable?")

    stop(call. = FALSE, msg)
  }

  data |>
    group_by(!!! dots) |>
    arrange(!!! minimal_vars) |>
    mutate(
      !! bin_col := assign_bins_vec(
        !! time_var,
        bin_width,
        na_location,
        partial
      )
    ) |>
    ungroup()
}

#' Truncate times to fit bin width
#'
#' Samples of eyetracking data are excluded so that the number of frames is
#' evenly divisible by a given bin width. For example, given a bin width of 3
#' frames, a trial with 181 frames would lose 1 frame. The frames aligned so
#' that a key time value have a specific position in a bin. For example, setting
#' time 0 to position 1 will truncate the times so that time 0 will be the first
#' frame inside of its bin.
#'
#' @inheritParams assign_bins
#' @param key_time,key_position arguments controlling the trimming. The given
#'   time value (`key_time`) will have a specific position within a bin
#'   (`key_position`). For example, given a value of 0 and position of 2, the
#'   trimming will force the frame with time 0 to fall in the second frame of
#'   its bin.
#' @param min_time,max_time optional arguments controlling the trimming. If
#'   used, the time values are filtered to exclude whole bins of frames before
#'   `min_time` and after `max_time`.
#' @return the original dataframe with its time column trimmed to make it easier
#'   to  bin time values into groups of `bin_width`.
#' @export
#' @examples
#' data1 <- tibble(
#'   task = "testing",
#'   id = "test1",
#'   time = -4:6,
#'   frame = seq_along(time))
#'
#' data2 <- tibble(
#'   task = "testing",
#'   id = "test2",
#'   time = -5:5,
#'   frame = seq_along(time))
#'
#' # Number of rows per id is divisible by bin width
#' # and time 0 is center of its bin
#' bind_rows(data1, data2) |>
#'   trim_to_bin_width(3, key_time = 0, key_position = 2, time, id) |>
#'   assign_bins(3, time, id) |>
#'   group_by(id, .bin) |>
#'   dplyr::mutate(center_time = median(time))
#'
#' # And exclude times in bins before some minimum time
#' bind_rows(data1, data2) |>
#'   trim_to_bin_width(3, key_time = 0, key_position = 2, time, id,
#'                     min_time = -1) |>
#'   assign_bins(3, time, id)
#'
#' # And exclude times in bins after some maximum time
#' bind_rows(data1, data2) |>
#'   trim_to_bin_width(3, key_time = 0, key_position = 2, time, id,
#'                     min_time = -1, max_time = 4) |>
#'   assign_bins(3, time, id)
trim_to_bin_width <- function(data, bin_width = 3, key_time = NULL,
                              key_position = 1, time_var, ...,
                              min_time = NULL, max_time = NULL) {
  stopifnot(!is.null(key_time))
  dots <- quos(...)
  time_var <- enquo(time_var)

  minimal_vars <- c(dots, time_var)
  minimal_data <- data |> distinct(!!! minimal_vars)

  if (nrow(data) != nrow(minimal_data)) {
    names <- as.character(lapply(dots, f_rhs))
    msg <- paste0(
      "Grouping variables do not uniquely specify data: \n  Groups: ",
      expr_text(names),
      "\n  Are you missing a grouping variable?")

    stop(call. = FALSE, msg)
  }

  # # Fast alternative: Do all the times without grouping
  # if (!is.null(min_time) & !is.null(max_time)) {
  #   minimal_data |>
  #     group_by(!!! dots) |>
  #     summarise(.min_time = min(!! time_var),
  #               .max_time = max(!! time_var)) |>
  #     ungroup() |>
  #     summarise()
  #
  #   time_values <- data |>
  #     pull(!! time_var) |>
  #     unique() |>
  #     sort()
  #
  #   to_keep <- time_values |>
  #     determine_frame_trimming(bin_width, key_time, key_position,
  #                              min_time, max_time)
  #   times_to_keep <- range(time_values[to_keep])
  #
  #   data |>
  #     filter(between(UQ(time_var), times_to_keep[1], times_to_keep[2]))
  # } else {
  data |>
    group_by(!!! dots) |>
    dplyr::filter(
      determine_frame_trimming(!! time_var, bin_width, key_time,
                               key_position, min_time, max_time)) |>
    ungroup()
  # }


}



determine_frame_trimming <- function(times, bin_width = 3, key_time = NULL,
                                     key_position = NULL, min_time = NULL,
                                     max_time = NULL) {
  key_position <- key_position %||% 1

  if (key_position > bin_width) {
    warning("Key position ", key_position, " larger than bin width ",
            bin_width, call. = FALSE)
    key_position <- key_position %% bin_width
    if (key_position == 0) key_position <- bin_width
  }

  raw_times <- times
  times <- sort(times)

  # Default to times that are one bin from edges
  min_was_null <- is.null(min_time)
  max_was_null <- is.null(max_time)
  min_time <- min_time %||% times[bin_width + 1]
  max_time <- max_time %||% times[length(times) - bin_width]

  min_frame <- which.min(abs(times - min_time))
  max_frame <- which.min(abs(times - max_time))

  # Add a bin of slack on either side if possible.
  if (min_frame > bin_width) {
    min_frame <- min_frame - bin_width
  }

  if ((max_frame + bin_width) <= length(times)) {
    max_frame <- max_frame + bin_width
  }

  trim_min_time <- times[min_frame]
  trim_max_time <- times[max_frame]

  curr_times <- times[seq(min_frame, max_frame)]

  key_position <- key_position %||% 1
  key_time <- key_time %||% min_time
  key_frame <- which.min(abs(curr_times - key_time))

  # Repeat frame positions from the key frame
  kernel <- key_frame + (seq_len(bin_width) - key_position)

  # Reverse in order to repeat to the left
  until_kernel <- seq_len(min(kernel) - 1)
  first_half <- rev(rep_along(until_kernel, rev(kernel)))

  # Repeat to the right
  until_end <- seq(min(kernel), length(curr_times))
  other_half <- rep_along(until_end, kernel)

  kernel_long <- c(first_half, other_half)

  trimmed <- tibble(
    frame_times = curr_times,
    frames = seq_along(frame_times),
    bins = .data$frames - kernel_long
  ) |>
    group_by(.data$bins) |>
    mutate(
      n_frames = n(),
      frames_in_bin = seq_len(n()),
      right_size = .data$n_frames == bin_width,
      after_min = min_was_null | max(.data$frame_times) > min_time,
      before_max = max_was_null | min(.data$frame_times) < max_time
    ) |>
    ungroup() |>
    filter(.data$right_size, .data$after_min, .data$before_max)

  trimmed_times <- trimmed |> pull(.data$frame_times)
  key_time_frame <- which.min(abs(trimmed_times - key_time))
  stopifnot(
    key_time_frame %% bin_width == key_position %% bin_width,
    length(trimmed_times) %% bin_width == 0
  )

  raw_times %in% trimmed_times
}




#' Assign bin numbers to a vector
#'
#' The first step in binning/down-sampling some data is assigning items to bins.
#' This function takes a vector and a bin size and returns the bin assignments.
#'
#' @keywords internal
#' @param xs a vector
#' @inheritParams assign_bins
#' @return a vector of bin-numbers. If `bin_width` does not evenly divide
#'   `xs`, the remainder elements are given a bin number of `NA`.
#' @examples
#' assign_bins_vec(1:14, bin_width = 3, "head")
#' # [1] NA NA  1  1  1  2  2  2  3  3  3  4  4  4
#' assign_bins_vec(1:14, bin_width = 3, "tail")
#' # [1]  1  1  1  2  2  2  3  3  3  4  4  4 NA NA
#' assign_bins_vec(1:7, bin_width = 5, "split")
#' # [1] NA  1  1  1  1  1 NA
#' assign_bins_vec(1:8, bin_width = 5, "split")
#' # [1] NA  1  1  1  1  1 NA NA
assign_bins_vec <- function(xs, bin_width = 3, na_location = "tail", partial = FALSE) {
  if (is.unsorted(xs)) {
    warning("Elements to be binned are not sorted")
  }

  if (length(xs) != length(unique(xs))) {
    warning("Elements to be binned are not unique")
  }

  num_bins <- floor(length(xs) / bin_width)
  leftover <- length(xs) %% bin_width
  bin_indices <- sort(rep(seq_len(num_bins), times = bin_width))

  if (partial) {
    partial_bin <- rep(max(bin_indices) + 1, leftover)
    bin_indices <- c(bin_indices, partial_bin)
    leftover <- 0
  }

  if (na_location == "head") {
    bin_indices <- c(rep(NA, leftover), bin_indices)
  } else if (na_location == "tail") {
    bin_indices <- c(bin_indices, rep(NA, leftover))
  } else if (na_location == "split") {
    first <- floor(leftover / 2)
    last <- ceiling(leftover / 2)
    bin_indices <-  c(rep(NA, first), bin_indices, rep(NA, last))
  }

  lost_values <- xs[which(is.na(bin_indices))]

  if (length(lost_values) > 0) {
    listed_values <- paste0(lost_values, collapse = (", "))
    warning("Some values were not assigned to a bin: ", listed_values)
  }

  bin_indices
}


