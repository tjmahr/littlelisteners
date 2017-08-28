
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
assign_bins <- function(data, bin_width = 3, time_var, ..., bin_col = ".bin", na_location = "tail", partial = FALSE) {
  dots <- quos(...)
  time_var <- enquo(time_var)

  minimal_vars <- c(dots, time_var)
  minimal_data <- data %>% distinct(!!! minimal_vars)

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

  data %>%
    group_by(!!! dots) %>%
    arrange(!!! minimal_vars) %>%
    mutate(!! bin_col := assign_bins_vec(!! time_var, bin_width,
                                         na_location, partial)) %>%
    ungroup()
}

#' Assign bin numbers to a vector
#'
#' The first step in binning/down-sampling some data is assigning items to bins.
#' This function takes a vector and a bin size and returns the bin assignments.
#'
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


