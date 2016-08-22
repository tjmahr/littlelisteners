
#' Aggregate looks
#'
#' @param data a long data frame of looking data
#' @param resp_def a "response definition" list that describes eyetracking
#'   response values. The list should have elements named "primary", "others",
#'   "elsewhere", and "missing".
#' @param formula an aggregation formula. The lefthand terms will be grouping
#'   variables, and the righthand term is the column with eyetracking responses.
#' @return a dataframe of the grouping columns with columns plus columns with
#'   the number of responses and the proportion/se of looks to the primary
#'   response type and the proportion of missing data
#' @seealso aggregate_looks_ for a version that uses regular evaluation on
#'   character values of column names.
#' @export
aggregate_looks <- function(data, resp_def, formula) {
  resp_col <- as.character(formula)[3]
  grouping <- all.vars(formula[[2]])
  aggregate_looks_(data, resp_def, grouping, resp_col)
}


#' Standard-evaluation version of `aggregate_looks`
#'
#' @inheritParams aggregate_looks
#' @param grouping Grouping columns.
#' @param resp_col Name of the column that contains eyetracking responses.
#' @export
aggregate_looks_ <- function(data, resp_def, grouping, resp_col) {
  # Check that all response types have a coding
  resp_vals <- unique(data[[resp_col]])
  check_resp_def(resp_def, resp_vals)

  # Check that a grouping column name doesn't show up as a gaze column name
  reserved_name <- c(with_na_label(resp_vals), "Others", "Elsewhere",
                     "Prop", "PropSE", "PropMissing")

  if (any(grouping %in% reserved_name)) {
    illegal_names <- grouping[grouping %in% reserved_name] %>%
      paste0(collapse = ", ")

    stop("Please rename the following grouping variables: ", illegal_names)
  }

  data_wide <- data %>%
    group_by_(.dots = grouping) %>%
    count_(resp_col) %>%
    ungroup %>%
    tidyr::spread_(key_col = resp_col, value_col = "n", fill = 0) %>%
    tibble::remove_rownames()

  data_wide$Elsewhere <- data_wide %>%
    maybe_row_sums(with_na_label(resp_def$elsewhere))

  data_wide$Missing <- data_wide %>%
    maybe_row_sums(with_na_label(resp_def$missing))

  data_wide$Others <- data_wide %>%
    maybe_row_sums(with_na_label(resp_def$others))

  data_wide <- data_wide %>%
    drop_one_of(with_na_label(resp_def$missing)) %>%
    drop_one_of(with_na_label(resp_def$elsewhere))

  data_wide$Primary <- data_wide %>%
    maybe_row_sums(with_na_label(resp_def$primary))

  data_wide %>%
    mutate_(Looks = ~ Primary + Others + Elsewhere + Missing,
            Prop = ~ Primary / (Others + Primary),
            PropSE = ~ se_prop(Prop, Others + Primary),
            PropNA = ~ Missing / Looks)
}


check_resp_def <- function(resp_def, observed_resp_vals) {
  possible_resp_vals <- unlist(resp_def)
  missing <- setdiff(observed_resp_vals, possible_resp_vals)

  if (length(missing) != 0) {
    bad <- paste0(missing, collapse = ", ")
    stop("Values not found in response definition: ", bad, call. = FALSE)
  }
  NULL
}

with_na_label <- function(xs, na_label = "<NA>") {
  xs[is.na(xs)] <- na_label
  xs
}

drop_one_of <- function(.data, vars) {
  matches <- intersect(colnames(.data), vars)
  if (length(matches) != 0) {
    .data <- select(.data, -one_of(matches))
  }
  .data
}

maybe_row_sums <- function(df, col_names) {
  matches <- intersect(colnames(df), col_names)
  if (length(matches) == 0) {
    rep_len(0, nrow(df))
  } else {
    rowSums(df[matches])
  }
}

se_prop <- function(proportion, n_possible) {
  spread <- proportion * (1 - proportion)
  sqrt(spread / n_possible)
}
