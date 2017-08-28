


#' Aggregate looks
#'
#' This function uses an aggregation formula.
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
#' @seealso `aggregate_looks2` for a version that does not use a formula.
#' @export
aggregate_looks <- function(data, resp_def, formula) {
  resp_var <- quo(!! formula[[3]])
  grouping <- quos(!!! syms(all.vars(formula[[2]])))
  aggregate_looks2(data, resp_def, !! resp_var, !!! grouping)
}

#' Create a response definition
#' @param primary the primary response of interest
#' @param others other responses of interest
#' @param elsewhere responses to ignore
#' @param missing responses that indicate missing data. Defaults to NA.
#' @return a list
#' @export
#' @rdname response-definition
create_response_def <- function(primary, others, elsewhere = NULL, missing = NA) {
  list(
    primary = primary,
    others = others,
    elsewhere = elsewhere,
    missing = missing
  )
}

#' Alternative function for aggregating looks
#'
#' @inheritParams aggregate_looks
#' @param resp_var Name of the column that contains eyetracking responses.
#' @param ... Grouping columns.
#' @return a dataframe of the grouping columns with columns plus columns with
#'   the number of responses and the proportion/se of looks to the primary
#'   response type and the proportion of missing data
#' @export
aggregate_looks2 <- function(data, resp_def, resp_var, ...) {
  grouping <- quos(...)
  grouping_data <- data %>% distinct(!!! grouping)
  resp_var <- enquo(resp_var)

  # Check that all response types have a coding
  resp_vals <- unique(pull(data, !! resp_var))
  check_resp_def(resp_def, resp_vals)

  # Check that a grouping column name doesn't show up as a gaze column name
  reserved_name <- c(with_na_label(resp_vals), "Others", "Elsewhere",
                     "Prop", "PropSE", "PropMissing")

  if (any(grouping %in% reserved_name)) {
    group_names <- names(grouping_data)
    illegal_names <- group_names[grouping %in% reserved_name] %>%
      paste0(collapse = ", ")

    stop("Please rename the following grouping variables: ", illegal_names)
  }
  n <- sym("n")

  data_wide <- data %>%
    group_by(!!! grouping) %>%
    count(!! resp_var) %>%
    ungroup() %>%
    tidyr::spread(!! resp_var, !! n, fill = 0) %>%
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

  primary <- sym("Primary")
  others <- sym("Others")
  elsewhere <- sym("Elsewhere")
  missing <- sym("Missing")
  prop <- sym("Prop")
  looks <- sym("Looks")

  data_wide %>%
    mutate(Looks = UQ(primary) + UQ(others) + UQ(elsewhere) + UQ(missing),
           Prop = UQ(primary) / (UQ(others) + UQ(primary)),
           PropSE = se_prop(UQ(prop), UQ(others) + UQ(primary)),
           PropNA = UQ(missing) / UQ(looks))
}


check_resp_def <- function(resp_def, observed_resp_vals) {
  possible_resp_vals <- unlist(resp_def)
  missing <- setdiff(observed_resp_vals, possible_resp_vals)

  if (length(missing) != 0) {
    bad <- paste0(missing, collapse = ", ")
    stop("Response values not found in response definition: ",
         bad, call. = FALSE)
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
