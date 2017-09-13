#' Create a response definition
#'
#' A response definition controls how `aggregate_looks()` works.
#'
#' @param primary the primary response of interest
#' @param others other responses of interest
#' @param elsewhere responses to ignore
#' @param missing responses that indicate missing data. Defaults to `NA`.
#' @param label optional label for the response definition. Defaults to the
#'   value of `primary`.
#' @return a `response_def` object
#' @export
#' @rdname response-definition
#'
#' @details
#' To deal with eyetracking data in a generic way, we need a way to describe
#' eyetracking responses. We assume that there are four basic gaze types.
#'
#' * Primary responses: A gaze to a primary or target image.
#' * Other responses: Gazes to competing images.
#' * Elsewhere looks: A gaze that is onscreen but not a primary or
#'   other response. Typically, this occurs when the participant is
#'   shifting between images.
#' * Missing looks: A missing or offscreen gaze.
#'
#' A _response definition_ is a programmatic way of describing these response
#' types, and it allows `aggregate_looks()` to map gaze data into looking
#' counts.
#'
#' @examples
#' create_response_def(
#'   label = "looks to target",
#'   primary = "Target",
#'   others = c("PhonologicalFoil", "SemanticFoil", "Unrelated"),
#'   elsewhere = "tracked",
#'   missing = NA)
create_response_def <- function(primary, others, elsewhere = NULL,
                                missing = NA, label = NULL) {
  structure(list(
    response_def = label %||% paste0(primary, collapse = "-"),
    primary = primary,
    others = others,
    elsewhere = elsewhere,
    missing = missing
  ), class = "response_def")
}

#' @export
print.response_def <- function(object, ...) {
  str(object, ...)
}



#' Aggregate looks
#'
#' Aggregate the number of looks to each response type over some grouping
#' variables like Subject, Time, Condition.
#'
#' @param data a long data frame of looking data
#' @param resp_def a response definition or a list of response definition.
#' @param formula an aggregation formula. The lefthand terms will be grouping
#'   variables, and the righthand term is the column with eyetracking responses.
#' @return a dataframe of the grouping columns along with the number of looks to
#'   each response type, the proportion (and standard error) of looks to the
#'   primary response, and the proportion (and standared error) of missing data.
#' @export
#' @rdname aggregating-looks
#'
#' @details
#' This function is the main tool for preparing eyetracking data for a growth
#' curve analysis. For example, an aggregation formula like `Subject + Time ~
#' Gaze` would provide the number of looks to each image over time for each
#' subject.
#'
#' `aggregate_looks()` uses an aggregation formula like
#' [stats::aggregate()], whereas `aggregate_looks2()` uses column names.
#'
#' @examples
#' target_def <- create_response_def(
#'   label = "looks to target",
#'   primary = "Target",
#'   others = c("PhonologicalFoil", "SemanticFoil", "Unrelated"),
#'   elsewhere = "tracked",
#'   missing = NA)
#'
#' four_image_data %>%
#'   aggregate_looks(target_def, Subject + TrialNo ~ GazeByImageAOI)
#'
#' four_image_data %>%
#'   aggregate_looks(target_def, Subject ~ GazeByImageAOI) %>%
#'   str()
#'
#' # With column names
#' four_image_data %>%
#'   aggregate_looks2(target_def, GazeByImageAOI, Subject, TrialNo)
#'
#' four_image_data %>%
#'   aggregate_looks2(target_def, GazeByImageAOI, Subject) %>%
#'   str()
#'
#' phonological_def <- create_response_def(
#'   label = "looks to phonological foil",
#'   primary = "PhonologicalFoil",
#'   others = c("Target", "SemanticFoil", "Unrelated"),
#'   elsewhere = "tracked",
#'   missing = NA)
#'
#' # Aggregate looks to multiple response definitions at once
#' defs <- list(target_def, phonological_def)
#' four_image_data %>%
#'   aggregate_looks(defs, Subject + BlockNo ~ GazeByImageAOI) %>%
#'   dplyr::select(.response_def, Subject, BlockNo, Primary:PropNA) %>%
#'   dplyr::mutate_at(c("Prop", "PropSE", "PropNA"), round, 3)
#'
#' # Compute a growth curve
#' growth_curve <- four_image_data %>%
#'   adjust_times(Time, TargetOnset, Subject, BlockNo, TrialNo) %>%
#'   aggregate_looks(target_def, Time ~ GazeByImageAOI) %>%
#'   filter(-1000 <= Time, Time <= 2000)
#'
#' library(ggplot2)
#' ggplot(growth_curve) +
#'   aes(x = Time, y = Prop) +
#'   geom_hline(size = 2, color = "white", yintercept = .25) +
#'   geom_vline(size = 2, color = "white", xintercept = 0) +
#'   geom_pointrange(aes(ymin = Prop - PropSE, ymax = Prop + PropSE)) +
#'   labs(y = "Proportion of looks to target",
#'        x = "Time relative to target onset [ms]") +
#'   theme_grey(base_size = 14)
aggregate_looks <- function(data, resp_def, formula) {
  resp_var <- quo(!! formula[[3]])
  grouping <- quos(!!! syms(all.vars(formula[[2]])))
  aggregate_looks2(data, resp_def, !! resp_var, !!! grouping)
}

#' @param resp_var Name of the column that contains eyetracking responses
#' @param ... Grouping columns.
#' @export
#' @rdname aggregating-looks
aggregate_looks2 <- function(data, resp_def, resp_var, ...) {
  grouping <- quos(...)
  resp_var <- enquo(resp_var)

  # Tuck a single definition in a list
  if (class(resp_def) == "response_def") {
    resp_def <- list(resp_def)
  }

  stopifnot(vapply(resp_def, class, "class") == "response_def")

  # Process all definitions in the list
  x <- list_along(resp_def)
  for (def in seq_along(resp_def)) {
    x[[def]] <- .aggregate_looks2(data, resp_def[[def]],
                                  !! resp_var, !!! grouping)
  }

  bind_rows(x)
}

# workhorse function for a single aggregation
.aggregate_looks2 <- function(data, resp_def, resp_var, ...) {
  grouping <- quos(...)
  grouping_data <- data %>% distinct(!!! grouping)
  resp_var <- enquo(resp_var)

  # Check that all response types have a coding
  resp_vals <- unique(pull(data, !! resp_var))
  check_resp_def(resp_def, resp_vals)

  # Check that a grouping column name doesn't show up as a gaze column name
  reserved_name <- c(with_na_label(resp_vals), "Others", "Elsewhere",
                     "Prop", "PropSE", "PropMissing", ".response_def")

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
    mutate(
      .response_def = resp_def$response_def,
      Looks = UQ(primary) + UQ(others) + UQ(elsewhere) + UQ(missing),
      Prop = UQ(primary) / (UQ(others) + UQ(primary)),
      PropSE = se_prop(UQ(prop), UQ(others) + UQ(primary)),
      PropNA = UQ(missing) / UQ(looks)) %>%
    select(.response_def, everything())
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
