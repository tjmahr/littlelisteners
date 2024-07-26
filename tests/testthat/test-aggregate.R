
test_that("aggregate_looks() computes looking proprtions", {
  two_img <- create_response_def(
    primary = c("Target"),
    others = c("Distractor"),
    elsewhere = c("tracked"),
    missing = c(NA)
  )

  data <- tidyr::crossing(
    Subject = c("S01", "S02", "S03"),
    Condition = c("a", "b"),
    TrialNo = 1:4,
    Time = 1:4
  )

  # Each line here is a subject:condition where
  # rep(c(look, at, each, frame), 4 trials)
  l <- c(
    rep(c(0, 0, 0, 1), 4),
    rep(c(0, 0, 1, 1), 4),
    rep(c(0, 1, 1, 1), 4),
    rep(c(1, 1, 1, 1), 4),
    rep(c(NA,  0, 0, 1), 4),
    rep(c(NA, NA, 0, 1), 4)
  )
  data$GazeByImageAOI <- ifelse(l, "Target", "Distractor")

  by_subject <- aggregate_looks(data, two_img, Subject ~ GazeByImageAOI)
  expect_equal(by_subject$Prop,   c(3/8, 7/8, 2/5))
  expect_equal(by_subject$PropNA, c(  0,   0, 3/8))

  by_subject_by_condition <- aggregate_looks(
    data,
    two_img,
    Subject + Condition ~ GazeByImageAOI
  )
  expect_equal(by_subject_by_condition$Prop,   c(1/4, 2/4, 3/4, 4/4, 1/3, 1/2))
  expect_equal(by_subject_by_condition$PropNA, c(  0,   0,   0,   0, 1/4, 2/4))

  by_time <- aggregate_looks(
    data,
    two_img,
    Time ~ GazeByImageAOI
  )
  expect_equal(by_time$Prop,   c(1/4, 2/5, 3/6, 6/6))
  expect_equal(by_time$PropNA, c(2/6, 1/6,   0,   0))

  flat <- aggregate_looks(data, two_img, 1 ~ GazeByImageAOI)
  expect_equal(flat$Prop, sum(l %in% 1) / sum(l %in% c(0, 1)))
  expect_equal(flat$PropNA, mean(is.na(l)))

})

test_that("aggregate_looks2() works the same as aggregate_looks()", {
  four_img_def <- create_response_def(
    primary = c("Target"),
    others = c("Distractor1", "Distractor2", "Distractor3"),
    elsewhere = c("tracked"),
    missing = c(NA)
  )

  data <- tidyr::crossing(
    Subject = c("S01", "S02", "S03", "S04"),
    Condition = c("a", "b"),
    TrialNo = 1:10,
    Time = 1:4
  )

  data$GazeByImageAOI <- sample(
    x = unlist(four_img_def[-1]),
    size = nrow(data),
    replace = TRUE,
    prob = c(.4, .1, .1, 1, .05, .15)
  )

  expect_equal(
    aggregate_looks(data, four_img_def, Subject ~ GazeByImageAOI),
    aggregate_looks2(data, four_img_def, GazeByImageAOI, Subject)
  )

  expect_equal(
    aggregate_looks(data, four_img_def, Subject + Time ~ GazeByImageAOI),
    aggregate_looks2(data, four_img_def, GazeByImageAOI, Subject, Time)
  )
})


test_that("aggregate_looks works like lookr::AggregateLooks", {
  testthat::skip_if_not_installed("lookr")
  lookr_def <- create_response_def(
    primary = c("Target"),
    others = c("Distractor"),
    elsewhere = c("tracked"),
    missing = c(NA)
  )

  data <- tidyr::crossing(
    Subject = c("S01", "S02", "S03", "S04"),
    Condition = c("a", "b"),
    TrialNo = 1:10,
    Time = 1:4
  )

  data$GazeByImageAOI <- sample(
    x = unlist(lookr_def[-1]),
    size = nrow(data),
    replace = TRUE,
    prob = c(.6, .2, .05, .15)
  )

  lookr_results <- lookr::AggregateLooks(data, Subject ~ GazeByImageAOI)
  our_results <- aggregate_looks(data, lookr_def, Subject ~ GazeByImageAOI)

  testthat::expect_equal(lookr_results$Target, our_results$Target)
  testthat::expect_equal(lookr_results$Distractor, our_results$Distractor)
  testthat::expect_equal(lookr_results$NAs, our_results$Missing)
  testthat::expect_equal(lookr_results$Looks, our_results$Looks)
  testthat::expect_equal(lookr_results$Elsewhere, our_results$Elsewhere)
  testthat::expect_equal(lookr_results$Proportion, our_results$Prop)
  testthat::expect_equal(lookr_results$PropNA, our_results$PropNA)

  lookr_results <- lookr::AggregateLooks(data, Subject + Condition + Time ~ GazeByImageAOI)
  our_results <- aggregate_looks(data, lookr_def, Subject + Condition + Time ~ GazeByImageAOI)

  testthat::expect_equal(lookr_results$Target, our_results$Target)
  testthat::expect_equal(lookr_results$Distractor, our_results$Distractor)
  testthat::expect_equal(lookr_results$NAs, our_results$Missing)
  testthat::expect_equal(lookr_results$Looks, our_results$Looks)
  testthat::expect_equal(lookr_results$Elsewhere, our_results$Elsewhere)
  testthat::expect_equal(lookr_results$Proportion, our_results$Prop)
  testthat::expect_equal(lookr_results$PropNA, our_results$PropNA)

})


