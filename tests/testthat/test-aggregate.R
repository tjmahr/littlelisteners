context("aggregate")

test_that("aggregate_looks works like lookr::AggregateLooks", {
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
    Time = 1:4)

  data$GazeByImageAOI <- sample(
    x = unlist(lookr_def),
    size = nrow(data),
    replace = TRUE,
    prob = c(.6, .2, .05, .15))

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

test_that("aggregate_looks2", {
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
    Time = 1:4)

  data$GazeByImageAOI <- sample(
    x = unlist(four_img_def),
    size = nrow(data),
    replace = TRUE,
    prob = c(.4, .1, .1, 1, .05, .15))

  nse_results <- aggregate_looks(data, four_img_def,
                                 Subject ~ GazeByImageAOI)

  se_results <- aggregate_looks2(data, four_img_def, GazeByImageAOI, Subject)

  testthat::expect_equal(nse_results, se_results)

  nse_results <- aggregate_looks(data, four_img_def,
                                 Subject + Time ~ GazeByImageAOI)
  se_results <- aggregate_looks2(data, four_img_def, GazeByImageAOI,
                                 Subject, Time)

  testthat::expect_equal(nse_results, se_results)
})
