context("binning")

test_that("assign_bins without grouping variables", {
  data <- data_frame(
    Subject = c("S01"),
    Condition = c("a"),
    TrialNo = 1,
    Time = 1:18)

  # Column name error
  expect_error(assign_bins(data, 3, Time, Subject, Condition,
                           TrialNo, bin_col = "Time"))

  # No effect of grouping when it doesn't matter
  b1 <- assign_bins(data, 3, Time)
  b2 <- assign_bins(data, 3, Time, Subject, Condition, TrialNo)
  expect_equal(b1, b2)

  # Accurate binning
  bins <- sort(rep(1:6, 3))
  expect_equal(b1$.bin, bins)

  # Sorting
  data2 <- data
  data2$Time <- rev(data2$Time)
  b3 <- assign_bins(data2, 3, Time)
  expect_equal(b1, b3)

  # Ragged binning
  expect_warning(assign_bins(data, 5, Time))

  with_nas1 <-
    suppressWarnings(assign_bins(data, 5, Time, na_location = "tail"))
  with_nas2 <-
    suppressWarnings(assign_bins(data, 5, Time, na_location = "head"))
  with_nas3 <-
    suppressWarnings(assign_bins(data, 5, Time, na_location = "split"))

  bins <- sort(rep(1:3, 5))
  expect_equal(c(bins, NA, NA, NA), with_nas1$.bin)
  expect_equal(c(NA, NA, NA, bins), with_nas2$.bin)
  expect_equal(c(NA, bins, NA, NA), with_nas3$.bin)

  # Partial binning
  part_bin <- assign_bins(data, 5, Time, partial = TRUE)
  expect_equal(c(bins, 4, 4, 4), part_bin$.bin)
})



test_that("assign_bins with grouping variables", {
  data <- tidyr::crossing(
    Subject = c("S01", "S02", "S03", "S04"),
    Condition = c("a", "b"),
    TrialNo = 1:10,
    Time = 1:18)

  # Grouping errors
  expect_error(assign_bins(data, 3, Time))
  expect_error(assign_bins(data, 3, Time, Subject))
  expect_error(assign_bins(data, 3, Time, Subject, Condition))

  # Column name error
  expect_error(assign_bins(data, 3, Time, Subject, Condition,
                           TrialNo, bin_col = "Time"))

  binned <- assign_bins(data, 3, Time, Subject, Condition,
                        TrialNo, bin_col = "Bin")

  bins <- rep(sort(rep(1:6, 3)), 80)
  expect_true(all(binned$Bin == bins))

  # Working with NAs
  with_nas <-
    suppressWarnings(assign_bins(data, 5, Time, Subject, Condition,
                                 TrialNo, bin_col = "Bin"))
  bins <- rep(sort(c(rep(1:3, 5), NA, NA, NA), na.last = TRUE), 80)
  expect_equal(bins, with_nas$Bin)
})
