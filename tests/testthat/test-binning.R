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


test_that("trim_to_bin_width defaults to earliest time", {
  data1 <- data_frame(
    task = "testing",
    id = "test1",
    time = -12:13,
    frame = seq_along(time))

  data2 <- data_frame(
    task = "testing",
    id = "test2",
    time = -10:13,
    frame = seq_along(time))

  trimmed1 <- data1 %>%
    trim_to_bin_width(bin_width = 3, time_var = time)
  trimmed2 <- data2 %>%
    trim_to_bin_width(bin_width = 3, time_var = time)

  expect_equal(nrow(trimmed1) %% 3, 0)
  expect_equal(nrow(trimmed2) %% 3, 0)

  trimmed1b <- data1 %>%
    trim_to_bin_width(bin_width = 4, time_var = time)
  trimmed2b <- data2 %>%
    trim_to_bin_width(bin_width = 5, time_var = time)

  expect_equal(nrow(trimmed1b) %% 4, 0)
  expect_equal(nrow(trimmed2b) %% 5, 0)

  expect_error({
    bind_rows(data1, data2) %>%
      trim_to_bin_width(bin_width = 3, time_var = time)
  })

  # grouped data
  trimmed3 <- bind_rows(data1, data2) %>%
    trim_to_bin_width(bin_width = 3, time_var = time, id)

  expect_equal(nrow(trimmed3) %% 3, 0)

  trimmed4 <- bind_rows(data1, data2) %>%
    trim_to_bin_width(bin_width = 3, time_var = time, task, id)

  expect_equal(nrow(trimmed4) %% 3, 0)
})




test_that("trim_to_bin_width handles key times", {
  fast_data <- function(data, bin_width, key_time, key_position = 1) {
    data %>%
      trim_to_bin_width(bin_width, time,
                        key_time = key_time, key_position = key_position) %>%
      assign_bins(bin_width, time) %>%
      group_by(.bin) %>%
      mutate(frame_in_bin = seq_along(frame)) %>%
      ungroup()
  }


  data1 <- data_frame(
    task = "testing",
    id = "test1",
    time = -12:13,
    frame = seq_along(time))

  data2 <- data_frame(
    task = "testing",
    id = "test2",
    time = -10:13,
    frame = seq_along(time))

  trimmed1 <- fast_data(data1, 3, 0, 1)
  trimmed2 <- fast_data(data2, 3, 0, 1)

  expect_equal(nrow(trimmed1) %% 3, 0)
  expect_equal(nrow(trimmed2) %% 3, 0)

  bind_rows(trimmed1, trimmed2) %>%
    filter(time == 0) %>%
    pull(frame_in_bin) %>%
    expect_equal(c(1, 1))

  trimmed1b <- fast_data(data1, 4, 0, 1)
  trimmed2b <- fast_data(data2, 5, 0, 1)

  bind_rows(trimmed1b, trimmed2b) %>%
    filter(time == 0) %>%
    pull(frame_in_bin) %>%
    expect_equal(c(1, 1))

  trimmed1b <- fast_data(data1, 4, 0, 2)
  trimmed2b <- fast_data(data2, 5, 0, 2)

  bind_rows(trimmed1b, trimmed2b) %>%
    filter(time == 0) %>%
    pull(frame_in_bin) %>%
    expect_equal(c(2, 2))

  # Selecting final frame
  expect_warning(fast_data(data1, 4, 0, 8))
  trimmed1b <- suppressWarnings(fast_data(data1, 4, 0, 8))
  trimmed2b <- fast_data(data2, 4, 0, 4)

  bind_rows(trimmed1b, trimmed2b) %>%
    filter(time == 0) %>%
    pull(frame_in_bin) %>%
    expect_equal(c(4, 4))

  expect_warning(fast_data(data1, 4, 0, 7))
  trimmed1b <- suppressWarnings(fast_data(data1, 4, 0, 7))
  trimmed2b <- fast_data(data2, 4, 0, 3)

  bind_rows(trimmed1b, trimmed2b) %>%
    filter(time == 0) %>%
    pull(frame_in_bin) %>%
    expect_equal(c(3, 3))

  # grouped data
  trimmed3 <- bind_rows(data1, data2) %>%
    trim_to_bin_width(bin_width = 3, time_var = time, id,
                      key_time = 0, key_position = 2)

  trimmed3 %>%
    assign_bins(bin_width = 3, time, id) %>%
    group_by(id, .bin) %>%
    mutate(frame_in_bin = seq_along(frame)) %>%
    ungroup() %>%
    filter(time == 0) %>%
    pull(frame_in_bin) %>%
    expect_equal(c(2, 2))
})



test_that("trim_to_bin_width handles min and max times", {
  data1 <- data_frame(
    task = "testing",
    id = "test1",
    time = -12:13,
    frame = seq_along(time))

  data2 <- data_frame(
    task = "testing",
    id = "test2",
    time = -10:13,
    frame = seq_along(time))

  trimmed1 <- data1 %>%
    trim_to_bin_width(3, time_var = time, min_time = -2)

  expect_equal(nrow(trimmed1) %% 3, 0)
  expect_equal(min(trimmed1$time), -2)

  trimmed2 <- data2 %>%
    trim_to_bin_width(3, time_var = time, max_time = 2)

  expect_equal(nrow(trimmed2) %% 3, 0)
  # max time of trimmed values is within a bin of max time give
  expect_lte(max(trimmed2$time), 2 + 2)

  trimmed1 <- data1 %>%
    trim_to_bin_width(3, time, min_time = -2, max_time = 2)

  expect_equal(nrow(trimmed1) %% 3, 0)
  # max time of trimmed values is within a bin of max time give
  expect_lte(max(trimmed1$time), 2 + 2)

  both <- bind_rows(data1, data2) %>%
    trim_to_bin_width(3, time, id, min_time = -11, max_time = 2)

  both %>%
    group_by(id) %>%
    summarise(time = min(time)) %>%
    pull(time) %>%
    expect_equal(c(-11, -10))

  max_times <- both %>%
    group_by(id) %>%
    summarise(time = max(time)) %>%
    pull(time)

  expect_lte(max_times[1], 2 + 2)
  expect_lte(max_times[2], 2 + 2)

  # Force a key time
  both <- bind_rows(data1, data2) %>%
    trim_to_bin_width(3, time, id, min_time = -11, max_time = 2,
                      key_time = 0, key_position = 2)

  both %>%
    assign_bins(bin_width = 3, time, id) %>%
    group_by(id, .bin) %>%
    mutate(frame_in_bin = seq_along(frame)) %>%
    ungroup() %>%
    filter(time == 0) %>%
    pull(frame_in_bin) %>%
    expect_equal(c(2, 2))

  both %>%
    group_by(id) %>%
    summarise(time = min(time)) %>%
    pull(time) %>%
    expect_equal(c(-10, -10))


  max_times <- both %>%
    group_by(id) %>%
    summarise(time = max(time)) %>%
    pull(time)

  expect_lte(max_times[1], 2 + 2)
  expect_lte(max_times[2], 2 + 2)

  # Force a key time
  both <- bind_rows(data1, data2) %>%
    trim_to_bin_width(3, time, id, min_time = -11, max_time = 2,
                      key_time = 0, key_position = 1)

  both %>%
    group_by(id) %>%
    summarise(time = min(time)) %>%
    pull(time) %>%
    expect_equal(c(-9, -9))


  max_times <- both %>%
    group_by(id) %>%
    summarise(time = max(time)) %>%
    pull(time)

  expect_lte(max_times[1], 2 + 2)
  expect_lte(max_times[2], 2 + 2)

  both <- bind_rows(data1, data2) %>%
    trim_to_bin_width(3, time, id, min_time = -11, max_time = 2,
                      key_time = 0, key_position = 3)

  both %>%
    group_by(id) %>%
    summarise(time = min(time)) %>%
    pull(time) %>%
    expect_equal(c(-11, -8))

  max_times <- both %>%
    group_by(id) %>%
    summarise(time = max(time)) %>%
    pull(time)

  expect_lte(max_times[1], 2 + 2)
  expect_lte(max_times[2], 2 + 2)

})
