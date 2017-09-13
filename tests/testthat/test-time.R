context("time")

test_that("adjusting and aligning times", {
  trial1 <- data.frame(trial = 1, time_ms = 1:5, event = 2)
  trial2 <- data.frame(trial = 2, time_ms = 6:10, event = 8.5)
  trial_times <- dplyr::bind_rows(trial1, trial2)
  trial_times$raw_time_ms <- trial_times$time_ms

  adj1 <- adjust_times(trial_times, time_ms, event, trial,
                       align = FALSE, fps = 1000)

  adj2 <- adjust_times(trial_times, time_ms, event, trial, fps = 1000)
  adj3 <- adjust_times(trial_times, time_ms, event, trial, fps = 1000,
                       ties = "last")

  expect_equal(adj1$time_ms, trial_times$time_ms - trial_times$event)
  expect_equal(adj2$time_ms, ceiling(trial_times$time_ms - trial_times$event))
  expect_equal(adj3$time_ms, floor(trial_times$time_ms - trial_times$event))

  expect_error(adjust_times(trial_times, time_ms, event, fps = 1000))
})
