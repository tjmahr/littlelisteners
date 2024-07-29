test_that("interpolation example works", {
  # We have time in ms, measured at 60 fps, and we want to fill in gaps of 100 ms.
  looks <- tibble::tribble(
    ~Subject, ~Trial, ~Time,    ~AOI,         ~Hint,
         "A",      1,  1000,  "Left",     "present",
         "A",      1,  1017,  "Left",     "present",
         "A",      1,  1034,      NA,   "legal gap",
         "A",      1,  1051,      NA,   "legal gap",
         "A",      1,  1068,      NA,   "legal gap",
         "A",      1,  1084,  "Left",     "present",
         "A",      1,  1100,      NA, "illegal gap",
         "A",      2,   983,  "Left",     "present",
         "A",      2,  1000, "Right",     "present",
         "A",      2,  1017,      NA, "illegal gap",
         "A",      2,  1034,      NA, "illegal gap",
         "A",      2,  1051,      NA, "illegal gap",
         "A",      2,  1068,      NA, "illegal gap",
         "A",      2,  1084,      NA, "illegal gap",
         "A",      2,  1100,      NA, "illegal gap",
         "A",      2,  1118,      NA, "illegal gap",
         "A",      2,  1135, "Right",     "present",
  )

  # Note that only the "legal gap" rows were interpolated
  filled <- looks |>
    dplyr::group_by(Trial) |>
    interpolate_looks(
      window = 100,
      fps = 60,
      response_col = "AOI",
      interp_col = "Interpolated",
      fillable = c("Left", "Right"),
      missing_looks = NA
    )

  expect_s3_class(filled, "grouped_df")
  expect_true(
    all((filled$Hint == "legal gap") == filled$Interpolated)
  )
})


test_that("interpolation works", {
  looks <- tibble::tribble(
    ~Subject, ~Trial, ~Time, ~AOI, ~Hint, ~Hint2,
    "A", 1, 1000, "Left", "filled", "Left",
    "A", 1, 2000, "Left", "filled", "Left",
    "A", 1, 3000, NA,   "fillable", "Left",
    "A", 1, 4000, "Left", "filled", "Left",
    "A", 1, 5000, NA,   "unfillable", NA,    # trial-final
    "A", 2, 1000, "Left", "filled", "Left",
    "A", 2, 2000, "Left", "filled", "Left",
    "A", 2, 3000, NA, "unfillable", NA,      # change directions
    "A", 2, 4000, "Right", "filled", "Right",
    "A", 2, 5000, "Right", "filled", "Right",
    "A", 3, 1000, NA, "unfillable", NA,      # trial-initial
    "A", 3, 2000, "Right", "filled", "Right",
    "A", 3, 3000, NA, "unfillable", NA,      # too wide for this test
    "A", 3, 4000, NA, "unfillable", NA,
    "A", 3, 5000, NA, "unfillable", NA,
    "A", 3, 6000, NA, "unfillable", NA,
    "A", 3, 7000, NA, "unfillable", NA,
    "A", 3, 8000, NA, "unfillable", NA,
    "A", 3, 9000, NA, "unfillable", NA,
    "A", 3, 1000, "Right", "filled", "Right",
    "A", 3, 1100, "Right", "filled", "Right",
    "A", 4, 1000, "Left", "filled",  "Left",
    "A", 4, 2000, NA, "fillable",  "Left",
    "A", 4, 3000, "offscreen", "fillable",  "Left",
    "A", 4, 4000, "Left", "filled",  "Left",
    "A", 4, 5000, "Left", "filled", "Left",
  )

  filled <- looks |>
    group_by(Trial) |>
    interpolate_looks(
      window = 3000,
      fps = 1,
      response_col = "AOI",
      interp_col = "Interpolated",
      fillable = c("Left", "Right"),
      missing_looks = c(NA, "offscreen")
  )

  expect_s3_class(filled, "grouped_df")
  expect_equal(filled$AOI, filled$Hint2)
})
