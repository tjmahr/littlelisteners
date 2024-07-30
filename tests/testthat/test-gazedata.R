

test_that("reading gazedata files", {
  gazedata_path <- example_files(1)[1]

  d <- gazedata_path |>
    read_gazedata()

  xs <- c(d$XLeft, d$XRight, d$XMean)
  ys <- c(d$YLeft, d$YRight, d$YMean)

  # gaze locations bounded between 0, 1
  c(xs, ys) |>
    min(na.rm = TRUE) |>
    expect_gte(0)

  c(xs, ys) |>
    max(na.rm = TRUE) |>
    expect_lte(1)

  zs <- c(d$ZLeft, d$ZRight, d$ZMean)
  ds <- c(d$DiameterLeft, d$DiameterRight, d$DiameterMean)

  # measures of length/distance are positive
  c(zs, ds) |>
    min(na.rm = TRUE) |>
    expect_gte(0)
})
