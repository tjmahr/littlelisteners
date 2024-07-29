test_that("Reading a simple DataWiz file", {
  # Parse this file so that there are no repeated header rows and the blank
  # headers are given negative time values:

  # Header1	Header2	Header3	 	 	 	 	 	F0	F33	F67
  # KidA	Day1	Data3	-	-	-	-	-	-	-	-
  # KidA	Day1	Data3	.	.	.	.	.	.	.	.
  # KidA	Day1	Data3	0	0	0	0	0	0	0	0
  # Header1	Header2	Header3	 	 	 	 	 	F0	F33	F67
  # KidA	Day2	Data3	1	1	1	1	1	1	1	1
  # KidA	Day2	Data3	.	-	0	1	.	-	0	1
  # KidA	Day2	Data3	1	1	1	0	0	.	.	.
  # Header1	Header2	Header3	 	 	 	 	 	F0	F33	F67
  # KidB	Day1	Data3	.	.	.	.	.	.	.	.
  # KidB	Day1	Data3	.	.	.	.	.	.	.	.
  # KidB	Day1	Data3	.	.	.	.	.	.	.	.

  df <- read_datawiz(testthat::test_path("data/data_wiz_mock.txt"))

  # Outputted dataframe does not have any blank column names
  expected_names <- c(
    "Header1", "Header2", "Header3", "X167", "X133", "X100",
    "X67", "X33", "F0", "F33", "F67"
  )
  expect_equal(names(df), expected_names)

  # The repeated column names don't show up in the data
  expect_true(all(df$Header1 %in% c("KidA", "KidB")))

  # Make sure that c(".", "-", "1", "0") are not coerced to numerics
  expect_type(df$X167, "character")
})


test_that("Reading a ragged DataWiz file", {
  # A "ragged" file has some rows with more columns than the header row
  df <- read_datawiz(testthat::test_path("data/data_wiz_mock_ragged.txt"))

  expected_names <- c(
    "Header1", "Header2", "Header3", "X167", "X133", "X100",
    "X67", "X33", "F0", "F33", "F67",
    # These column names do not appear in the header and have to be inferred
    "F100", "F133", "F167",
    "F200", "F233"
  )
  expect_equal(names(df), expected_names)

  # default readr behavior is to merge the ragged columns so the last named
  # column will have extra characters. Confirm that this does not happen.
  expect_true(all(nchar(df$F67) == 1))
})
