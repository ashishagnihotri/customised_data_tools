
#mocking of input dataframe
create_mock_df <- function(cols, rows = 1) {
  as.data.frame(matrix(ncol = length(cols), nrow = rows, dimnames = list(NULL, cols)))
}

# mocking of MasterConcordance fiel
mock_master_concordance <- create_mock_df(c("A", "B"), rows = 5)


test_that("Invalid input_df", {
  input_df <- "not_a_dataframe"
  expect_error(custom_concordance(input_df, "A", "B",
                                  concordance_file_path = mock_master_concordance,
                                  output_file_path = tempfile(), join_area = "A"))
})

test_that("Missing MasterConcordance", {
  input_df <- create_mock_df(c("A", "B"))
  expect_error(custom_concordance(input_df, "A", "B",
                                  concordance_file_path = NULL,
                                  output_file_path = tempfile(), join_area = "A"))
})

test_that("Empty or Invalid MasterConcordance", {
  input_df <- create_mock_df(c("A", "B"))
  empty_master_concordance <- create_mock_df(c("A", "B"), rows = 0)
  expect_error(custom_concordance(input_df, "A", "B",
                                  concordance_file_path = empty_master_concordance,
                                  output_file_path = tempfile(), join_area = "A"))
})

test_that("Non-existing start_area or required_area", {
  input_df <- create_mock_df(c("A", "B"))
  start_area <- "NonExistingArea"
  required_area <- "B"
  expect_error(custom_concordance(input_df, start_area, required_area,
                                  concordance_file_path = mock_master_concordance,
                                  output_file_path = tempfile(), join_area = "A"))
})

test_that("Invalid output_file_path", {
  input_df <- create_mock_df(c("A", "B"))
  invalid_output_file_path <- "/invalid/path/output.csv"
  expect_error(custom_concordance(input_df, "A", "B",
                                  concordance_file_path = mock_master_concordance,
                                  output_file_path = invalid_output_file_path,
                                  join_area = "A"))
})

test_that("Invalid join_area", {
  input_df <- create_mock_df(c("A", "B"))
  invalid_join_area <- "NonExistingJoinArea"
  expect_error(custom_concordance(input_df, "A", "B",
                                  concordance_file_path = mock_master_concordance,
                                  output_file_path = tempfile(),
                                  join_area = invalid_join_area))
})
