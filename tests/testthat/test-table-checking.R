
# mock input dataframe
create_mock_df <- data.frame(
  var1 = factor(c("A", "B", "A", "B")),
  var2 = factor(c("C", "D", "C", "D")),
  var3 = rnorm(4),
  var4 = rnorm(4)
)
exec_summary_vars <- c("var3", "var4")
detail_summary_vars <- c("var1", "var2")

test_that("executive summary is generated correctly", {
  output_path <- tempfile(fileext = ".xlsx")
  summarize_table(create_mock_df, exec_summary_vars, detail_summary_vars, output_path)
  expect_true(file.exists(output_path))

  # Read the summary sheet and validate its contents
  wb <- openxlsx::read.xlsx(output_path, sheet = "Summary")
  expect_true(nrow(wb) == length(exec_summary_vars))
  expect_true(all(c("Variable_Name", "Max", "Min", "Round_Robin_3", "Grad_Rand_Rounding") %in% colnames(wb)))
})

test_that("detailed summary for each variable is correct", {
  output_path <- tempfile(fileext = ".xlsx")
  summarize_table(create_mock_df, exec_summary_vars, detail_summary_vars, output_path)

  # Read each detail worksheet and validate contents
  for (var in detail_summary_vars) {
    sheet_data <- openxlsx::read.xlsx(output_path, sheet = var)
    expect_true(nrow(sheet_data) > 0)
    expect_true(all(names(create_mock_df) %in% colnames(sheet_data)))
  }
})


test_that("rr3_check function works correctly", {
  expect_true(rr3_check(c(3, 6, 9)))
  expect_false(rr3_check(c(3, 5, 9)))
})


test_that("grr_check function works correctly", {
  expect_true(grr_check(c(15, 20, 25)))
  expect_false(grr_check(c(15, 22, 25)))
})

test_that("handle empty dataframe gracefully", {
  empty_df <- data.frame()
  output_path <- tempfile(fileext = ".xlsx")
  expect_error(summarize_table(empty_df, exec_summary_vars, detail_summary_vars, output_path = output_path))
})


test_that("output file is created and saved correctly", {
  output_path <- tempfile(fileext = ".xlsx")
  summarize_table(create_mock_df, exec_summary_vars, detail_summary_vars, output_path = output_path)
  expect_true(file.exists(output_path))
})

test_that("column order is preserved in summarized data", {
  output_path <- tempfile(fileext = ".xlsx")
  summarize_table(create_mock_df, exec_summary_vars, detail_summary_vars, output_path)

    for (var in detail_summary_vars) {
    sheet_data <- openxlsx::read.xlsx(output_path, sheet = var)
    expect_equal(names(sheet_data), names(create_mock_df))
  }
})


test_that("handle invalid inputs appropriately", {
  expect_error(summarize_table(create_mock_df, c("nonexistent_var"), detail_summary_vars, output_path = tempfile(fileext = ".xlsx")))
  expect_error(summarize_table(create_mock_df, exec_summary_vars, c("nonexistent_var"), output_path = tempfile(fileext = ".xlsx")))
})

