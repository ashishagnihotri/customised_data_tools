sample_df <- data.frame(var1 = 1:10, var2 = 11:20, var3 = 21:30, var4 = 31:40)

test_that("summarize specific pair of variables", {
  expect_silent(check_variable(sample_df, variables_list = list(c("var1", "var2")), output_path = "output_specific.xlsx"))
})

test_that("summarize multiple pairs of variables", {
  expect_silent(check_variable(sample_df, variables_list = list(c("var1", "var2"), c("var3", "var4")), output_path = "output_multiple.xlsx"))
})

test_that("summarize all variable pairs by default", {
  expect_silent(check_variable(sample_df, output_path = "output_all.xlsx"))
})

test_that("rounding works correctly", {
  expect_silent(check_variable(sample_df, variables_list = list(c("var1", "var2")), round = TRUE, output_path = "output_rounded.xlsx"))
})

test_that("handle non-existent variables", {
  expect_error(check_variable(sample_df, variables_list = list(c("nonexistent", "var2")), output_path = "output_nonexistent.xlsx"))
})

test_that("handle single column dataframe", {
  single_col_df <- data.frame(var1 = 1:10)
  expect_error(check_variable(single_col_df, output_path = "output_single_col.xlsx"))
})

test_that("validate variables_list format", {
  expect_error(check_variable(sample_df, variables_list = "invalid_input", output_path = "output_invalid.xlsx"))
})

test_that("handle different data types correctly", {
  mixed_type_df <- data.frame(num = 1:10, char = letters[1:10])
  expect_silent(check_variable(mixed_type_df, variables_list = list(c("num", "char")), output_path = "output_mixed_types.xlsx"))
})

test_that("create and save output file correctly", {
  expect_silent(check_variable(sample_df, variables_list = list(c("var1", "var2")), output_path = "output_file.xlsx"))
})

