
test_that("Valid dataframe with no subtotals", {
  df <- data.frame(a = 1:3, b = 4:6)
  expect_identical(custom_sort(df), df)
})


test_that("Valid data frame with valid subtotals", {
  df <- data.frame(a = factor(c("Total", "B", "A"), levels = c("A", "B", "Total")), b = 1:3)
  sub_totals <- list(a = c("A"))
  sorted_df <- custom_sort(df, sub_totals)
  expect_equal(as.character(sorted_df$a), c("B", "A", "Total"))
})

test_that("Empty datafrrame", {
  df <- data.frame()
  expect_identical(custom_sort(df), df)
})


test_that("Non-Data Frame Input", {
  df <- list(a = 1:3, b = 4:6)
  expect_error(custom_sort(df))
})


test_that("Subtotals Not in Dataframe", {
  df <- data.frame(a = 1:3, b = 4:6)
  sub_totals <- list(c = c("A"))
  expect_error(custom_sort(df, sub_totals))
})

test_that("Data Frame with Special Characters or Non-Standard Text", {
  df <- data.frame(a = factor(c("Total", "*&^", "123"), levels = c("*&^", "123", "Total")), b = 1:3)
  sorted_df <- custom_sort(df)
  expect_equal(as.character(sorted_df$a), c("*&^", "123", "Total"))
})




