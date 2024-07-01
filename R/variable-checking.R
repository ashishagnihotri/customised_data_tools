################################################################################
# Description: Grouping dataframe by variable pairs
# Author: Ashish Agnihotri
#
# Notes:
# - Needs a dataframe as an input, do not provide a file path
# - Provide a file path
#
# Issues:
#
#
################################################################################

#' Produce pair-wise counts of columns and output to Excel workbook.
#'
#'
#' It allows for specifying variable pairs needed and rounding the count to the nearest 100.
#' The data is saved in an Excel sheet with each pair of variables in a separate sheet.
#' If variable pair is not mentioned by the user, the function will generate a pair of each variable
#' present in the dataframe.
#'
#' @param df The data frame to be summarized.
#' @param variables A list of pairs of variable names to be summarized.
#'   If NULL, all pairs of variables in the dataframe are used.
#' @param round Logical value indicating whether to round the total counts.
#'   If TRUE, the total counts are rounded to the nearest hundred.
#' @param outputPath File path where the output Excel workbook will be saved.
#'
#' @return There are no return objects, a workbook in the specified file path is created
#' @export
#' @importFrom openxlsx setColWidths

checkVariable <- function(df, variables = NULL, round = FALSE, outputPath) {

  wb <- createWorkbook()

  # if variablesList is NULL, create a list of all pairs of variables
  if (is.null(variables)) {
    variableNames <- names(df)
    variables <- utils::combn(variableNames, 2, simplify = FALSE)
  }

  
  if (is.null(outputPath)){
    stop("Output file path has not been provided")
  }

  if (ncol(df) < 2) {
    stop("Dataframe must have at least 2 columns")
  }

  for (vars in variables) {

    # applying group by on variable pairs generated, or given
    dfSummary <- dplyr::summarise(
      dplyr::group_by(df, !!rlang::sym(vars[1]), !!rlang::sym(vars[2])),
      Total_Count = dplyr::n(),
      .groups = 'drop'
    )


    # rounding data to the nearest 100
    if (round) {
      dfSummary$Total_Count <- round(dfSummary$Total_Count / 100) * 100
    }

    # creating new worksheet, adding data and saving it
    sheetName <- paste(vars[1], vars[2], sep = "_")
    addWorksheet(wb, sheetName)
    writeData(wb, sheetName, dfSummary)
    setColWidths(wb, sheetName, cols = 1:3, widths = "auto")
  }


  # Create the filename by incorporating the dataframe name
  filename <- paste0("checkVariable.xlsx")

  # create final file path - augment output file path given by user with generated filename
  fullFilePath <- file.path(outputPath, filename)

  saveWorkbook(wb, file = fullFilePath, overwrite = TRUE)
}


