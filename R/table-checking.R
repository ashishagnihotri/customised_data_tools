################################################################################
# Description: Summarize Dataframe with Custom Grouping
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

# Helper function
#' Checking RandomRounding3 is applied to variable or not
#' Checks if all variables in a numeric vector are multiples of 3.
#'
#'
#' @param x column variable that needs checking
#'
#' @return Boolean indicating whether all values in `x` are multiples of 3.
#' @export
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom purrr walk
rr3Check <- function(x) {
  all(x %% 3 == 0)
}



#Helper function
#'Checking Graduated Random Rounding is applied to the variable or not
#' Performs a graduated random rounding check on column variable.
#' Rounds numbers based on defined ranges and checks for multiples.
#' @param x A numeric vector.
#'
#' @return Logical value indicating if the numbers in `x` follow
#'   the graduated random rounding rules.
#' @export
#'
grrCheck <- function(x) {
  if (all(x < 20)) {
    return(all(x %% 3 == 0))
  } else if (all(x < 100)) {
    return(all(x %% 5 == 0))
  } else {
    return(all(x %% 10 == 0))
  }
}

#Helper function
#' Summarise and Filter Data
#'
#' Applies a cumulative filtering process on a data frame and then summarizes the data.
#' Specifically filters rows with a "Total" value in specified variables.
#'
#' @param inputDf Data frame to be processed.
#' @param groupVar The variable to group by.
#' @param columnOrder Order of columns to maintain in the output.
#' @param detailSummaryVars Variables used for cumulative filtering.
#'
#' @return A summarized data frame after applying the filtering process.
#' @export
#'
summariseAndFilter <- function(inputDf, groupVar, columnOrder, detailSummaryVars) {

  # iterate over categories for cumulative filtering
  filteredData <- Reduce(function(.data, .y) {
    if (any(grepl("Total", .data[[.y]]))) {
      output <- dplyr::filter(.data, grepl("Total", .data[[.y]]))
      return(output)
    } else {
      return(.data)
    }
  }, detailSummaryVars[!detailSummaryVars %in% groupVar], init = inputDf)

    # Group and summarize data, keeping all columns
    summarisedData <- dplyr::reframe(
      dplyr::group_by(filteredData, !!rlang::sym(groupVar)),
      dplyr::across(dplyr::everything(), ~ .),
      .groups = "drop"
    )


  # Reorder columns to match original dataframe order
  summarisedData <- summarisedData[, columnOrder, drop = FALSE]


  summarisedData <- customSort(summarisedData)

  return(summarisedData)
}

#Main function
#' Summarise Dataframe with Custom Grouping
#'
#' Creates a workbook with multiple summaries of a given data.frame, split
#' across multiple sheets. A variety of statistical summaries are provided
#' for user-specified numeric variables, and the data is filtered to provide
#' total counts for each user-specified categorical variable.
#'
#'
#' @param df The data frame to be summarized.
#' @param numericVars Numerical variables to be used in the executive summary.
#' @param categoricalVars Variables to be summarized in detail.
#' @param outputPath File path to save the output workbook. Just mention the folder,
#' the function will assign "checkTable-{df}.xlsx" as the file name to the workbook generated.
#'
#' @return None. The function saves the output workbook to the specified file path.
#' @export
#'
checkTable <- function(df, numericVars, categoricalVars, outputPath) {
  if (nrow(df) == 0 || !all(c(numericVars, categoricalVars) %in% names(df))) {
    stop("Invalid input: dataframe is empty or variables are not found")
  }

  
  if (is.null(outputPath)){
    stop("Output file path has not been provided")
  }

  # take df name assigned by user
  dfName <- deparse(substitute(df))

  # create filename by taking df name assigned by user
  filename <- paste0("checkTable-", dfName, ".xlsx")

  # create final file path - augment output file path given by user with generated filename
  fullFilePath <- file.path(outputPath, filename)

  #create new workbook
  wb <- createWorkbook()


  #prepare the executive summary data
  numericVars <- lapply(numericVars, function(var) {
    summaryRow <- dplyr::summarise(df,
                             Variable_Name = var,
                             Max = if(is.factor(df[[var]])) NA else max(df[[var]], na.rm = TRUE),
                             Min = if(is.factor(df[[var]])) NA else min(df[[var]], na.rm = TRUE),
                             RR3 = if(is.factor(df[[var]])) "Not Applicable to this variable" else rr3Check(df[[var]]),
                             GRR = if(is.factor(df[[var]])) "Not Applicable to this variable" else grrCheck(df[[var]]))
    return(summaryRow)
  })
  execSummaryDf <- do.call(rbind, numericVars)

  #add summary worksheet with executive summary data
  addWorksheet(wb, "Summary")
  writeData(wb, "Summary", execSummaryDf)

  #column order based on the original dataframe
  columnOrder <- names(df)

  # addd a worksheet for each variable in detail_summary_vars and summarize
  walk(categoricalVars, function(var) {
    addWorksheet(wb, var)
    summarisedData <- summariseAndFilter(df, var, columnOrder, categoricalVars)
    writeData(wb, var, summarisedData)
  })

  # Save the workbook
  saveWorkbook(wb, file = fullFilePath, overwrite = TRUE)
}

