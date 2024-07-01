################################################################################
# Description: Sorting dataframe with custom priorities
# Author: Ashish Agnihotri
#
# Notes:
# - Needs a dataframe as an input, do not provide a file path
# -
#
# Issues:
#
#
################################################################################


#' Sorting dataframe with custom priorities
#'
#' Sorts a dataframe based on a custom order, and can handle additional custom
#' sorting order provided in `subtotals`
#'
#' The sorting is done in the following manner -
#'
#' 1) NA occurences in dataframe are displayed
#'
#' 2) Standard values in the dataframe
#'
#' 3) A custom set of values (that can be given as a list of input by the
#' user), configured through the subtotals variable
#'
#' 4) Total stated
#'
#' 5) Total
#'
#'
#'
#' @param df the dataframe to be sorted.
#' @param subtotals A list where one mentions the column variable name and the variable instance that needs to be pushed to the bottom of the dataframe
#'
#' @return A sorted dataframe based on the defined priorities.
#' @export
#'
customSort <- function(df, subtotals = NULL) {
  
  if (!is.data.frame(df)) {
    stop("Dataframe not provided")
  }

  # Check if all subtotals columns are in df
  if (!is.null(subtotals) && !all(names(subtotals) %in% names(df))) {
    stop("Some columns in sub_total_list are not in the dataframe")
  }

  
  if (any(is.na(df))) {
    warning("Dataframe contains NA values, they have been pushed to the top of the output dataframe")
  }

  priorityList <- list()

  # function to get sort priority
  getSortPriority <- function(column, columnName, subTotalList, globalNAPriority) {
    priority <- rep(2, length(column))
    priority[globalNAPriority] <- 1 # Assign highest priority to rows with any NA
    if (!is.null(subTotalList) && columnName %in% names(subTotalList)) {
      priority[column %in% subTotalList[[columnName]]] <- 3
    }
    priority[column == "Total stated"] <- 4
    priority[column == "Total"] <- 5
    return(priority)
  }

  #determine rows with any NA value
  globalNAPriority <- rowSums(is.na(df)) > 0

  # priority list for each column
  for (colName in names(df)) {
    priorityList[[colName]] <- getSortPriority(df[[colName]], colName, subtotals, globalNAPriority)
  }

  #convert each column to character for alphanumeric sorting
  df[] <- lapply(df, as.character)

  # Create an order index
  orderArgs <- c()
  for (colName in names(df)) {
    orderArgs <- c(orderArgs, list(priorityList[[colName]], df[[colName]]))
  }

  orderIndex <- do.call(order, orderArgs)

  # Sort dataframe
  outputDf <- df[orderIndex, ]

  return(outputDf)
}



