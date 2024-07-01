################################################################################
# Description: Generating Custom Concordances
# Author: Ashish Agnihotri
#
# Notes:
# - Needs a dataframe as an input, do not provide a file path
#
#
#
# Issues:
#
#
################################################################################

utils::globalVariables(c("n"))

#' Applying Custom Concordances
#'
#' This function aligns a dataframe with a specified concordance mapping, enabling the
#' transformation of existing data categories to a new categorization system. It supports
#' both a pre-loaded master concordance and custom user-provided concordances. The
#' function is particularly useful for re-mapping or re-classifying data, such as geographic
#' areas, but is also adaptable to other types of categorical data. All concordances till
#' 2024 are already loaded in this package. If your concordanceColumn and targetColumn are
#' already present in this package, there is no need to provide concordanceFilePath. If its not present
#' and you'd like the concordances in this package to be up-to-date, please contact the maintainer
#' of this package (details present in the DESCRIPTION file).
#'
#' Run 'colnames(MasterConcordance)' to check whether the intended concordances are present in the package.
#'
#'
#'
#'
#' @param df The dataframe to which the targetColumn is applied
#' @param joinColumn The name of the column in data used to join with concordanceColumn in the concordance.
#' @param concordanceColumn The name of the column in the concordance representing the original categorization in data.
#' @param targetColumn The name of the column in the concordance representing the new categorization to be applied to data.
#' @param ConcordanceSource Optional file path to a custom concordance CSV file or a dataframe containing concordance data.
#' If NULL, a pre-loaded master concordance is used.
#' @param uniqueMapping Boolean value indicating whether to remove duplicates.
#'   If TRUE, only the first occurrence of each start_area is kept.
#'
#' @return A dataframe with the concordance applied.
#' @importFrom utils write.csv
#' @export
#'
customConcord <- function(df, joinColumn, concordanceColumn, targetColumn,
                          ConcordanceSource = NULL , uniqueMapping = TRUE) {

  
  if (!is.data.frame(df)) {
    stop("The input is not a dataframe.")
  }


  # load or read concordance
  if (!is.null(ConcordanceSource)) {
    if (is.character(ConcordanceSource)) {
      # If concordance_file_path is a character vector, read the file
      MasterConcordance <- utils::read.csv(ConcordanceSource)
    } else {
      stop("concordanceFilePath must be a character string representing a file path")
    }
  } else {


    utils::data("MasterConcordance")

    if (!exists("MasterConcordance")) {
      stop("MasterConcordance data not found, please provide a file path to the MasterConcordance")
    }
  }



  
  if (!is.data.frame(MasterConcordance) || nrow(MasterConcordance) == 0) {
    stop("MasterConcordance is not a valid dataframe or is empty.")
  }

  
  if (!(concordanceColumn %in% names(MasterConcordance)) || !(targetColumn %in% names(MasterConcordance))) {
    stop("One or more mentioned areas are not found in MasterConcordance.")
  }


  # creating concordance
  concordance <- dplyr::count(
    dplyr::group_by(
      MasterConcordance,
      !!as.name(concordanceColumn),
      !!as.name(targetColumn)
    )
  )



  # handling duplications
  adjustedConcordance <- if (uniqueMapping) {
    groupedConcordance <- dplyr::group_by(concordance, !!as.name(concordanceColumn))
    slicedConcordance <- dplyr::slice_head(groupedConcordance, n = 1)
    selectedConcordance <- dplyr::select(slicedConcordance, -n)
    selectedConcordance
  } else {
    concordance
  }


  # join with the selected concordance values
  names(concordanceColumn) = joinColumn
  outputDf <- dplyr::left_join(df, adjustedConcordance, by = concordanceColumn)

  return(outputDf)
}


#'

# function to load MasterConcordance when package is loaded
#' MasterConcordance Data Set
#'
#' A dataset containing the master concordance information.
#' This data set is loaded automatically in the user session when the package is loaded.
#'
#' @param libname sample description for libname
#' @param pkgname sample description for pkgname
#'
#' @format A data frame with all concordances
#' @source database
#'
.onAttach <- function(libname, pkgname) {
  utils::data("MasterConcordance", package = "CustomisedDataTools", envir = .GlobalEnv)
  packageStartupMessage("MasterConcordance data loaded into the global environment.")
}


