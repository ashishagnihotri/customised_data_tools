% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/variable-checking.R
\name{checkVariable}
\alias{checkVariable}
\title{Produce pair-wise counts of columns and output to Excel workbook.}
\usage{
checkVariable(df, variables = NULL, round = FALSE, outputPath)
}
\arguments{
\item{df}{The data frame to be summarized.}

\item{variables}{A list of pairs of variable names to be summarized.
If NULL, all pairs of variables in the dataframe are used.}

\item{round}{Logical value indicating whether to round the total counts.
If TRUE, the total counts are rounded to the nearest hundred.}

\item{outputPath}{File path where the output Excel workbook will be saved.}
}
\value{
There are no return objects, a workbook in the specified file path is created
}
\description{
It allows for specifying variable pairs needed and rounding the count to the nearest 100.
The data is saved in an Excel sheet with each pair of variables in a separate sheet.
If variable pair is not mentioned by the user, the function will generate a pair of each variable
present in the dataframe.
}
