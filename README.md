# Overview
CustomisedDataTools is a R package that was developed as part of an internship I undertook with Stats NZ. This package, developed for the Customised Data Requests Team within Stats, offers consistent data summaries to analysts, streamlining data validation before delivery to customers. Key features include functionalities for sorting dataframes with custom priorities, summarizing dataframes with custom grouping, producing summary statistics for specified variables, and generating custom concordances, especially for geographical data.

# Installation
To install, run: 
`
devtools::install_git(
  "https://gitlabstats-prd/data-services/r-packages/customiseddatatools",
  build_vignettes = TRUE
)
`

Once the package has been downloaded, run the following command - 

`
install.packages("Name_of_Package.tar.gz", repos = NULL, type = "source")

`

Note - Name_of_Package could be different, depending on the current development phase of the package, a sample name for the package is `CustomisedDataTools_0.1.0.9008.tar.gz`


Once the installation is complete, run the following commands and get the necessary libaries installed, in order to run all the functions in this package

`
library(readr)
library(CustomisedDataTools)
`

# Functions and sample calls

## customSort 

In any given input dataframe, all rows with NA are pushed to the top of the dataframe, rest of the values are alphanumerically sorted based on the left most column variable. 

Rows with Total and Total stated are pushed to the bottom. 

subtotals is an optional parameter where user can mention variables in a column variable that will be put above rows with Total or Total stated. 

sample call :

`
input_df <- read.csv("path/to/sample_file.csv")
output_df <- customSort(input_df, subtotals = c( "sample_column_variable" = "sample_value_from_column_variable")
`


## checkTable
Creates a workbook with multiple summaries of a given dataframe, each summary being stored in a separate worksheet. Each summary is of a column variable



sample call :
`
input_df <- read.csv read.csv("path/to/sample_file.csv")
checkTable(input_df, c("Numerical_var1", "Numerical_var2"),
                c("Categorical_var1", "Categorical_var2"), "path/to/where/workbook/should/be/saved")
`

Name of the workbook file saved - 
`checkTable-{df_name_assigned_by_user}.xlsx`


## checkVariable
Produces pair-wise counts of columns and outputs to an Excel workbook, with options for specifying variable pairs and rounding counts. 
Each variable pair mentioned is stored in a separate worksheet

sample call:
`
input_df <- read.csv read.csv("path/to/sample_file.csv")
checkVariable(input_df, list(c("var1", "var2"), 
                  c("var3", "var4"), c("var5", "var6" )), 
                  FALSE, "path/to/where/workbook/should/be/saved")
`


## customConcordance 
Aligns a dataframe with a specified mapping. The function also supports custom user-provided concordances. If user-provided concordances are being used - mention the file path of the CSV file in the function argument. 

sample call: 
`
input_df <- readr::read_csv("path/to/sample_file.csv", 
                col_types = readr::cols(.default = "c"))

output_df <- customConcord(input_df_concordance, "colVariable_on_which_join_needs_to_happen",
                "concordance_you_have", "concordance_you_want")
`



# License
This code is licensed under GPL-3.0. To view a copy of this license, visit https://www.gnu.org/licenses/gpl-3.0.en.html . 

