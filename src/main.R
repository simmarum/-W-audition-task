library(readr)

#' Initialize summary file - reset content of file
#'
#' @param x path to summary file
#' @examples
#' summaryINIT(pathToFile)
summaryINIT <- function(x) {
  sink(x)
}

#' Enable capture output (like print) to summary file - append future content to this file
#'
#' @param x path to summary file
#' @examples
#' summaryON(pathToFile)
summaryON <- function(x) {
  sink(x, append = TRUE)
}

#' Disable capture output (like print) from any file
#' Enalbe capture output (like print) to console
#'
#' @examples
#' summaryOFF(pathToFile)
summaryOFF <- function() {
  sink()
}

### MAIN

#path to summary file
summaryFile = "./summary.txt"

#init summary file
summaryINIT(summaryFile)
summaryOFF()

# read data
colClass <-
  c("integer",
    "character",
    "character",
    "character",
    "factor",
    "factor",
    "factor")
colNames <-
  c("learner_id",
    "country",
    "in_course",
    "unit",
    "avg_score",
    "completion",
    "inv_rate")
data2018 <-
  read.csv(
    file = "../data/data2018.csv",
    header = TRUE,
    sep = ";",
    dec = ".",
    na.strings = c(""),
    colClasses = colClass,
    col.names = colNames,
    encoding = "UTF-8-BOM"
  )


# small summary of data

## empty column
## @knitr MDisNaCol
isNaCol <- data.frame(sapply(data2018,
                  function(x)
                    print(sum(is.na(x)))))
colnames(isNaCol) <- c("Empty values")
summaryON(summaryFile)
print("Table with empty values in every column:")
print(isNaCol)
summaryOFF()



