library(readr)

summaryINIT <- function(){
  sink("./src/summary.txt")
}

summaryON <- function(){
  sink("./src/summary.txt",append=TRUE)
}
summaryOFF <- function(){
  sink()
}

#init summary file
summaryINIT()
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
    file = "./data/data2018.csv",
    header = TRUE,
    sep = ";",
    dec = ".",
    na.strings=c(""),
    colClasses = colClass,
    col.names = colNames,
    encoding = "UTF-8-BOM"
  )


# small summary of data

## empty column
isNaCol <- sapply(data2018, function(x) print(sum(is.na(x))))
summaryON()
print("Table with empty values in every column:")
print(isNaCol)
summaryOFF()
