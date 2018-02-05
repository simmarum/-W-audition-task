library(readr)
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
