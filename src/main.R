library(readr)
library(ggplot2)

#functions

#' Plot bar graph from data frame
#' 
#' @param x A data.frame.
#' @param ax A name of X axis.
#' @param ay A name of Y axis.
#' @param angle A angle of X axis ticks (in degrees)
#' @return The plot of \code{x} with \code{ax} horizontal axis and \code{ay} vertical axis.
plot_bar <- function(x,ax,ay,angle=0){
  plot_temp <-
    ggplot(data = x, aes_string(x = ax, y = ay)) +
    geom_bar(stat = "identity", width = 0.8, fill = "cadetblue3") +
    geom_text(
      aes_string(label = ay),
      vjust = -0.2,
      color = "black",
      size = 3.5
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(margin=margin(4), size=8,angle=angle))
  return(plot_temp)
}

#' Plot pie graph from data frame
#' 
#' @param x A data.frame.
#' @param ax (string with quotes) A name of X axis.  (what will be in legend)
#' @param ay (string with quotes) A name of Y axis. (number of every item in legend)
#' @return The pie plot of \code{x}.
plot_pie <- function(x,ax,ay){
  # create blank theme (for pie chart)
  blank_theme <- theme_minimal()+
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.border = element_blank(),
      panel.grid=element_blank(),
      axis.ticks = element_blank(),
      plot.title=element_text(size=14, face="bold")
    )
  # prepare variable to plot
  number_all_row <- sum(x[[ay]])
  yy <- x[[ay]]/2 + c(0, cumsum(x[[ay]])[-length(x[[ay]])])
  yy_percent <- paste(signif(x[[ay]]/number_all_row, 3) * 100, "%", sep="") 
  # pie graph
  plot_temp <-
    ggplot(data = x, aes_string(x = "\"\"",y = ay, fill = ax)) +
    geom_bar(width = 1, stat = "identity") +
    coord_polar("y", start = 0) +
    blank_theme +
    scale_fill_brewer(palette = "Blues") +
    theme(axis.text.x=element_blank()) +
    geom_text(aes_q(y = yy, label = yy_percent), size=5)
  return(plot_temp)
}


plot_hist <- function(x,xlab,ylab,binwidth = 0.03){
  plot_temp = qplot(x, geom="histogram",binwidth = binwidth,xlab=xlab,ylab=ylab)
  return(plot_temp)
}

# read data
colClass <-
  c("integer",
    "factor",
    "factor",
    "factor",
    "double",
    "double",
    "double")
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
number_of_part <- nrow(data2018)


## empty column
isNaCol <- data.frame(sapply(data2018,
                             function(x)
                               print(sum(is.na(
                                 x
                               )))))
colnames(isNaCol) <- c("Empty values") # add columns names


## remove rows with NA in learner_id
data2018 <- data2018[!(is.na(data2018$learner_id)), ]
number_of_part_no_na <- nrow(data2018)


## plot country
number_of_country <-
  length(unique(data2018$country)) # count unique country name

country_vec <-
  table(data2018$country) # create table from country only

country_fisrt_10 <-
  data.frame(head(sort(country_vec, decreasing = TRUE), n = 10)) # get first 10 country with the most numer of participants
country_last_1 <-
  data.frame(country_vec[country_vec == 1]) # get country with only 1 participant

colNamesCountry <- c("Country", "Frequency") # create columns names
colnames(country_fisrt_10) <- colNamesCountry # add columns name
colnames(country_last_1) <- colNamesCountry

# create plot (bar) from first 10 country
plot_country_10 <- plot_bar(country_fisrt_10,"Country","Frequency",angle=0)


## in course
course_vec <- table(data2018$in_course) # table with in_course
names(course_vec)[names(course_vec) == "f"] <- "False" #change f to False
names(course_vec)[names(course_vec) == "t"] <- "True" # change t to True
course_vec <- data.frame(course_vec,colClass <- c("factor","integer"))
colNamesCourse <- c("Teacher", "Frequency") # create columns names
colnames(course_vec) <- colNamesCourse # add columns name
## create plot (pie) course
plot_course <- plot_pie(course_vec,"Teacher", "Frequency")
freq_f = scales::percent(course_vec$Frequency[course_vec$Teacher == "False"]/number_of_part_no_na)
freq_t = scales::percent(course_vec$Frequency[course_vec$Teacher == "True"]/number_of_part_no_na)

## chapter
chapter_vec <- factor(data2018$unit,exclude = NULL) # factor from unit column
chapter_vec <- factor(chapter_vec,levels(chapter_vec)[c(1,13,5,14,6,15,7,16,8:12,2:4,17,18)],exclude = NULL) #reorder levels in factor (to ascending)
chapter_na <- sum(is.na(data2018$unit))
chapter_vec <- data.frame(table(chapter_vec))
colNamesCourse <- c("Unit", "Frequency") # create columns names
colnames(chapter_vec) <- colNamesCourse # add columns name
# create plot (bar) from first 10 country
plot_unit <- plot_bar(chapter_vec,"Unit","Frequency",angle=45)

## score
score_vec <-data2018$avg_score
score_na <- sum(is.na(score_vec)) # number of NA values
number_score_incorect <- sum(score_vec>1,na.rm = TRUE) # incorect percentage (greater than 1)
score_vec_cor <- score_vec[!score_vec>1] # remove incorrect percentage
score_vec_cor <- score_vec_cor[!(is.na(score_vec_cor))] #remove NA values
# create plot (hist)
summary_score <- summary(score_vec_cor)
ecdf_score_half <- scales::percent(ecdf(score_vec_cor)(0.5)) # get percent of score for half of participants
plot_score <- plot_hist(score_vec_cor,"Average score","Frequency")
