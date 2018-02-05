library(readr)
library(ggplot2)

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
plot_country_10 <-
  ggplot(data = country_fisrt_10, aes(x = Country, y = Frequency)) +
  geom_bar(stat = "identity", width = 0.8, fill = "cadetblue3") +
  geom_text(
    aes(label = Frequency),
    vjust = -0.2,
    color = "black",
    size = 3.5
  ) +
  theme_minimal()


## in course
course_vec <- table(data2018$in_course) # table with in_course
names(course_vec)[names(course_vec) == "f"] <- "False" #change f to False
names(course_vec)[names(course_vec) == "t"] <- "True" # change t to True
course_vec <- data.frame(course_vec,colClass <- c("factor","integer"))
colNamesCourse <- c("Teacher", "Frequency") # create columns names
colnames(course_vec) <- colNamesCourse # add columns name

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
# pie graph
plot_course <-
  ggplot(course_vec, aes(x = "", y = Frequency, fill = Teacher)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  blank_theme +
  scale_fill_brewer(palette = "Blues") +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(y = Frequency/2 + c(0, cumsum(Frequency)[-length(Frequency)]), 
                label = scales::percent(Frequency/number_of_part_no_na)), size=5)
freq_f = scales::percent(course_vec$Frequency[course_vec$Teacher == "False"]/number_of_part_no_na)
freq_t = scales::percent(course_vec$Frequency[course_vec$Teacher == "True"]/number_of_part_no_na)
