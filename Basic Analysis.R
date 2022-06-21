#Reads the data sets from csv files into R

report2015 <- read.table("/Users/paigeball/Github/R-Projects/Data Files/2015.csv",
                         header=TRUE,
                         sep=",", 
                         row.names=NULL)
report2016 <- read.table("/Users/paigeball/Github/R-Projects/Data Files/2016.csv",
                         header=TRUE,
                         sep=",", 
                         row.names=NULL)
report2017 <- read.table("/Users/paigeball/Github/R-Projects/Data Files/2017.csv",
                         header=TRUE,
                         sep=",", 
                         row.names=NULL)
report2018 <- read.table("/Users/paigeball/Github/R-Projects/Data Files/2018.csv",
                         header=TRUE,
                         sep=",", 
                         row.names=NULL)
report2019 <- read.table("/Users/paigeball/Github/R-Projects/Data Files/2019.csv",
                         header=TRUE,
                         sep=",",
                         row.names=NULL)

#Extracts the columns for country and overall rank from each data set
extract2015 <- report2015 %>% select(Country,Happiness.Rank)
extract2016 <- report2016 %>% select(Country,Happiness.Rank)
extract2017 <- report2017 %>% select(Country,Happiness.Rank)
extract2019 <- report2019 %>% select(Country.or.region,Overall.rank)
extract2018 <- report2018 %>% select(Country.or.region,Overall.rank)

#Renames the extracted columns of the last two data sets to be consistent
#with the others
colnames(extract2018)[colnames(extract2018) == "Country.or.region"] <- "Country"
colnames(extract2018)[colnames(extract2018) == "Overall.rank"] <- "Happiness.Rank"
colnames(extract2019)[colnames(extract2019) == "Country.or.region"] <- "Country"
colnames(extract2019)[colnames(extract2019) == "Overall.rank"] <- "Happiness.Rank"

#Joins the five data sets on the ID "Country"
require(purrr)
require(dplyr)

joined <- list(extract2015, extract2016, extract2017, extract2018, extract2019) %>% 
  reduce(left_join, by = "Country")

#Creates and appends a new column that contains the average score
#for each row/country, then sorts the mean from lowest to highest
joined$rMean <- sort(apply(joined[2:6],1,mean,na.rm=TRUE))

