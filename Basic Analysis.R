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

#Extracts the desired columns from each data set and stores in new variables
extract2015 <- report2015 %>% select(Country, 
                                     Happiness.Rank, 
                                     Happiness.Score, 
                                     Economy..GDP.per.Capita.,
                                     Health..Life.Expectancy.,
                                     Trust..Government.Corruption.)
extract2016 <- report2016 %>% select(Country, 
                                     Happiness.Rank, 
                                     Happiness.Score, 
                                     Economy..GDP.per.Capita., 
                                     Health..Life.Expectancy., 
                                     Trust..Government.Corruption.)
extract2017 <- report2017 %>% select(Country, 
                                     Happiness.Rank, 
                                     Happiness.Score, 
                                     Economy..GDP.per.Capita., 
                                     Health..Life.Expectancy., 
                                     Trust..Government.Corruption.)
extract2018 <- report2018 %>% select(Country.or.region, 
                                     Overall.rank, 
                                     Score,
                                     GDP.per.capita,
                                     Healthy.life.expectancy,
                                     Perceptions.of.corruption)
extract2019 <- report2019 %>% select(Country.or.region, 
                                     Overall.rank, 
                                     Score,
                                     GDP.per.capita,
                                     Healthy.life.expectancy,
                                     Perceptions.of.corruption)

#Changes the column names for two data sets to match the rest. This will allow
#for the tables to all be joined on the same key.
colnames(extract2018)[1] <- 'Country'
colnames(extract2019)[1] <- 'Country'

#Joins the five data sets on the ID "Country"
require(purrr)
require(dplyr)

joined <- list(extract2015, extract2016, extract2017, extract2018, extract2019) %>%
  reduce(left_join, by = "Country")

#Renames the columns of the table using a grep function, which finds table 
#indices by looking up column names with a certain keyword, then uses those
#indices to find the columns to rename.
colnames(joined)[grep('score',colnames(joined),ignore.case = TRUE)] <- "Score"
colnames(joined)[grep('rank',colnames(joined),ignore.case = TRUE)] <- "Rank"
colnames(joined)[grep('GDP',colnames(joined),ignore.case = TRUE)] <- "GDP.per.Capita"
colnames(joined)[grep('life',colnames(joined),ignore.case = TRUE)] <- "Healthy.Life.Expectancy"
colnames(joined)[grep('corruption',colnames(joined),ignore.case = TRUE)] <- "Corruption"

#Applies as a suffix the year of the report that each column is from.
colnames(joined)[2:6] <- paste(colnames(joined)[2:6],'2015',sep='.')
colnames(joined)[7:11] <- paste(colnames(joined)[7:11],'2016',sep='.')
colnames(joined)[12:16] <- paste(colnames(joined)[12:16],'2017',sep='.')
colnames(joined)[17:21] <- paste(colnames(joined)[17:21],'2018',sep='.')
colnames(joined)[22:26] <- paste(colnames(joined)[22:26],'2019',sep='.')

#Creates and appends new columns that contain averages
joined$Mean.Rank <- rowMeans(subset(joined, select = c(grep('rank',colnames(joined),ignore.case = TRUE))), na.rm = TRUE)

joined$Mean.Score <- rowMeans(subset(joined, select = c(grep('score',colnames(joined),ignore.case = TRUE))), na.rm = TRUE)

joined[grep('life',colnames(joined),ignore.case = TRUE)] <- joined[grep('life',colnames(joined),ignore.case = TRUE)]*100

#Views the output
View(joined)
# View(report2015)
# View(report2016)
# View(report2017)
# View(report2018)
# View(report2019)

#Histogram of mean scores
ggplot(joined,aes(x=Mean.Score)) + geom_histogram(binwidth=0.2,fill='beige',color='black')

#Scatterplots of scores by GDP per capita with trend line
spGDP2015 <- ggplot(joined, aes(x=Score.2015,y=GDP.per.Capita.2015)) + geom_point() + geom_smooth()
spGDP2016 <- ggplot(joined, aes(x=Score.2016,y=GDP.per.Capita.2016)) + geom_point() + geom_smooth()
spGDP2017 <- ggplot(joined, aes(x=Score.2017,y=GDP.per.Capita.2017)) + geom_point() + geom_smooth()
spGDP2018 <- ggplot(joined, aes(x=Score.2018,y=GDP.per.Capita.2018)) + geom_point() + geom_smooth()
spGDP2019 <- ggplot(joined, aes(x=Score.2019,y=GDP.per.Capita.2019)) + geom_point() + geom_smooth()

#Arranges above scatterplots onto the same view
grid.arrange(spGDP2015,spGDP2016,spGDP2017,spGDP2018,spGDP2019, nrow = 3)

#Scatterplots of scores by life expectancy with trend line
spLife2015 <- ggplot(joined, aes(x=Score.2015,y=Healthy.Life.Expectancy.2015)) + geom_point() + geom_smooth() + coord_cartesian(xlim=c(2,8),ylim=c(0,120))
spLife2016 <- ggplot(joined, aes(x=Score.2016,y=Healthy.Life.Expectancy.2016)) + geom_point() + geom_smooth() + coord_cartesian(xlim=c(2,8),ylim=c(0,120))
spLife2017 <- ggplot(joined, aes(x=Score.2017,y=Healthy.Life.Expectancy.2017)) + geom_point() + geom_smooth() + coord_cartesian(xlim=c(2,8),ylim=c(0,120))
spLife2018 <- ggplot(joined, aes(x=Score.2018,y=Healthy.Life.Expectancy.2018)) + geom_point() + geom_smooth() + coord_cartesian(xlim=c(2,8),ylim=c(0,120))
spLife2019 <- ggplot(joined, aes(x=Score.2019,y=Healthy.Life.Expectancy.2019)) + geom_point() + geom_smooth() + coord_cartesian(xlim=c(2,8),ylim=c(0,120))

#Arranges the above scatterplots onto the same view
grid.arrange(spLife2015,spLife2016,spLife2017,spLife2018,spLife2019, nrow = 3)