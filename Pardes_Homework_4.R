# Sammy Pardes
# IST 719
# Week 4 Homework
# 4/30/2021

#### Part 1 ####

#### Bar Chart ####

hotdogs <- read.csv("http://datasets.flowingdata.com/hot-dog-contest-winners.csv"
                       , sep = ","
                       , header = TRUE)

fill_colors <- c()

for (i in 1:length(hotdogs$New.record)) {
  if (hotdogs$New.record[i] == 1) {
    fill_colors <- c(fill_colors, "#821122")
  }
  else {
    fill_colors <- c(fill_colors, "#cccccc")
  }
}

barplot(hotdogs$Dogs.eaten, names.arg = hotdogs$Year
        , border = NA
        , xlab = "Year"
        , ylab = "Hot dogs and buns (HBD) eaten"
        , col = fill_colors
        , space = 0.3
        , main = "Nathan's Hot Dog Eating Contest Results, 1980-2010"
        , ylim = c(0, 70)
        )



#### Stacked Bar Chart ####

hotdog_places <- read.csv("http://datasets.flowingdata.com/hot-dog-places.csv"
                          , sep = ","
                          , header = TRUE)

names(hotdog_places) <- c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010")

hotdog_matrix <- as.matrix(hotdog_places)

barplot(hotdog_matrix
        , border = NA
        , space = 0.25
        , ylim = c(0, 200)
        , xlab = "Year"
        , ylab = "Hot dogs and buns (HBD) eaten"
        , main = "Hot Dog Eating Contest Results, 2000-2010" #updated from 1980 to 2000
)

#### Scatter Plot ####

subscribers <- read.csv("http://datasets.flowingdata.com/flowingdata_subscribers.csv"
                        , sep = ","
                        , header = TRUE)

plot(subscribers$Subscribers
     , type = "h"
     , ylim = c(0, 30000)
     , xlab = "Day"
     , ylab = "Subscribers"
)

points(subscribers$Subscribers
       , pch = 19
       , col = "#821122")

#### Time Series ####

population <- read.csv("http://datasets.flowingdata.com/world-population.csv"
                       , sep = ","
                       , header = TRUE)

plot(population$Year
     , population$Population
     , type= "l"
     , ylim = c(0, 7000000000)
     , xlab = "Year"
     , ylab = "Population")


#### Step Chart ####

postage <- read.csv("http://datasets.flowingdata.com/us-postage.csv"
                    , sep = ","
                    , header = TRUE)

plot(postage$Year
     , postage$Price
     , type = "s" #type = s for step plot
     , main = "US Postage Rates for Letters, First Ounce, 1991-2010"
     , xlab = "Year"
     , ylab = "Postage Rate (Dollars)"
)

#### LOESS Curve ####

unemployment <- read.csv("http://datasets.flowingdata.com/unemployment-rate-1948-2010.csv"
                         , sep = ","
                         , header = TRUE)


scatter.smooth(x=1:length(unemployment$Value), y=unemployment$Value
               , ylim = c(0, 11)
               , degree = 2
               , col = "#cccccc"
               , span = 0.5
               #, xlim = c(0, 800)
               )

#### Part 2 ####

#### Scatterplot Matrix ####

crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.csv"
                     , sep = ","
                     , header = TRUE)

crime[1:3, ]

plot(crime$murder, crime$burglary)


crime2 <- crime[crime$state != "District of Columbia", ]
crime2 <- crime2[crime2$state != "United States", ]

plot(crime2$murder, crime2$burglary
     , xlim = c(0, 10)
     , ylim = c(0, 1200)
     )

scatter.smooth(crime2$murder, crime2$burglary
               , xlim = c(0, 10)
               , ylim = c(0, 1200)
               )

plot(crime2[, 2:9], panel = panel.smooth)

#### Bubble Chart ####

crime <- read.csv("http://datasets.flowingdata.com/crimeRatesByState2005.tsv"
                  , sep = "\t"
                  , header = TRUE)

symbols(crime$murder, crime$burglary, circles = crime$population)

radius <- sqrt(crime$population/pi)

symbols(crime$murder, crime$burglary, circles = radius
        , inches = 0.35
        , fg = "white"
        , bg = "red"
        , xlab = "Murder Rate"
        , ylab = "Burglary Rate"
        )

text(crime$murder, crime$burglary, crime$state, cex = 0.5)

#### Histogram ####

birth <- read.csv("http://datasets.flowingdata.com/birth-rate.csv"
                  , sep = ","
                  , header = TRUE)

hist(birth$X2008
     , xlim = c(0, 60)
     , ylim = c(0,60)
     , col = "purple"
     , main = "Global Distribution of Birth Rates"
     )

#### Density Plot ####

birth2008 <- birth$X2008[!is.na(birth$X2008)]

d2008 <- density(birth2008)

density.default(x = birth2008)

plot(d2008, type = "n")

polygon(d2008, col = "#821122", border = "#cccccc")

#### Olympic Data Plots ####

# Data Cleaning

#load dictionary data
dictionary <- read.csv("C://Users/slpar/OneDrive/Desktop/graduate/IST719/data/dictionary.csv"
                       , header = TRUE
                       , stringsAsFactors = FALSE
                       )

#load summer data
summer <- read.csv("C://Users/slpar/OneDrive/Desktop/graduate/IST719/data/summer.csv"
                   , header = TRUE
                   , stringsAsFactors = FALSE
                   )

#load winter data
winter <- read.csv("C://Users/slpar/OneDrive/Desktop/graduate/IST719/data/winter.csv"
                   , header = TRUE
                   , stringsAsFactors = FALSE
                   )

#add column for season
summer["Season"] <- "Summer"
winter["Season"] <- "Winter"

#combine summer + winter 
olympics <- rbind(summer, winter)

#update country column name to country code
colnames(olympics)[which(names(olympics) == "Country")] <- "Code"

#merge dictionary and summer data sets on "Code"
olympics <- merge(olympics, dictionary, by = "Code")

str(olympics)

# Plot 1 

library(lattice)

olympics$Medal <- as.factor(olympics$Medal)

colors = c("peru", "gold", "azure3")

h <- histogram(~ Medal | Country
          , data = olympics
          , layout = c(10,13)
          , col = colors
          )

update(h, index.cond = list(c(121:130, 111:120, 101:110, 91:100, 81:90, 71:80, 61:70, 51:60, 41:50, 31:40, 21:30, 11:20, 1:10)))

# Plot 2

olympics$Gender <- as.factor(olympics$Gender)

h2 <- histogram(~ Gender | olympics$Sport
               , data = olympics
               , col = c("blue", "pink1")
               , layout = c(8,6)
               )

update(h2, index.cond = list(c(42:48, 36:41, 30:35, 24:29, 18:23, 12:17, 6:11, 1:5)))
       