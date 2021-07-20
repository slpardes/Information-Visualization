#Sammy Pardes
#IST 719
#Final Project

#### Resources ####
#https://www.kaggle.com/heesoo37/120-years-of-olympic-history-athletes-and-results?select=athlete_events.csv
#https://www.britannica.com/list/7-significant-political-events-at-the-olympic-games
#https://colorswall.com/palette/109/ 
#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
#https://stackoverflow.com/questions/16159253/r-count-unique-values-by-category
#https://stackoverflow.com/questions/14868661/how-to-get-the-maximum-value-by-group
#https://stackoverflow.com/questions/41917761/r-ggplot-getting-all-discrete-x-values-to-be-displayed-on-axis-in-histogram
#https://www.edureka.co/community/3900/how-to-extract-every-nth-element-of-a-vector-using-r
#https://stackoverflow.com/questions/37173770/aggregate-function-in-r-using-two-columns-simultaneously
#https://r-graphics.org/recipe-bar-graph-labels
#https://olympics.com/ioc/faq/history-and-origin-of-the-games/when-did-women-first-compete-in-the-olympic-games
#https://stackoverflow.com/questions/27433798/how-can-i-change-the-y-axis-figures-into-percentages-in-a-barplot
#https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html
#https://ggplot2.tidyverse.org/reference/scale_gradient.html
#https://stackoverflow.com/questions/50163072/different-colors-with-gradient-for-subgroups-on-a-treemap-ggplot2-r
#https://eriqande.github.io/rep-res-web/lectures/making-maps-with-R.html
#https://www.statology.org/r-aggregate-multiple-columns/
#https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
#https://stackoverflow.com/questions/26489276/separate-name-into-firstname-and-lastname-columns-of-data-frame
#https://towardsdatascience.com/create-a-word-cloud-with-r-bde3e7422e8a
#https://www.topendsports.com/events/summer/oldest-youngest.htm
#https://en.wikipedia.org/wiki/Winter_Olympic_Games

#### Load Libraries ####

library(ggplot2)
library(dplyr)
library(wordcloud)
library(unglue)
#library(ggridges)
library(gridExtra)
library(treemap)

#### Data Loading and Cleaning #### 

#load Olympic athlete data set
olympics <- read.csv("C://Users/slpar/OneDrive/Desktop/graduate/IST719/work-in-progress/athlete_events.csv"
                     , header = TRUE
                     , stringsAsFactors = FALSE
)

#load countries file
countries <- read.csv("C://Users/slpar/OneDrive/Desktop/graduate/IST719/work-in-progress/noc_regions.csv")

#remove notes column 
countries <- countries[, 1:2] 

#merge Olympic and country files on "NOC" value
olympics <- merge(olympics, countries, by = "NOC")

#preview data
head(olympics)

length(unique(olympics$ID))
length(unique(olympics$Sport))
length(unique(olympics$NOC))

#### Create Color Palette ####
my_blue <- "#3e76ec"
my_yellow <- "#ffce01"
my_green <- "#179a13"
my_red <- "#ff0000"

olympic_colors <- c(my_red, my_yellow, my_green, my_blue)

#### Single Dimensional Plot: Age ####
ggplot(olympics) + 
  aes(x = Age, y = ..count..) +
  geom_density(fill = my_green, col = my_green) +
  ggtitle("Olympic Athlete Age Distribution") +
  xlab("Age") +
  ylab("Number of Athletes") +
  xlim(0,75)  +
  geom_vline(aes(xintercept = mean(Age, na.rm = T)), 
             linetype = "dashed", size = 0.6) 

mean(olympics$Age, na.rm = T)
min(olympics$Age, na.rm = T)

#### Single Dimensional Plot: Height ####
box_h <- ggplot(olympics) + 
  aes(y = Height) + 
  geom_boxplot(fill = my_blue)

mean(olympics$Height, na.rm = T)
olympics[which.max(olympics$Height), ]
olympics[which.min(olympics$Height), ]

#### Single Dimensional Plot: Weight ####
box_w <- ggplot(olympics) + 
  aes(y = Weight) + 
  geom_boxplot(fill = my_yellow)

mean(olympics$Weight, na.rm = T)
olympics[which.max(olympics$Weight), ]
olympics[which.min(olympics$Weight), ]

grid.arrange(box_h, box_w, nrow = 1)

#### Single Dimensional Plot:  Names ####
olympics_unique <- olympics[unique(olympics$ID), ]
dim(olympics_unique)

olympic_names <- unglue_unnest(olympics_unique, Name, "{FirstName} {LastName}")

olympic_names_m <- olympic_names[olympic_names$Sex == "M", ]
olympic_names_f <- olympic_names[olympic_names$Sex == "F", ]

names_m <- olympic_names_m$FirstName
names_f <- olympic_names_f$FirstName

names_m_df <- as.data.frame(table(names_m))
names_f_df <- as.data.frame(table(names_f))

as.data.frame(sort(table(names_m), decreasing = T)[1:10])
as.data.frame(sort(table(names_f), decreasing = T)[1:10])

#par(mar = c(0,0,0,0))

wordcloud(words = names_m_df$names_m, 
          freq = names_m_df$Freq, 
          min.freq = 10,           
          max.words=50, 
          random.order=FALSE, 
          rot.per=0.35,            
          colors= c(my_green, my_blue)
)

wordcloud(words = names_f_df$names_f, 
          freq = names_f_df$Freq, 
          min.freq = 10,           
          max.words=50, 
          random.order=FALSE, 
          rot.per=0.35,            
          colors= c(my_yellow, my_red)
)

#### Single Dimensional Plot: Athletes by Sex, Season ####

season <- aggregate(olympics$ID, list(olympics$Season, olympics$Sex), length)
colnames(season) <- c("season", "sex", "n_athletes")
season

ggplot(season) +
  aes(x = season, y = n_athletes, fill = sex) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values =c (my_red, my_blue))

#### Multi-Dimensional Plot: Map ####
athletes_by_country <- as.data.frame(aggregate(olympics$ID, list(olympics$region), FUN = function(x) length(unique(x))))
colnames(athletes_by_country) <- c("region", "n_athletes")
dim(athletes_by_country)
head(athletes_by_country)

athletes_by_country[which.max(athletes_by_country$n_athletes), ]
athletes_by_country[which.min(athletes_by_country$n_athletes), ]

athletes_by_country[order(-athletes_by_country$x)[1:10],]

athletes_by_country$region[athletes_by_country$region == "Boliva"] <- "Bolivia"

world <- map_data("world")

athletes_map <- inner_join(athletes_by_country, world, by = "region")
head(athletes_map)

map_base <- ggplot(data = athletes_map, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")

map_base + 
  geom_polygon(data = athletes_map, aes(fill = n_athletes), color = "gray") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ggtitle("Number of Olympians by Country, 1896-2016") + 
  scale_fill_gradientn(colors = c(my_green, my_yellow, my_red), na.value = "white")

#### Multi Dimensional Plot: Gender & Year ####
gender_year <- as.data.frame(aggregate(olympics$Name, list(olympics$Sex, olympics$Year), FUN = function(x) length(unique(x))))
colnames(gender_year) <- c("sex", "year", "n_athletes")

y1896 <- c("F", as.integer(1896), as.integer(0))
gender_year <- rbind(gender_year, y1896)

gender_year$n_athletes <- as.integer(gender_year$n_athletes)
gender_year$year <- as.integer(gender_year$year)

gender_year

years <- min(olympics$Year):max(olympics$Year)
x_axis_labels <- years[seq(1, length(years), 8)]

gender_year_plot <- ggplot(gender_year) +
  aes(x = year, y = n_athletes, fill = sex) +
  geom_area(position = "fill") +
  scale_fill_manual(values = c(my_red, my_blue)) +
  scale_x_continuous(labels = x_axis_labels, breaks = x_axis_labels) +
  scale_y_continuous(labels = scales::percent) +
  xlab("Year") +
  ylab("Number of Athlets") +
  ggtitle("Olympic Participation by Sex, 1896-2016") +
  geom_hline(yintercept = 0.50, linetype = "dashed")

gender_year_plot

#### Multi Dimensional Plot: Age & Sport ####
#age_sport <- aggregate(olympics$Age, list(olympics$Sport), mean, na.rm = T)
#colnames(age_sport) <- c("sport", "avg_age")
#age_sport

#treemap(age_sport, index = "sport"
#        , vSize = "avg_age"
#        , vColor = "avg_age"
#        , type = "dens"
#        , fontcolor.labels = "black"
#        , palette = c(my_blue, my_green, my_yellow, my_red)
#        , title = "Average Olympian Age by Sport"
#)

#### Multi Dimensional Plot: Sports by Number of Athletes ####
num_sport <- as.data.frame(aggregate(olympics$ID, list(olympics$Sport), length))
colnames(num_sport) <- c("sport", "n_athletes")
num_sport

num_sport <- num_sport[num_sport$n_athletes > 1000, ]

treemap(num_sport, index = "sport"
        , vSize = "n_athletes"
        , vColor = "n_athletes"
        , type = "dens"
        , fontcolor.labels = "black"
        , palette = c(my_blue, my_green, my_yellow, my_red)
        , title = "Number of Olympians by Sport (at least 1,000)"
)

num_sport[order(-num_sport$n_athletes)[1:10],]

athletics <- olympics[olympics$Sport == "Athletics", ]
athletics_agg <- aggregate(athletics$ID, list(athletics$Sport, athletics$Event), length)
colnames(athletics_agg) <- c("sport", "event", "n_athletes")
head(athletics_agg)

dim(athletics_agg)

athletics_agg[order(-athletics_agg$n_athletes),]

#### Key Visualization: Height, Weight, Sport, Season ####
height_weight <- aggregate(cbind(Height, Weight) ~ Sport + Season, data = olympics, FUN = mean, na.rm = TRUE)
head(height_weight)

ggplot(height_weight) + 
  aes(x = Weight, y = Height, label = Sport, col = Season) + 
  geom_point(size = 3, alpha = 0.75) +
  geom_text(cex = 2, check_overlap = T, nudge_x = 0.5, nudge_y = 0.5, col = "black") +
  xlab("Weight") +
  ylab("Height") +
  ggtitle("Mean Height and Weight of Olympians by Sport") +
  scale_color_manual(values = c(my_blue, my_green))

height_weight[order(-height_weight$Height)[1:10],]
height_weight[order(height_weight$Height)[1:10],]

height_weight[order(-height_weight$Weight)[1:10],]
height_weight[order(height_weight$Weight)[1:10],]
