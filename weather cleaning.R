#exploring raw data
summary(weather)
glimpse(weather)

#looking at the data
head(weather)
tail(weather)
str(weather$measure)

#tidying the data
#making wide dataset long
weather2 <- gather(weather, day, temp, X1:X31, na.rm = TRUE)
head(weather2)
dim(weather2)

#making long dataset wide
weather3 <- spread(weather2, measure, temp)
head(weather3)

dim(weather3)

#preparing the data for analysis:
#I will start by combining the year, month, and day columns
#and recoding the resulting character column as a date. 

library(lubridate)
library(stringr)

#removing the X's in the day variable and replacing it with an empty space
weather3$day <- str_replace(weather3$day, "X", "")

#Uniting year, month, and day together into one variable: date
weather4 <- unite(weather3, date, year, month, day, sep = "-")
weather4$date <- ymd(weather4$date)

#rearranging columns
weather5 <- select(weather4, date, Events, CloudCover:WindDirDegrees)

head(weather5)

str(weather5)
#Need to change some of these variable types for analysis.
#But before I do that, I need to deal with strange values
#such as "T" in precipitation by converting them to 0's.
#T stands for trace amount - not enough to measure.

weather5$PrecipitationIn <- str_replace(weather5$PrecipitationIn, "T", "0")

weather6 <- mutate_at(weather5, vars(CloudCover:WindDirDegrees), funs(as.numeric))

str(weather6)

#Finding missing values, outliers, and unexpected values.


summary(weather6)

#Counting missing values
total_na <- sum(is.na(weather6))
total_na

#Finding the rows where there are missing values
na_ind <- which(is.na(weather6$Max.Gust.SpeedMPH))
weather6[na_ind,]

#Its not clear why these values are missing at this point.

summary(weather6)
#Looks like there is a value of 1000 in Max.Humidity, which is impossible
#and is probably an error.

#finding the location of that value
ind <- which(weather6$Max.Humidity == 1000)
weather6[ind,]

weather6$Max.Humidity[ind] <- 100 #replacing 1000 with 100
weather6$Max.Humidity[ind]

summary(weather6)
summary(weather6$Mean.VisibilityMiles)
#There appears to be a negative value, which is an error.

ind_ <- which(weather6$Mean.VisibilityMiles == -1)
ind_

head(weather6$Mean.VisibilityMiles, n=20)
#Looking at the variable Max.VisbilityMiles it appears that most of the values range from 1-10. 
#Therefore, I am going to replace -1 with 10, because it probably was meant to be
#input that way.

weather6$Mean.VisibilityMiles[ind_] <- 10
weather6$Mean.VisibilityMiles[ind_]
summary(weather6$Mean.VisibilityMiles)

#Finishing touches:
#I want to now change the column names into something more workable.
#Removing ".", and making everything lowercase.

new_names <- c("date",
"events",
"cloud_cover",
"max_dew_point_f",
"max_gust_speed_mph",
"max_humidity",
"max_sea_level_pressure_in",
"max_temperature_f",
"max_visibility_miles",
"max_wind_speed_mph",
"mean_humidity",
"mean_sea_level_pressure_in",
"mean_temperature_f",
"mean_visibility_miles",
"mean_wind_speed_mph",
"mean_dew_point_f",
"min_dew_point_f",
"min_humidity",
"min_sea_level_pressure_in",
"min_temperature_f",
"min_visibility_miles",
"precipitation_in")

names(weather6) <- new_names
names(weather6)

#replacing empty values in events
weather6$events[weather6$events == ""] <- "None"

head(weather6)
tail(weather6)
