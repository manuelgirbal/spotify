#First you have to request and download your data form Spotify: https://www.spotify.com/us/account/privacy/
#After unzipping the downloaded file, we can proceed:

library(jsonlite)
library(lubridate)
library(gghighlight)
library(spotifyr)
library(tidyverse)
library(knitr)
library(plotly)


# READING JSON STREAMING HISTORY
streamHistory <- fromJSON("MyData/StreamingHistory0.json", flatten = TRUE)

# ADDING DATE AND TIMING
mySpotify <- streamHistory %>% 
  as_tibble() %>% 
  mutate_at("endTime", ymd_hm) %>% 
  mutate(endTime = endTime - hours(6)) %>% 
  mutate(date = floor_date(endTime, "day") %>% as_date, seconds = msPlayed / 1000, minutes = seconds / 60)

# PLAYBACK ACTIVITY PER WEEK AND HOURS
streamingHours <- mySpotify %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(date) %>% 
  group_by(date = floor_date(date, "week")) %>%
  summarize(hours = sum(minutes) / 60) %>% 
  arrange(date) %>% 
  ggplot(aes(x = date, y = hours)) + 
  geom_col(aes(fill = hours)) +
  scale_fill_gradient(low = "yellow", high = "red") + 
  labs(x= "Date", y= "Hours of music playback") + 
  ggtitle("On what dates I've listened to more or less music on Spotify?", "Playback activity per week")

streamingHours


# PLAYBACK ACTIVITY PER SPECIFIC ARTIST
#Here we highlight the results you are looking for using the “gghighlight” library, and through the artistName variable
hoursArtist <- mySpotify %>% 
  group_by(artistName, date = floor_date(date, "month")) %>% 
  summarize(hours = sum(minutes) / 60) %>% 
  ggplot(aes(x = date, y = hours, group = artistName)) + 
  labs(x= "Date", y= "Hours of music playback") + 
  ggtitle("On what dates I've listened to more or less music by a specific artist?", "E.g: Kendrick Lamar and Miles Davis") +
  geom_line() +
  gghighlight(artistName == "Kendrick Lamar" || artistName == "Miles Davis") 

hoursArtist


# MOST LISTENED ARTISTS (MORE THAN 3 HOURS)
minutesMostListened <- mySpotify %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(artistName) %>% 
  summarize(minutesListened = sum(minutes)) %>% 
  filter(minutesListened >= 180) %>%
  ggplot(aes(x = artistName, y = minutesListened)) + 
  geom_col(aes(fill = minutesListened)) +
  scale_fill_gradient(low = "yellow", high = "red") + 
  labs(x= "Artist", y= "Minutes of music playback") + 
  ggtitle("What were the most listened artists on my Spotify?", "> 3 hours listened") +
  theme(axis.text.x = element_text(angle = 90))
minutesMostListened


# PLAYBACK ACTIVITY BY DATE AND TIME OF DAY
timeDay <- mySpotify %>% 
  filter(date >= "2020-01-01") %>% 
  group_by(date, hour = hour(endTime)) %>% 
  summarize(minutesListened = sum(minutes)) %>% 
  ggplot(aes(x = hour, y = date, fill = minutesListened)) + 
  geom_tile() + 
  labs(x= "Time of the day", y= "Date") + 
  ggtitle("When has there been more playback activity on my Spotify?", "Activity by date and time of day") +
  scale_fill_gradient(low = "yellow", high = "red")
timeDay

# PLAYBACK ACTIVITY BY TIME OF THE DAY
hoursDay <- mySpotify %>% 
  filter(date >= "2019-01-01") %>% 
  group_by(date, hour = hour(endTime), weekday = wday(date, label = TRUE))%>% 
  summarize(minutesListened = sum(minutes))

hoursDay %>% 
  ggplot(aes(x = hour, y = minutesListened, group = date)) + 
  geom_col(fill = "#ff6600") +
  labs(x= "Time of the day", y= "Minutes of music playback") + 
  ggtitle("What time of day I've listened to the most music on Spotify?", "Activity from 0 to 24 hours")


# PLAYBACK ACTIVITY BY TIME OF THE DAY AND WEEKDAY
hoursDay %>% 
  group_by(weekday, hour) %>% 
  summarize(minutes = sum(minutesListened)) %>% 
  ggplot(aes(x = hour, weekday, fill = minutes)) + 
  geom_tile() + 
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(x= "Time of the day", y= "Weekday") + 
  ggtitle("What weekday and time of day I've listened to the most music on Spotify?", "Weekly activity from 0 to 24 hours")

# PLAYBACK ACTIVITY BY TIME OF THE DAY AND WEEKDAY - LINE CHART
weekDay <- hoursDay %>% 
  group_by(weekday, hour) %>% 
  summarize(minutes = sum(minutesListened)) %>% 
  ggplot(aes(x = hour, y = minutes, color = weekday)) + 
  geom_line() +
  labs(x= "Time of the day", y= "Minutes of music playback") + 
  ggtitle("What weekday and time of day I've listened to the most music on Spotify?", "Line chart - Weekly activity from 0 to 24 hours") 
weekDay

# PLAYBACK ACTIVITY BY DAY TYPE
dayType <- hoursDay %>% 
  mutate(day_type = if_else(weekday %in% c("sáb\\.", "dom\\"), "weekend", "weekday")) %>% 
  group_by(day_type, hour) %>% 
  summarize(minutes = sum(minutesListened)) %>% 
  ggplot(aes(x = hour, y = minutes, color = day_type)) + 
  geom_line() +
  labs(x= "Time of the day", y= "Minutes of music playback") + 
  ggtitle("What day type I've listened to the most music on Spotify?", "Weekday and weekend activity from 0 to 24 hours") 
dayType
