### --------------------------------------------------
'
### --- Data Science & Music Blog: Sentiment analysis of musical taste across Europe
### --- (C) Dr. Paul Elvers
### --- Created on "2018-01-22 15:29:32 GMT"
### --- Data: Spotify API
' 
### --------------------------------------------------

rm(list = ls())
setwd("~/Spotify")


'Data retrieval'
########################################################

# loading libraries
library(spotifyr)

# Setting system requirements for accessing Spotify API
Sys.setenv(SPOTIFY_CLIENT_ID = 'XXXXXXXXXXXXXXXXXXXXX')
Sys.setenv(SPOTIFY_CLIENT_SECRET = 'XXXXXXXXXXXXXXXXX')
access_token <- get_spotify_access_token(client_id = Sys.getenv('SPOTIFY_CLIENT_ID'), 
                                         client_secret = Sys.getenv('SPOTIFY_CLIENT_SECRET'))

# Retrieving data from Spotify
# Top 50 Charts of all European countries were added to the user profile before
# the function loads all songs from the playlist
eutop50 <- get_user_playlists('delospro') %>%
  get_playlist_tracks()

# retrieving audio features for all musical pieces retrieved in the previous step
features <- get_track_audio_features(eutop50)
eutop50 <- left_join(eutop50, features, by='track_uri')

# Some string preprocessing
# remove "Top 50"
library(tm)
eutop50$playlist_name <- removeWords(eutop50$playlist_name, c("Top", "50", " "))
# Change country names
library(stringr)
eutop50$playlist_name <- str_replace_all(eutop50$playlist_name, "CzechRepublic", "Czech Republic")
eutop50$playlist_name <- str_replace_all(eutop50$playlist_name, "UnitedKingdom", "United Kingdom")


'Plot Europe Map'
########################################

# loading libraries
library(highcharter)
library(scales)

# Mean valence score for each country
eutop50 %>%
  group_by(playlist_name) %>%
  summarise(valence = mean(valence)) -> map_stats

# getting basic map of Europe
eu_map <- get_data_from_map(download_map_data("custom/europe"))
# mergin plotdata
eu_plot<-left_join(map_stats, eu_map[,5:7], by = c("playlist_name" = "name"))
#rescaling the valence scores
eu_plot$valence<-rescale(eu_plot$valence, to = c(-10, 10))

# Specifying the highcharter plot
hcmap("custom/europe", data = eu_plot, value = "valence", 
      nullColor = "#FFFFFF",joinBy = c("hc-a2", "hc-a2"), name = "Mean Emotional Valence Score", 
      #dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#BDBDBD", borderWidth = 0.1,
      tooltip = list(valueDecimals = 2, valueSuffix = " Valence")) %>%
  hc_colorAxis(minColor = "#342D7E", maxColor = "#FDD017") %>%
  hc_title(text = "Emotion Classification of Top 50 Music Charts Across Europe") %>%
  hc_legend(title= "Valence",align = "left", verticalAlign = "middle", reversed = T, margin = 30,
            layout = "vertical", x = 0, y = -40)


'Portugal vs. Spain'
########################################

# loading libraries
library(ggplot2)

#Subset for Balearic Countries
balearic<-eutop50 %>%
  filter(playlist_name == "Spain" | playlist_name == "Portugal") %>%
  select(playlist_name,track_uri,valence)

# Creating index to exclude tracks that are dublicates
balearic_index <- duplicated(balearic$track_uri) | duplicated(balearic$track_uri, fromLast = T)

#Conducting t-test
t.test(valence ~ playlist_name, data=balearic[!balearic_index,])

# Summary statistics for Balearic Countries
balearic_stats<-balearic[!balearic_index, ] %>%
  group_by(playlist_name) %>%
  summarise(sd = sd(valence), se = sd/sqrt(n()), valence = mean(valence))

# Violin plot with means and SE-errorbars
ggplot(balearic[!balearic_index, ], aes(y = valence, x = playlist_name, fill = playlist_name)) +
  geom_violin(alpha = 0.6) +
  geom_jitter(size = 1,alpha = 0.5, width = 0.1, shape = 21, fill = "white") +
  scale_fill_manual(values=c("#342D7E", "#FDD017")) +
  theme(axis.text.x = element_text(size = 12), panel.background = element_blank(), legend.position = "none") +
  geom_errorbar(data = balearic_stats, width = 0.03, aes(ymin = valence - se, ymax = valence + se), colour = "black") +
  geom_point(data = balearic_stats, aes(y = valence,x = playlist_name), shape = 21, size = 3, fill = "black", color = "black") +
  xlab("") +
  ylab("Valence") 

# Top negative valence tracks in Portugal
balearic[!balearic_index, ] %>%
  filter(playlist_name == "Portugal") %>%
  left_join(unique(eutop50[ ,3:6], by = "track_uri")) %>%
  select(valence, track_name, artist_name) %>%
  arrange(valence)

# Top positive valence tracks in Spain
balearic[!balearic_index, ] %>%
  filter(playlist_name == "Spain") %>%
  left_join(unique(eutop50[ ,3:6], by = "track_uri")) %>%
  select(valence, track_name, artist_name) %>%
  arrange(desc(valence))


'Northern vs. Southern Europe'
########################################

# loading libraries
library(forcats)
library(ggplot2)

# using the forcats package for collapsing factor levels
eutop50$direction <- eutop50$playlist_name %>% 
  fct_collapse(north = c("Belgium", "Czech Republic", "Denmark",  
                         "Estonia", "Finland", "France","Germany", 
                         "Iceland", "Ireland", "Latvia", "Lithuania", 
                         "Netherlands", "Norway", "Poland", "Sweden", 
                         "United Kingdom"),
               south = c("Austria","Greece", "Hungary", "Italy", "Portugal", 
                         "Slovakia", "Spain", "Switzerland", "Turkey"))

# Summary statistics for northen vs southern Europe
direction_index <- duplicated(eutop50$track_uri) | duplicated(eutop50$track_uri, fromLast = T)
direction_stats <- eutop50[!direction_index, ] %>%
  group_by(direction) %>%
  summarise(sd = sd(valence, na.rm = T), se = sd/sqrt(n()),valence = mean(valence, na.rm = T))

#Conducting t-test
t.test(valence ~ direction, data=eutop50[!direction_index, ])

# Violin plot with means and SE-errorbars
ggplot(eutop50[!direction_index,], aes(y = valence,x = direction, fill=direction)) +
  geom_violin(alpha = 0.6) +
  geom_jitter(size = 1, alpha = 0.5, shape = 21, width = 0.15, fill = "white") +
  scale_fill_manual(values=c("#FDD017","#342D7E")) +
  geom_errorbar(data = direction_stats, width = .03, aes(ymin = valence-se, ymax = valence+se), colour = "black") +
  geom_point(data = direction_stats, aes(y = valence,x = direction), shape = 23, size = 3, fill = "black", color = "black") +
  theme(axis.text.x = element_text(size = 12), panel.background = element_blank(), legend.position = "none") +
  scale_x_discrete(name = "", labels = c("Southern Europe", "Northern Europe")) +
  ylab("Valence")


'Life satisfaction'
########################################

# loading Better Life Index (bli) 
bli<- read.csv("~/Spotify/bli.csv", stringsAsFactors = F) 

# wrangling to make tidy
bli %>%
  group_by(Country, Indicator) %>%
  summarise(avg = mean(Value, na.rm = T)) %>%
  spread(Indicator, avg) -> bli

# left join on aggregated music data
satisfaction_music <- eutop50[!direction_index, ] %>%
  group_by(playlist_name) %>%
  summarise(valence_mean = mean(valence, na.rm = T)) %>%
  left_join(bli[,c(1,14)], by = c("playlist_name" = "Country"))

# scatterplot with loess regression
ggplot(data=satisfaction_music, aes(x = `Life satisfaction`, y = valence_mean, label = playlist_name)) +
  geom_label() +
  stat_smooth(method = "lm") + 
  ylab("Mean Valence of Chart Music")

# regression model
summary(lm(valence_mean ~ `Life satisfaction`, data = satisfaction_music))
