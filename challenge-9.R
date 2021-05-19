### --- R Challenge 9, Alvin Aziz, 19.05.2021 --- ###

# Challenge 9
# P. Kessling, 29.04.2021

# Heute wollen wir das Kommunikationsverhalten von Twitter-User*innen zu einem
# bestimmtem Thema untersuchen. Suche dafür ein aktuelles Thema und nutze die Twitter-API.
# Durchsuche Twitter nach dem Thema, beachte dabei, dass wir zu mindestens 1500 Tweet
# brauchen.
# Für die einhundert aktivsten Nutzer bestimmen wir das Kommunikations verhalten genauer.
# Lade für diese Nutzer jeweils die letzten 1000 Tweets, falls so viele vorhanden sind
# und Werte visuell nach der Zeit aus.
# ACHTUNG: das Laden der Daten kann eine ganze Weile in Anspruch nehmen.
# * Erstelle eine Visualisierung des gesamten Kommunikatinosaufkommen, aufgelöst in 5 Minuten-Intervallen.
# * Erstelle eine Visualisierung der aktivsten Nutzer.
# * Erstelle eine Visualisierung der von diesen Nutzern am häufigsten genutzen Hashtags.


### --- Load libraries --- ###
library(tidyr)
library(dplyr)
library(rtweet)
library(ggplot2)
library(ggwordcloud)
library(plotly)


### --- Twitter Token Setup --- ###
token <- get_token()


### --- Save tweets as RDS --- ###
tweets <- search_tweets("MonacoGP", n = 1500)
saveRDS(tweets, "tweets.rds")

userTweets <- readRDS("tweets.rds") %>%
  mutate(no_of_tweets = 1) %>% 
  select(user_id) %>% 
  group_by(user_id) %>%
  mutate(tweets_per_user = n()) %>% 
  group_by(user_id, tweets_per_user) %>% 
  summarise() %>% 
  arrange(desc(tweets_per_user), .by_group = FALSE)


mostActive <- userTweets %>% 
  head(100)


### --- Save most active users' timeline as RDS --- ###
mostActiveTL <- get_timelines(mostActive$user_id, 
                              n = 1000, 
                              token = token, 
                              retryonratelimit = T)
saveRDS(mostActiveTL, "topUserTimelines.rds")

fullTimeline <- readRDS("topUserTimelines.rds")


### --- Save most used hashtags as RDS --- ###
hashtags <- fullTimeline[,"hashtags", with = FALSE] %>% 
  filter(hashtags != "NA") %>% 
  unnest(cols = c(hashtags)) %>%
  group_by(hashtags) %>% 
  mutate(no_hashtags = n())

topHashtags <- hashtags %>%
  group_by(hashtags, no_hashtags) %>% 
  summarise() %>% 
  arrange(desc(no_hashtags), .by_group = FALSE) %>%
  ungroup(hashtags) %>% 
  head(50)


### --- Plot all as charts --- ###

### --- Gesamtes Kommunikatinosaufkommen, aufgelöst in 5 Minuten-Intervallen. --- ###

chart1 <-
  ggplot(tweets) +
  geom_freqpoly(aes(created_at, 
                    color = is_retweet), 
                position = "dodge", 
                bins = 100) + 
  scale_x_datetime(date_breaks = "4 hours")+
  labs(
    title = "Kommunikatinosaufkommen zum Großer Preis von Monaco",
    x = "Zeit",
    y = "Anzahl Tweets",
    color = "Retweet"
    )

# Chart 1 Plotly
ggplotly(chart1, tooltip = "")


### --- Die aktivste Nutzer --- ###

filteredTimeline <- fullTimeline %>%
  filter(created_at > as.POSIXct("2021-05-01"))
  # filtered to avoid bins issue

chart2 <-
  ggplot(filteredTimeline) +
  geom_freqpoly(aes(created_at, 
                    color = is_retweet), 
                position = "dodge",
                bins = 50) +
  labs(
    title = "Timeline der aktivsten Nutzer",
    subtitle = "Die letzten 1000 Tweets der Nutzer",
    x = "Zeit",
    y = "Anzahl Tweets",
    color = "Retweet"
    )

# Chart 2 Plotly
ggplotly(chart2, tooltip = "")


### --- Häufig genutzen Hashtags --- ###

chart3 <- 
  ggplot(topHashtags, aes(label = hashtags, size = no_hashtags)) +
  geom_text_wordcloud_area(area_corr = TRUE, rm_outside = T, shape = "square", area_corr_power = 1) +
  scale_size_area(max_size = 100)
  

# Chart 3 Wordcloud
chart3
