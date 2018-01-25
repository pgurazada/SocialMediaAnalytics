library(Rfacebook)
library(rtweet)
library(caret)
library(tidyverse)

set.seed(20130810)
options(scipen=999)  # turn-off scientific notation like 1e+48
theme_set(theme_bw())
dev.new()

amulFB <- getPage(page = 'amul.coop', 
                  token = "EAACEdEose0cBALYoWNbtsAHZCKwsb5HPX2qZBl3Ah3DHkc5WJJNm7uNrZB25ZBjnEsQI04gg01MMlPBaFSE3ItExpQgOXg6jCqDr37jGMRQ640yvZATVyX88jATZCgdCV1i15aclHHO4WglhPHj6QjinLUsgdhSgVZAEjMGnPrxgQXITZC5fzXO2kViqut7P4NkZD", 
                  n = 200,
                  reactions = TRUE,
                  verbose = FALSE,
                  api = "v2.11")

dim(amulFB)
glimpse(amulFB)

amulTwitter <- get_timeline("Amul_Coop", n = 3200)

dim(amulTwitter)
glimpse(amulTwitter)

amulFB$created_time <- as.POSIXct(amulFB$created_time, 
                                  tz = "GMT",
                                  format = "%Y-%m-%dT%H:%M:%S+0000")
amulFB$month <- format(amulFB$created_time, "%Y-%m")
amulFB$year <- format(amulFB$created_time, "%Y")

glimpse(amulFB)

amulTwitter$postType <- sapply(amulTwitter$media_type, function(x) {unlist(x)})
glimpse(amulTwitter)

amulTwitter %>% group_by(postType) %>% summarize(N = n(), Freq = N/nrow(amulTwitter))
amulTwitter$photo <- ifelse(is.na(amulTwitter$postType), 0 , 1)
amulTwitter %>% group_by(photo) %>% summarize(N = n(), Freq = N/nrow(amulTwitter))

ggplot(data = amulTwitter) +
  geom_histogram(aes(favorite_count)) +
  labs(x = "Number of favorites",
       y = "Count",
       title = "Histogram of number of favorites received on Amul Twitter posts") 

ggplot(data = amulTwitter) +
  geom_histogram(aes(retweet_count)) +
  labs(x = "Number of rewtweets",
       y = "Count",
       title = "Histogram of number of rewteets of Amul Twitter posts")

ggplot(data = amulTwitter) + 
  geom_point(aes(x = created_at, y = retweet_count, color = factor(photo))) +
  labs(x = "Post Date",
       y = "Number of retweets", 
       title = "Distribution of retweets over time",
       subtitle = "(separated into photos and non-photos)",
       color = "Photo")

amulTwitterClean <- amulTwitter %>% select(created_at, text, favorite_count, retweet_count, photo)
amulTwitter$month <- format(amulTwitter$created_at, "%Y-%m")
amulTwitter$year <- format(amulTwitter$created_at, "%Y")

amulTwitterClean <- amulTwitter %>% select(created_at, month, year, text, favorite_count, retweet_count, photo)
View(amulTwitterClean)

