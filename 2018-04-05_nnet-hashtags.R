#' ---
#' title: "A nueral net for hash tags analysis in R "
#' author: https://web.stanford.edu/~pmohanty/kerasformula_barug.pdf
#' output: github_document
#' ---

#' last update: Thu Apr 05 15:11:43 2018
#' Replicate results from this recent presentation

library(kerasformula)
library(keras)
library(rtweet)
library(tidyverse)
library(caret)
library(feather)

theme_set(theme_minimal())

blackbuck_case <- search_tweets("#BlackBuckPoachingCase", 
                                n = 1e4, 
                                include_rts = FALSE, 
                                token = twitter_token)
str(blackbuck_case, max.level = 1)

counts <- blackbuck_case %>% select(favorite_count, retweet_count) %>% 
                             gather(Variable, count)

dev.new()
ggplot(counts) +
  geom_density(aes(x = log10(count+1), fill = Variable), alpha = 0.5) +
  labs(title = "Response to tweets on the black buck poaching case verdict")

blackbuck_case$popularity <- blackbuck_case$favorite_count + blackbuck_case$retweet_count

#' Since this is being implemented as a classification problem, the dependent 
#' variable will be discretized into approximate levels

breaks <- c(-1, 0, 1, 10, 100, 1000, 10000)

blackbuck_data <- blackbuck_case %>% select(popularity, screen_name, source,
                                            hashtags, mentions_screen_name, 
                                            urls_url, text, media_type, created_at) %>% 
                                     mutate(n_hashtags = lengths(hashtags),
                                            n_mentions_screen_name = lengths(mentions_screen_name),
                                            n_urls_url = lengths(urls_url),
                                            n_char_text = nchar(text), 
                                            is_photo = grepl('photo', media_type)) %>% 
                                     select(popularity, screen_name, source, 
                                            n_hashtags, n_mentions_screen_name,
                                            n_urls_url, n_char_text, is_photo)

summary(blackbuck_data)

pop_lm <- train(log(popularity + 1) ~ screen_name + source + n_hashtags + 
                                      n_mentions_screen_name + n_urls_url + 
                                      n_char_text + grepl('photo', media_type),
                data = blackbuck_data[1:1000, ],
                method = "lm",
                preProcess = c("center", "scale"),
                trControl = trainControl(method = "repeatedcv",
                                         number = 10,
                                         repeats = 1))

pop <- kms(cut(popularity, breaks) ~ screen_name + source + n_hashtags +
                                     n_mentions_screen_name + n_urls_url + 
                                     n_char_text + grepl('photo', media_type) +
                                     format(created_at, '%H'),
           blackbuck_data)
