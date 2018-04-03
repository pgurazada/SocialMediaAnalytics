#' ---
#' title: "Tidy text mining of tweets"
#' author: https://www.hvitfeldt.me/2018/03/binary-text-classification-with-tidytext-and-caret/
#' output: github_document
#' ---
#' last update: Tue Apr 03 14:33:41 2018
#' 

library(tidyverse)
library(tidytext)
library(caret)

set.seed(20130810)
theme_set(theme_minimal())

disaster_df <- read_csv("https://github.com/EmilHvitfeldt/blog/raw/master/data/socialmedia-disaster-tweets-DFE.csv")
glimpse(disaster_df)

#' The data is a bunch of tweets regarding accidents and some that oare not
#' relevant to accidents. 
#' 
#' The question we are trying to explore is:
#' Is there a predictable set of words that are used to describe disasters?
#' 
#' So, the dependent variable is whether the tweet was classified as a disaster 
#' and the predictor variables are the wordsin the tweet

disaster_clean_df <- disaster_df %>% filter(choose_one != "Can't Decide") %>% 
                                     mutate(id = `_unit_id`,
                                            disaster = choose_one == "Relevant",
                                            text = str_replace_all(text, " ?(f|ht)tp(s?)://(.*)[.][a-z]+", "")) %>% 
                                     select(id, disaster, text)

#' The regular expression takes out urls
#' 
#' The dependent variable is disaster

dev.new()

ggplot(disaster_clean_df) +
  geom_bar(aes(x = disaster))

#' Remove stop words
#' 

data_counts <- map_df(1:2, ~ unnest_tokens(disaster_clean_df, word, text, token = "ngrams", n = .x)) %>% 
               anti_join(stop_words, by = "word") %>% 
               count(id, word, sort = TRUE)
glimpse(data_counts)

#' Look at words that ppaear in at least 10 different tweets

words_10 <- data_counts %>% group_by(word) %>% 
                            summarize(n = n()) %>% 
                            filter(n >= 10) %>% 
                            select(word)

disaster_dtm <- data_counts %>% right_join(words_10, by = "word") %>% 
                                bind_tf_idf(word, id, n) %>% 
                                cast_dtm(id, word, tf_idf)
glimpse(disaster_dtm)

meta <- tibble(id = as.numeric(dimnames(disaster_dtm)[[1]])) %>%
        left_join(disaster_clean_df[!duplicated(disaster_clean_df$id), ], by = "id")

training_rows <- createDataPartition(meta$disaster, p = 0.8, list = FALSE, times = 1)

disaster_train_df <- disaster_dtm[training_rows, ] %>% as.matrix() %>% as.data.frame() %>% drop_na() 
disaster_test_df <- disaster_dtm[-training_rows, ] %>% as.matrix() %>% as.data.frame() %>% drop_na()

dim(disaster_train_df)

response_train <- meta$disaster[training_rows]
length(response_train)

svm_mod <- train(x = disaster_train_df, 
                 y = as.factor(response_train),
                 method = "svmLinearWeights2",
                 tuneGrid = data.frame(cost = 1, Loss = 0, weight = 1),
                 trControl = trainControl(method = "none"))

nb_mod <- train(x = disaster_train_df,
                y = as.factor(response_train),
                method = "naive_bayes",
                trControl = trainControl(method = "none"),
                tuneGrid = data.frame(laplace = 0,
                                      usekernel = FALSE,
                                      adjust = FALSE))
rf_mod <- train(x = disaster_train_df, 
                y = as.factor(response_train), 
                method = "ranger",
                trControl = trainControl(method = "none"),
                tuneGrid = data.frame(mtry = floor(sqrt(dim(disaster_train_df)[2])),
                                      splitrule = "gini",
                                      min.node.size = 1))
