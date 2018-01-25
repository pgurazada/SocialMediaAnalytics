library(caret)
library(tidyverse)

dev.new()
options(digits = 2)
set.seed(20130810)

amul <- read_csv("Data/amul.coop_facebook_statuses.csv")
glimpse(amul)

cor(amul[, c("num_comments", "num_shares", "num_likes")])

plot(density(amul$num_likes), main = "Likes")
plot(density(amul$num_shares), main = "Shares")

hist(amul$num_comments, main = "Comments")
head(amul$status_message)

amul %>% group_by(status_type) %>% 
         summarize(N = n()) %>% 
         mutate(Freq = N/sum(N))

# Lets build a regression model

trainingRows <- createDataPartition(amul$status_type, p = 0.7, list = FALSE)
amulTrain <- amul[trainingRows, ]
amulTest <- amul[-trainingRows, ]

amulTrain %>% group_by(status_type) %>% 
              summarize(N = n()) %>% 
              mutate(freq = N/sum(N))

amulTest %>% group_by(status_type) %>% 
             summarize(N = n()) %>% 
             mutate(freq = N/sum(N))

# Horrible model

amulModel0 <- train(num_shares ~ status_type,
                    data = amulTrain,
                    method = "lm",
                    trControl = trainControl(method = "cv", number = 10)) 

summary(amulModel0)
amulModel0$results

# Less horrible

amulModel1 <- train(num_shares ~ status_type + num_likes,
                    data = amulTrain,
                    method = "lm",
                    trControl = trainControl(method = "cv", number = 10))
summary(amulModel1)
amulModel1$results

amulTrain %>% filter(num_shares == 0) %>% count()
amulTrain %>% filter(num_likes == 0) %>% count()
amulTrain %>% filter(num_comments == 0) %>% count()

# Much better

amulModel2 <- train(log(num_shares) ~ status_type + log(num_likes),
                    data = subset(amulTrain, num_shares != 0),
                    method = "lm",
                    trControl = trainControl(method = "cv", number = 10))
summary(amulModel2)
amulModel2$results

# Lets build a classification model

amul$sharesGt1000 <- ifelse(amul$num_shares > 1000, 1, 0)

trainingRows <- createDataPartition(amul$status_type, p = 0.7, list = FALSE)

amulTrain <- amul[trainingRows, ]
amulTest <- amul[-trainingRows, ]

amulModel3 <- train(factor(sharesGt1000) ~ status_type + log(num_likes),
                    data = subset(amulTrain, num_shares != 0),
                    method = "glm", 
                    trControl = trainControl(method = "cv", number = 10))
summary(amulModel3)
confusionMatrix(amulModel3)

