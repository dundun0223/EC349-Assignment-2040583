# Load the data and package
library(tidyverse)
library(flextable)
library(corrplot)
library(rpart)
library(ranger)
library(tm)
library(caret)

setwd("C:/Users/u2040583/EC349-Assignment (2040583)")
load("yelp_review_small.Rda")
load("yelp_user_small.Rda")

dat <- review_data_small %>%
  inner_join(user_data_small, by = "user_id") %>%
  select(c(4,8,19))

# Text analysis
corpus <- VCorpus(VectorSource(dat$text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords,stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
frequencies <- DocumentTermMatrix(corpus)
sparse <- removeSparseTerms(frequencies, 0.8)
reviewsSparse <- as.data.frame(as.matrix(sparse))
colnames(reviewsSparse) <- make.names(colnames(reviewsSparse))

reviewsSparse %>%
  head(n = 6) %>%
  flextable() %>%
  set_caption(caption = "Table 1: The data of reviwew sparse") %>%
  font(fontname = "Calibri (Body)", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  theme_booktabs() %>%
  autofit()

# Merge the data
dat <- cbind(dat[,c(1,3)], reviewsSparse)

dat %>%
  head(n = 6) %>%
  flextable() %>%
  set_caption(caption = "Table 2: The clean data of reviwew") %>%
  font(fontname = "Calibri (Body)", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  theme_booktabs() %>%
  autofit()

# Exploratory data analysis
ggplot(dat, aes(as.factor(stars), average_stars, fill = as.factor(stars))) +
  geom_boxplot() +
  labs(x = "stars", fill = "stars", title = "Figure1: The relationship between review stars and user average stars") +
  theme_bw()

corrplot(cor(dat[,-2]))

# Split the data
set.seed(1)
index <- sample(1:nrow(dat), 10000)
test_dat <- dat[index,]
train_dat <- dat[-index,]

ggplot(train_dat, aes(stars, fill = as.factor(stars))) +
  geom_bar(stat = "count", width = 0.5) +
  labs(title = "Fig 3: The barplot of stars(train)", 
       fill = "stars") + 
  theme_bw()

ggplot(test_dat, aes(stars, fill = as.factor(stars))) +
  geom_bar(stat = "count", width = 0.5) +
  labs(title = "Fig 4: The barplot of stars(test)", 
       fill = "stars") + 
  theme_bw()

# Build the model
mod <- ranger(as.factor(stars)~., train_dat)

pred <- predict(mod, test_dat)$predictions
cm <- confusionMatrix(factor(pred), factor(test_dat$stars), dnn = c("Prediction", "Reference"))
plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="white", high="#009194") +
  labs(x = "Prediction",y = "Reference") +
  scale_x_discrete(labels=c(5:1)) +
  scale_y_discrete(labels=c(1:5))
