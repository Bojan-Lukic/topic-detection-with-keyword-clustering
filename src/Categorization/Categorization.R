rm(list=ls()) 

library(tidytext)
library(dplyr)
library(tidyr)
library(stringr)
library(tibble)
library(drlib)
library(ggplot2)
library(quanteda)
library(stm)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Load the previously built dataset
load("TidyWords.RData")

#Prepare the dataset for the idf-scoring algorithm
matrixwords_idf <- matrixwords %>%
  count(File, Word, sort = TRUE) %>%
  bind_tf_idf(Word, File, n) %>%
  arrange(-tf_idf) %>%
  group_by(File) %>%
  top_n(8) %>%
  ungroup

#The statistic tf-idf identifies words that are important to a document in a collection of documents.
#Here, a tf-idf score is computed for each word contained in the 20 text files
matrixwords_idf %>%
  mutate(Word = reorder_within(Word, tf_idf, File)) %>%
  ggplot(aes(Word, tf_idf, fill = File)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ File, scales = "free", ncol = 5) +
  scale_x_reordered() +
  coord_flip() +
  theme(strip.text = element_text(size = 11)) +
  labs(x = NULL, y = "tf-idf",
       title = "Highest tf-idf words in set of text data",
       subtitle = "Individual texts focus on different stories and elements")

#Create new data frame containing words sorted by tf-idf score from highest to lowest
matrixwordsSorted <- as.data.frame(matrixwords_idf[order(matrixwords_idf[, 6], decreasing = TRUE),])
matrixwordsSorted <- matrixwordsSorted[order(matrixwords_idf[, 1], decreasing = FALSE),]
save(matrixwordsSorted, file = "../Clustering/MatrixwordsSorted.RData")

matrixwordsSorted

#Prepare the dataset for the td-beta and td-gamma measure
matrixwords_dfm <- matrixwords %>%
  count(File, Word, sort = TRUE) %>%
  cast_dfm(File, Word, n)

matrixwords_sparse <- matrixwords %>%
  count(File, Word, sort = TRUE) %>%
  cast_sparse(File, Word, n)

#Creating a topic model with the stm package
topic_model <- stm(matrixwords_dfm, K = 6, 
                   verbose = FALSE, init.type = "Spectral")
#Tidy dataset for td-beta scoring
td_beta <- tidy(topic_model)
#Create new windows for plotting
dev.new(width = 15, height = 10, noRStudioGD = TRUE)

#td-beta shows the highest word probabilities for each topic. Different words are associated with
#different topics.
td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

#td-gamma shows the probability that each document is generated from each topic and which topics are 
#coming from which documents.
#In this case, each text is strongly associated with a single topic. 
td_gamma <- tidy(topic_model, matrix = "gamma",                    
                 document_names = rownames(matrixwords_dfm))

dev.new(width = 15, height = 10, noRStudioGD = TRUE)

ggplot(td_gamma, aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, ncol = 3) +
  labs(title = "Distribution of document probabilities for each topic",
       subtitle = "Each topic is associated with 1-3 texts",
       y = "Number of stories", x = expression(gamma))
