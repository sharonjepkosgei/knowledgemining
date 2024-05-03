# Libraries
library(readxl)
library(tidyverse)
library(tm)
library(topicmodels)
library(tidytext)
library(reshape2)
library(dplyr)
install.packages("dplyr")
install.packages("reshape2")

# Read in data from Excel file
data <- read_excel("ReleaseContent3.xlsx")

# Split data into real and fake stories
fake_story <- data[data$rating < 3, ]
real_story <- data[data$rating >= 3, ]

# Define clean.text function
clean.text <- function(data) {
  # Clean the text data
  cleaned_data <- data %>%
    mutate(text = tolower(text)) %>%
    unnest_tokens(word, text) %>%
    filter(word != "â") %>%
    filter(word != "results") %>%
    filter(word != "people") %>%
    filter(word != "university") %>%
    filter(word != "research") %>%
    filter(word != "researchers") %>%
    filter(word != "percent") %>%
    filter(word != "patients") %>%
    filter(word != "dr") %>%
    filter(word != "drugs") %>%
    filter(word != "studies") %>%
    filter(word != "levels") %>%
    filter(word != "found") %>%
    filter(word != "day") %>%
    filter(word != "islands") %>%
    filter(word != "cancers") %>%
    filter(word != "lower") %>%
    filter(word != "time") %>%
    filter(word != "tests") %>%
    filter(word != "republic") %>%
    filter(word != "called") %>%
    filter(word != "cells") %>%
    filter(!grepl("\\d+", word)) %>% 
    anti_join(stop_words)
  return(cleaned_data)
}

# Clean text data for fake stories
cleaned_fake_data <- clean.text(fake_story)

# Clean text data for real stories
cleaned_real_data <- clean.text(real_story)

# Create Document-Term Matrix for fake stories
dtm_fake <- cleaned_fake_data %>%
  count(id, word) %>%
  cast_dtm(id, word, n)

# Create Document-Term Matrix for real stories
dtm_real <- cleaned_real_data %>%
  count(id, word) %>%
  cast_dtm(id, word, n)

# Topic Modeling for fake stories
lda_model_fake <- LDA(dtm_fake, k = 4, control = list(seed = 1234))

# Topic Modeling for real stories
lda_model_real <- LDA(dtm_real, k = 4, control = list(seed = 1234))

# Get topic-term probabilities for fake stories
topics_fake <- tidy(lda_model_fake, matrix = "beta")

# Get topic-term probabilities for real stories
topics_real <- tidy(lda_model_real, matrix = "beta")

# Get top terms per topic for fake stories
top_terms_fake <- topics_fake %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Get top terms per topic for real stories
top_terms_real <- topics_real %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# Perplexity function
get.perplexity <- function(DTM, k.max) {
  mtrx <- data.frame(topics = numeric(), perplexity = numeric())
  for (i in 2:k.max) {
    lda <- LDA(DTM, i, control = list(seed = 1234))  # Set seed for reproducibility
    per <- perplexity(lda)
    mtrx <- bind_rows(mtrx, data.frame(topics = i, perplexity = per))
  }
  return(mtrx)
}

# Get perplexity values for fake stories
perplexity_values_fake <- get.perplexity(dtm_fake, k.max = 15)

# Get perplexity values for real stories
perplexity_values_real <- get.perplexity(dtm_real, k.max = 15)

# Plot perplexity of different LDA models for fake stories
perplexity_values_fake %>%
  ggplot(aes(topics, perplexity)) +
  geom_line(colour = "navy", size = 1) +
  geom_point(colour = "navy", size = 2) +
  labs(title = "Perplexity of different LDA models (Fake Stories)",
       x = "Number of topics", 
       y = "Perplexity") +
  theme_minimal()

# Plot perplexity of different LDA models for real stories
perplexity_values_real %>%
  ggplot(aes(topics, perplexity)) +
  geom_line(colour = "navy", size = 1) +
  geom_point(colour = "navy", size = 2) +
  labs(title = "Perplexity of different LDA models (Real Stories)",
       x = "Number of topics", 
       y = "Perplexity") +
  theme_minimal()

# Visualize top terms per topic for fake stories
top_terms_fake %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#ff7f00", "deepskyblue4", "gold1", "navy")) +
  facet_wrap(~ topic, scales = "free") +
  labs(title = "Top 10 words per topic (Fake Stories)",
       y = "Beta") +
  coord_flip() +
  theme_minimal()

# Visualize top terms per topic for real stories
top_terms_real %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = c("#ff7f00", "deepskyblue4", "gold1", "navy")) +
  facet_wrap(~ topic, scales = "free") +
  labs(title = "Top 10 words per topic (Real Stories)",
       y = "Beta") +
  coord_flip() +
  theme_minimal()

# Analyzing Bigrams for fake stories
fake_story %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  filter(!grepl("â", word1) & !grepl("â", word2)) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE) %>%
  slice(1:10) %>%
  ggplot() +
  geom_bar(aes(bigram, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Bigrams (Fake Stories)",
       subtitle = "using Tidytext in R",
       caption = "Data Source: Excel File")

# Analyzing Bigrams for real stories
real_story %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word & !word2 %in% stop_words$word) %>%
  filter(!grepl("â", word1) & !grepl("â", word2)) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE) %>%
  slice(1:10) %>%
  ggplot() +
  geom_bar(aes(bigram, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Bigrams (Real Stories)",
       subtitle = "using Tidytext in R",
       caption = "Data Source: Excel File")
