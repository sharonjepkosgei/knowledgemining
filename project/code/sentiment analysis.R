rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014") 

library(tm)
library(NLP)
library(xml2)
library(stringr)
library(tidytext)
# Read CSV file
release <- read.csv("https://raw.githubusercontent.com/sharonjepkosgei/knowledgemining/main/project/data/ReleaseContent3.csv")
story <- read.csv("https://raw.githubusercontent.com/sharonjepkosgei/knowledgemining/main/project/data/StoryContent.csv")
# Filter data based on rating
fake_story <- story[story$rating < 3, ]
real_story <- story[story$rating >= 3, ]

fake_release <- release[release$rating < 3, ]
real_release <- release[release$rating >= 3, ]
#############################################################################
## Health Story Data
#############################################################################

# Create a list of texts and assign metadata
texts1 <- list(
  fake_story = as.character(fake_story$text),
  real_story = as.character(real_story$text)
)

# Create a vector of document labels
doc_labels1 <- c(
  rep("Fake Story", nrow(fake_story)),
  rep("Real Story", nrow(real_story))
)

# Combine all texts into a single character vector
all_texts1 <- unlist(texts1)

# Create a data frame to store the text data and metadata
corpus_df1 <- data.frame(
  text = all_texts1,
  doc_id = doc_labels1
)

# Create a Corpus object
library(tm)
corpus1 <- Corpus(DataframeSource(corpus_df1))

##Cleaning

#Change to lower case
corpus1 <- tm_map(corpus1,content_transformer(tolower))
# Remove punctuations
corpus1 <- tm_map(corpus1, removePunctuation)
# Eliminate extra white spaces
corpus1 <- tm_map(corpus1,stripWhitespace)
# Remove english common stopwords
corpus1 <- tm_map(corpus1, removeWords, stopwords("english"))

# Remove more words
words_to_remove <- c("she","this","these","said","one","and","are",
                     "have","last","with","this","that","such","when","been",
                     "says","will","also","where","why","would","but",
                     "now", "two", "they", "found", " like", "help", "was", "can", 
                     "more", "who", "new", "from", "their", "were", "years", 
                     "percent", "health", "used", "patients", "time", "people", "cancer")
corpus1 <- tm_map(corpus1, removeWords, words_to_remove)

# Extract text from the corpus
corpus_texts1 <- sapply(corpus1, as.character)

# Create a data frame with text and doc_id
corpus_data_frame1 <- data.frame(
  text = corpus_texts1,
  doc_id = corpus_df1$doc_id
)

# Change the data structure to "tibble"
story_text <- as_tibble(corpus_data_frame1)

# Unnest tokens function
story_tkn <- story_text %>%
  unnest_tokens(input = text,
                output = word,
                token = "words")

# Delete single-letter words
story_tkn <- story_tkn %>%
  filter(nchar(word) > 1)

# Adding Line Numbers to words df
story_tkn <- story_tkn %>%
  mutate(linenumber = row_number())
library(tidyr)
# Joining bing lexicon 
story_sentiment<- story_tkn%>%          
  inner_join(get_sentiments("bing")) %>%
  count(doc_id, index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#Sentiment Score Plot by Story Type
ggplot(story_sentiment, aes(index, sentiment, fill = doc_id)) +
  geom_col(show.legend = FALSE) + theme_bw() + 
  labs(y = "Sentiment Score Plot by Story Type", x = NULL)+
  facet_wrap(~doc_id,ncol = 1, scales = "free_x")+
  labs(title = "Sentiments Score plot by Story Type",
       x = NULL, y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))


bing_word_counts1 <- story_tkn%>%
  inner_join(get_sentiments("bing")) %>%
  count(doc_id, word, sentiment, sort = TRUE) %>%
  ungroup()


# Define a custom color 
custom_colors <- c("Fake Story" = "darkcyan", "Real Story" = "brown3")

bing_word_counts1 %>%
  group_by(sentiment, doc_id) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = doc_id)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = custom_colors) + 
  facet_wrap(~sentiment + doc_id, scales = "free_y", ncol = 2) +
  theme(text = element_text(family = "Palatino")) +
  coord_flip() +
  theme_bw() +
  labs(title = "Sentiments Word Analysis by Story Type",
       x = NULL, y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))

#############################################################################
## Health Release Data
#############################################################################

# Create a list of texts and assign metadata
texts2 <- list(
  fake_release = as.character(fake_release$text),
  real_release = as.character(real_release$text)
)

# Create a vector of document labels
doc_labels2 <- c(
  rep("Fake Release", nrow(fake_release)),
  rep("Real Release", nrow(real_release))
)

# Combine all texts into a single character vector
all_texts2 <- unlist(texts2)

# Create a data frame to store the text data and metadata
corpus_df2 <- data.frame(
  text = all_texts2,
  doc_id = doc_labels2
)

# Create a Corpus object
library(tm)
corpus2 <- Corpus(DataframeSource(corpus_df2))

##Cleaning

#Change to lower case
corpus2 <- tm_map(corpus2,content_transformer(tolower))
# Remove punctuations
corpus2 <- tm_map(corpus2, removePunctuation)
# Eliminate extra white spaces
corpus2 <- tm_map(corpus2,stripWhitespace)
# Remove english common stopwords
corpus2 <- tm_map(corpus2, removeWords, stopwords("english"))

# Remove more words
words_to_remove <- c("she","this","these","said","one","and","are",
                     "have","last","with","this","that","such","when","been",
                     "says","will","also","where","why","would","but",
                     "now", "two", "they", "found", "help", "was", "can", 
                     "more", "who", "new", "from", "their", "were", "years", 
                     "percent", "health", "used", "patients", "time", "people",
                     "cancer", "patient")
corpus2 <- tm_map(corpus2, removeWords, words_to_remove)

# Extract text from the corpus
corpus_texts2 <- sapply(corpus2, as.character)

# Create a data frame with text and doc_id
corpus_data_frame2 <- data.frame(
  text = corpus_texts2,
  doc_id = corpus_df2$doc_id
)

# Change the data structure to "tibble"
release_text <- as_tibble(corpus_data_frame2)

# Unnest tokens function
release_tkn <- release_text %>%
  unnest_tokens(input = text,
                output = word,
                token = "words")

# Delete single-letter words
release_tkn <- release_tkn %>%
  filter(nchar(word) > 1)

# Adding Line Numbers to words df
release_tkn <- release_tkn %>%
  mutate(linenumber = row_number())
library(tidyr)
# Joining bing lexicon using on average videos des of 100 words.
release_sentiment<- release_tkn%>%          
  inner_join(get_sentiments("bing")) %>%
  count(doc_id, index = linenumber %/% 100, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#Sentiment Score Plot by Release Type
ggplot(release_sentiment, aes(index, sentiment, fill = doc_id)) +
  geom_col(show.legend = FALSE) + theme_bw() + 
  labs(y = "Sentiment Score Plot by Release Type", x = NULL)+
  facet_wrap(~doc_id,ncol = 1, scales = "free_x")+
  labs(title = "Sentiments Score plot by Release Type",
       x = NULL, y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))


bing_word_counts2 <- release_tkn%>%
  inner_join(get_sentiments("bing")) %>%
  count(doc_id, word, sentiment, sort = TRUE) %>%
  ungroup()


# Define a custom color 
custom_colors <- c("Fake Release" = "darkcyan", "Real Release" = "brown3")

bing_word_counts2 %>%
  group_by(sentiment, doc_id) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = doc_id)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values = custom_colors) + 
  facet_wrap(~sentiment + doc_id, scales = "free_y", ncol = 2) +
  theme(text = element_text(family = "Palatino")) +
  coord_flip() +
  theme_bw() +
  labs(title = "Sentiments Word Analysis by Release Type",
       x = NULL, y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))

