rm(list=ls())                          # Clear environment
oldpar <- par()                        # save default graphical parameters
if (!is.null(dev.list()["RStudioGD"])) # Clear plot window
  dev.off(dev.list()["RStudioGD"])   
cat("\014")                            # Clear the Console

#install.packages("easypackages")
library(easypackages)

# Load multiple packages using easypackage function "packages"
packages("XML","wordcloud","RColorBrewer","readr","NLP","tm","quanteda", prompt = T)

library(tm)
library(xml2)
library(stringr)
library(dplyr)
library(tidytext)

# Read CSV file
release <- read.csv("https://raw.githubusercontent.com/sharonjepkosgei/knowledgemining/main/project/data/ReleaseContent3.csv")
story <- read.csv("https://raw.githubusercontent.com/sharonjepkosgei/knowledgemining/main/project/data/StoryContent.csv")
# Filter data based on rating
fake_story <- story[story$rating < 3, ]
real_story <- story[story$rating >= 3, ]

fake_release <- release[release$rating < 3, ]
real_release <- release[release$rating >= 3, ]

# Extract the 'text' column from the CSV as a character vector
fakes <- fake_story$text
Corpus <- Corpus(VectorSource(fakes))

##Cleaning

#Change to lower case
corpus <- tm_map(Corpus,content_transformer(tolower))
# Remove punctuations
corpus <- tm_map(Corpus, removePunctuation)

# Eliminate extra white spaces
corpus <- tm_map(Corpus,stripWhitespace)
# Remove english common stopwords
corpus <- tm_map(Corpus, removeWords, stopwords("english"))
# Remove more words
words_to_remove <- c("she","this","these","said","one","and","are",
                     "have","last","with","this","that","such","when","been",
                     "says","will","also","where","why","would","but",
                     "now", "two", "they", "not", " the", "for", "was", "can", 
                     "more", "who", "from", "their", "were", "patients", 
                     "people")
corpus <- tm_map(Corpus, removeWords, words_to_remove)

# Check the final result
inspect(corpus[[1]])

#Making a DTM matrix
dtm <- DocumentTermMatrix(corpus)
dtm2 <- as.matrix(dtm)

# Save as data
df_clean <- data.frame(text = sapply(corpus,as.character),
                       stringsAsFactors = FALSE)

#change the data structure to "tibble"
storytext_df<-as_tibble(df_clean$text)
storytext_df

#unnest_tokens Function
storytext_token<-storytext_df %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")

# Delete single-letter and two-letter words
storytext_token<-storytext_token %>%
  filter(str_count(word)>2)

# Frequency analysis
word_space<-storytext_token %>%
  count(word, sort = TRUE)
word_space

# top 20 words
top20<-word_space %>%
  filter(word !="just") %>%
  filter(word !="the") %>%
  filter(word !="can") %>%
  filter(word !="has") %>%
  filter(word !="which") %>%
  filter(word !="about") %>%
  filter(word !="could") %>%
  filter(word !="had") %>%
  filter(word !="than") %>%
  filter(word !="new") %>%
  filter(word !="those") %>%
  filter(word !="you") %>%
  filter(word !="after") %>%
  filter(word !="than") %>%
  filter(word !="may") %>%
  filter(word !="some") %>%
  filter(word !="all") %>%
  filter(word !="but") %>%
  filter(word !="after") %>%
  filter(word !="found") %>%
  filter(word !="her") %>%
  filter(word !="other") %>%

  head(13)
top20


#make bar graph
library(ggplot2)

ggplot(top20, aes(x=reorder(word, n), y=n)) +
  geom_col(fill = "darkcyan") +  
  coord_flip() +
  geom_text(aes(label=n), hjust=-0.01, color = "black") +
  labs(title = "Frequency of Words in Fake Story",
       x="n", y=NULL) +
  theme(title=element_text(size=12))

###############################################################################
## Real Story
###############################################################################
# Extract the 'text' column from the CSV as a character vector
reals <- real_story$text
Corpus2 <- Corpus(VectorSource(reals))

##Cleaning

#Change to lower case
corpus2 <- tm_map(Corpus2,content_transformer(tolower))
# Remove punctuations
corpus2 <- tm_map(Corpus2, removePunctuation)

# Eliminate extra white spaces
corpus2 <- tm_map(Corpus2,stripWhitespace)
# Remove english common stopwords
corpus2 <- tm_map(Corpus2, removeWords, stopwords("english"))
# Remove more words
words_to_remove <- c("she","this","these","said","one","and","are",
                     "have","last","with","this","that","such","when","been",
                     "says","will","also","where","why","would","but",
                     "now", "two", "they", "not", " the", "for", "was", "can", 
                     "more", "who", "from", "their", "were", "patients", "people")
corpus2 <- tm_map(Corpus2, removeWords, words_to_remove)

# Check the final result
inspect(corpus2[[1]])



# Save as data
df_clean2 <- data.frame(text = sapply(corpus2,as.character),
                       stringsAsFactors = FALSE)

#change the data structure to "tibble"
storytext_df2<-as_tibble(df_clean2$text)
storytext_df2

#unnest_tokens Function
storytext_token2<-storytext_df2 %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")

# Delete single-letter words
storytext_token2<-storytext_token2 %>%
  filter(str_count(word)>2)

# Frequency analysis
word_space2<-storytext_token2 %>%
  count(word, sort = TRUE)
word_space2

# top 20 words
top20_real_story<-word_space2 %>%
  filter(word !="just") %>%
  filter(word !="the") %>%
  filter(word !="can") %>%
  filter(word !="has") %>%
  filter(word !="which") %>%
  filter(word !="about") %>%
  filter(word !="could") %>%
  filter(word !="had") %>%
  filter(word !="than") %>%
  filter(word !="new") %>%
  filter(word !="those") %>%
  filter(word !="you") %>%
  filter(word !="after") %>%
  filter(word !="than") %>%
  filter(word !="may") %>%
  filter(word !="some") %>%
  filter(word !="all") %>%
  filter(word !="but") %>%
  filter(word !="after") %>%
  filter(word !="found") %>%
  filter(word !="there") %>%
  filter(word !="most") %>%
  filter(word !="her") %>%
  filter(word !="other") %>%
  filter(word !="percent") %>%
  
  head(13)
top20_real_story


#make bar graph
library(ggplot2)

ggplot(top20_real_story, aes(x=reorder(word, n), y=n)) +
  geom_col(fill = "brown3") +  
  coord_flip() +
  geom_text(aes(label=n), hjust=-0.01, color = "black") +
  labs(title = "Frequency of Words in Real Story",
       x="n", y=NULL) +
  theme(title=element_text(size=12))


#compare all 2 types

keyword1<-df_clean %>%
  as_tibble() %>%
  mutate(type="Fake Story")

keyword2<-df_clean2 %>%
  as_tibble() %>%
  mutate(type="Real Story")


#bind rows
bind_keywords<- bind_rows(keyword1, keyword2) %>%
  select(text, type)
head(bind_keywords)

#unnest_tokens Function
bind_token<-bind_keywords %>%
  unnest_tokens(input = text,
                output = word,
                token = "words")
bind_token

# Frequency each keywords
frequency<-bind_token %>%
  count(type, word) %>%
  filter(str_count(word)>3)
head(frequency)

keytop20 <- frequency %>%
  filter(word !="could") %>%
  filter(word !="will") %>%
  filter(word !="which") %>%
  filter(word !="just") %>%
  filter(word !="some") %>%
  filter(word !="there") %>%
  filter(word !="like") %>%
  filter(word !="about") %>%
  filter(word !="know") %>%
  filter(word !="after") %>%
  filter(word !="also") %>%
  filter(word !="than") %>%
  filter(word !="those") %>%
  filter(word !="found") %>%
  filter(word !="other") %>%
  filter(word !="said") %>%
  filter(word !="many") %>%
  filter(word !="going") %>%
  filter(word !="think") %>%
  filter(word !="percent") %>%
  filter(word !="year") %>%
  filter(word !="them") %>%
  filter(word !="years") %>%
  filter(word !="used") %>%
  
  group_by(type) %>%
  slice_max(n, n=20, with_ties = F)
keytop20

# make compare Freq plot
ggplot(keytop20, aes(x=reorder_within(word, n, type),
                     y=n,
                     fill= type)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~type, scales = "free_y") +
  scale_x_reordered() +
  labs(x=NULL) +
  theme(text = element_text(size = 10))

#--------------------------------------------------------------
# TF-IDF
library(tidytext)
keytop20<- keytop20 %>%
  bind_tf_idf(term = word,
              document = type,
              n = n) %>%
  arrange(-tf_idf)
keytop20

# tf_idf for "fake"
keytop20 %>% filter(type == "Fake Story")
# tf_idf for "real"
keytop20 %>% filter(type == "Real Story")


#make order the graph
TF<- keytop20 %>%
  group_by(type) %>%
  slice_max(tf, n = 13, with_ties = F)

TF$type <- factor(TF$type,
                       levels = c("Fake Story", "Real Story"))

# make bar plot
ggplot(TF, aes(x=reorder_within(word, tf, type),
                  y=tf,
                  fill = type)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~type, scales = "free", ncol = 10) +
  scale_x_reordered() +
  labs(title = "Term Frequency by Story Type", x = NULL)

#----------weighted log odds
#install.packages("tidylo")
library(tidylo)
keytop20

bigram_log_odds <- keytop20 %>%
  bind_log_odds(type, word, n) 

bigram_log_odds %>%
  arrange(-log_odds_weighted)

# make plot by using log odds
bigram_log_odds %>%
  group_by(type) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, log_odds_weighted)) %>%
  ggplot(aes(word, log_odds_weighted, fill = type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~type, scales = "free") +
  coord_flip() +
  labs(title = "Weighted log-odds comparison by story type", x = NULL)

#---------------------------------------------------------------
## Release Data
#---------------------------------------------------------------
# Extract the 'text' column from the CSV as a character vector
fake_r <- fake_release$text
Corpus3 <- Corpus(VectorSource(fake_r))

##Cleaning

#Change to lower case
corpus3 <- tm_map(Corpus3,content_transformer(tolower))
# Remove punctuations
corpus3 <- tm_map(corpus3, removePunctuation)
# Eliminate extra white spaces
corpus3 <- tm_map(corpus3,stripWhitespace)
# Remove english common stopwords
corpus3 <- tm_map(corpus3, removeWords, stopwords("english"))

# Remove more words
words_to_remove <- c("she","this","these","said","one","and","are",
                     "have","last","with","this","that","such","when","been",
                     "says","will","also","where","why","would","but",
                     "now", "two", "they", "not", " the", "for", "was", "can", 
                     "more", "who", "found", "from", "their", "were", "patients", 
                     "people", "health")
corpus3 <- tm_map(corpus3, removeWords, words_to_remove)
# Check the final result
inspect(corpus3[[1]])


#Making a DTM matrix
dtm <- DocumentTermMatrix(corpus3)
dtm2 <- as.matrix(dtm)

# Save as data
df_clean3 <- data.frame(text = sapply(corpus3,as.character),
                       stringsAsFactors = FALSE)

#change the data structure to "tibble"
releasetext_df<-as_tibble(df_clean3$text)
releasetext_df

#unnest_tokens Function
releasetext_token<-releasetext_df %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")

# Delete single-letter and two-letter words
releasetext_token<-releasetext_token %>%
  filter(str_count(word)>2)

# Frequency analysis
word_space3<-releasetext_token %>%
  count(word, sort = TRUE)
word_space3 %>%
  
  head(13)

# top 20 words
top_fake_release<-word_space3 %>%
  filter(word !="may") %>%
  filter(word !="results") %>%
  filter(word !="found") %>%
  head(13)
top_fake_release


#make bar graph
library(ggplot2)

ggplot(top_fake_release, aes(x=reorder(word, n), y=n)) +
  geom_col(fill = "darkcyan") +  
  coord_flip() +
  geom_text(aes(label=n), hjust=-0.01, color = "black") +
  labs(title = "Frequency of Words in Fake Health Release",
       x="n", y=NULL) +
  theme(title=element_text(size=12))

###############################################################################
## Real Release
###############################################################################
# Extract the 'text' column from the CSV as a character vector
real_r <- real_release$text
corpus4 <- Corpus(VectorSource(real_r))

##Cleaning

#Change to lower case
corpus4 <- tm_map(corpus4,content_transformer(tolower))
# Remove punctuations
corpus4 <- tm_map(corpus4, removePunctuation)

# Eliminate extra white spaces
corpus4 <- tm_map(corpus4,stripWhitespace)
# Remove english common stopwords
corpus4 <- tm_map(corpus4, removeWords, stopwords("english"))
# Remove more words
words_to_remove <- c("she","this","these","said","one","and","are",
                     "have","last","with","this","that","such","when","been",
                     "says","will","also","where","why","would","but",
                     "now", "two", "they", "not", " the", "for", "was", "can", 
                     "more", "who", "from", "their", "were", "patients", 
                     "people", "may")
corpus4 <- tm_map(corpus4, removeWords, words_to_remove)
# Check the final result
inspect(corpus4[[1]])

#Making a DTM matrix
dtm <- DocumentTermMatrix(corpus4)
dtm2 <- as.matrix(dtm)

#Finding the most frequent items
frequency <- colSums(dtm2)
frequency <- sort(frequency, decreasing=TRUE)
head(frequency)

# Save as data
df_clean4 <- data.frame(text = sapply(corpus4,as.character),
                        stringsAsFactors = FALSE)

#change the data structure to "tibble"
releasetext_df2<-as_tibble(df_clean4$text)

#unnest_tokens Function
releasetext_token2<-releasetext_df2 %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")

# Delete single-letter words
releasetext_token2<-releasetext_token2 %>%
  filter(str_count(word)>2)

# Frequency analysis
word_space4<-releasetext_token2 %>%
  count(word, sort = TRUE)
word_space4

# top 20 words
top20_real_release<-word_space4 %>%
  head(13)
top20_real_release


#make bar graph
library(ggplot2)

ggplot(top20_real_release, aes(x=reorder(word, n), y=n)) +
  geom_col(fill = "brown3") +  
  coord_flip() +
  geom_text(aes(label=n), hjust=-0.01, color = "black") +
  labs(title = "Frequency of Words in Real Health Release",
       x="n", y=NULL) +
  theme(title=element_text(size=12))


#compare all 2 types

keyword3<-df_clean3 %>%
  as_tibble() %>%
  mutate(type="Fake Release")

keyword4<-df_clean4 %>%
  as_tibble() %>%
  mutate(type="Real Release")


#bind rows
bind_keywords<- bind_rows(keyword3, keyword4) %>%
  select(text, type)
head(bind_keywords)

#unnest_tokens Function
bind_token<-bind_keywords %>%
  unnest_tokens(input = text,
                output = word,
                token = "words")
bind_token

# Frequency each keywords
frequency<-bind_token %>%
  count(type, word) %>%
  filter(str_count(word)>3)
head(frequency)

keytop20 <- frequency %>%
  group_by(type) %>%
  slice_max(n, n=20, with_ties = F)
keytop20

# make compare Freq plot
ggplot(keytop20, aes(x=reorder_within(word, n, type),
                     y=n,
                     fill= type)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~type, scales = "free_y") +
  scale_x_reordered() +
  labs(x=NULL) +
  theme(text = element_text(size = 10))

#--------------------------------------------------------------
# TF-IDF
library(tidytext)
keytop20<- keytop20 %>%
  bind_tf_idf(term = word,
              document = type,
              n = n) %>%
  arrange(-tf_idf)
keytop20

# tf_idf for "fake"
keytop20 %>% filter(type == "Fake Release")
# tf_idf for "real"
keytop20 %>% filter(type == "Real Release")


#make order the graph
TF<- keytop20 %>%
  group_by(type) %>%
  slice_max(tf, n = 13, with_ties = F)

TF$type <- factor(TF$type,
                  levels = c("Fake Release", "Real Release"))

# make bar plot
ggplot(TF, aes(x=reorder_within(word, tf, type),
               y=tf,
               fill = type)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~type, scales = "free", ncol = 10) +
  scale_x_reordered() +
  labs(title = "Term Frequency by Health Release Type", x = NULL)

#----------weighted log odds
#install.packages("tidylo")
library(tidylo)
keytop20

bigram_log_odds <- keytop20 %>%
  bind_log_odds(type, word, n) 

bigram_log_odds %>%
  arrange(-log_odds_weighted)

# make plot by using log odds
bigram_log_odds %>%
  group_by(type) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, log_odds_weighted)) %>%
  ggplot(aes(word, log_odds_weighted, fill = type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~type, scales = "free") +
  coord_flip() +
  labs(title = "Weighted log-odds comparison by Health Release type", x = NULL)

#---------------------------------------------------------------
