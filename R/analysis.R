library(tidyverse)  # data wrangling tools
library(tidytext)   # bigrams
library(tm)         # text mining
library(wordcloud)  # word clouds

## ----- Load data -------------------------------------------------------------
dat <- read_csv("fake_survey.csv")
glimpse(dat)

# 1. how to load data sets
# 2. 

## ----- Summary tables --------------------------------------------------------






## ----- Word clouds -----------------------------------------------------------
corpus <- 
  Corpus(VectorSource(dat$q_limiting)) |>
  tm_map(content_transformer(tolower)) |>
  tm_map(removePunctuation) |>
  tm_map(removeNumbers) |>
  tm_map(removeWords, c(stopwords("en"), "just", "really", "get", "ive", "every")) |>
  tm_map(stripWhitespace)

# Stem words?
# corpus <- tm_map(corpus, stemDocument, language = "en")

tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
freq <- sort(rowSums(m), decreasing = TRUE)
word_freqs <- data.frame(word = names(freq), freq = freq)

set.seed(123)  # for reproducibility
wordcloud(
  words      = word_freqs$word,
  freq       = word_freqs$freq,
  min.freq   = 2,               # only words with freq >= 2
  max.words  = 100,             # draw up to 100 words
  random.order = FALSE,         # plot most frequent words in center
  colors     = RColorBrewer::brewer.pal(8, "Dark2")
)

## ----- Bigrams ---------------------------------------------------------------
bigram_counts <- 
  tibble(text = dat$q_limiting)  |>
  unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
  separate(bigram, into = c("word1", "word2"), sep = " ") |>
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) |>
  unite(bigram, word1, word2, sep = " ") |>
  count(bigram, sort = TRUE) |>
  filter(!bigram %in% c("wi fi")) |>
  mutate(bigram = str_replace_all(bigram, "wi fi", "wifi"))

bigram_counts$n[2] <- bigram_counts$n[1] + bigram_counts$n[2]  # Adjust a bit
bigram_counts <- bigram_counts[-1, ]

# combine certain bigrams
for (big in c("internet plan", "video call")) {
  idx <- which(grepl(big, bigram_counts$bigram))
  bigram_counts$n[idx[1]] <- sum(bigram_counts$n[idx])
  bigram_counts <- bigram_counts[-idx[-1], ]
}

wordcloud(
  words        = bigram_counts$bigram,
  freq         = bigram_counts$n,
  min.freq     = 2,
  max.words    = 100,
  random.order = FALSE,
  colors       = brewer.pal(8, "Dark2")
)