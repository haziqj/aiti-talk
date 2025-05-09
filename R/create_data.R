library(tidyverse)
library(bruneimap)
library(ellmer)
hsp <- read_csv("https://raw.githubusercontent.com/Bruneiverse/house-data/refs/heads/master/data/hspbn_2025-03-03.csv")
mukims <- unique(hsp$mukim)

# hsp |>
#   summarise(price = mean(price), .by = c("mukim", "district")) |>
#   left_join(x = mkm_sf) |>
#   ggplot() +
#   geom_sf(aes(fill = price)) +
#   scale_fill_viridis_c()

# Settings
n <- 2000

# Demography
set_gender <- c(Male = 0.5, Female = 0.5)
set_age <- c(`15-24` = 0.11, `25-34` = 0.23, `35-44` = 0.31, `45-54` = 0.24, 
             `55-64` = 0.09)
set_region <- c(`Brunei-Muara` = 0.74, `Tutong` = 0.12, `Belait` = 0.12, 
                `Temburong` = 0.02)
set_educ <- c(
  `Primary School` = 0.02,
  `Lower Secondary` = 0.08,
  `O Level` = 0.2,
  `A Level` = 0.07,
  `National Certificate` = 0.04,
  `Diploma` = 0.02,
  `National Diploma` = 0.09,
  `Higher National Diploma` = 0.12,
  `Bachelor Degree` = 0.26,
  `Master Degree` = 0.1,
  `PhD` = 0.01
)

# Questions
q_limiting <- c(
  `Cost of services is too high` = 0.39,
  `One time registration and installation cost is too high` = 0.31,
  `Limited plan and insufficient data capacity` = 0.25,
  `The speed and quality in my areas is not up to expectation` = 0.24,
  `Limited data plan/subscriptions` = 0.21,
  `Have access to internet elsewhere` = 0.18,
  `Not value for money` = 0.15,
  `Not useful` = 0.14,
  `Concerns about internet (virus, spyware, phishing)` = 0.14,
  `Poor mobile network coverage and quality of calls` = 0.14,
  `Lack of knowledge and skills` = 0.13,
  `Internet coverage is not available in the area` = 0.09,
  `Complicated process of billing` = 0.08,
  `Devices does not support new technology` = 0.03
)

q_fbspeed <- c(
  `700-1000` = 0.08,
  `250-700` = 0.06,
  `100-250` = 0.31,
  `60-100` = 0.07,
  `50-60` = 0.27,
  `30-50` = 0.08,
  `10-30` = 0.15
)
q_fbusage <- c(
  `800-2000` = 0.23,
  `500-800` = 0.35,
  `250-500` = 0.19,
  `200-250` = 0.13,
  `1-200` = 0.10
)
q_fbexpend <- c(
  `10-50` = 0.19,
  `50-100` = 0.55,
  `100-150` = 0.13,
  `150-200` = 0.09,
  `200-250` = 0.02,
  `250-300` = 0.01,
  `300-800` = 0.01
)
q_fbqual <- c(
  `Excellent` = 0.10,
  `Very Good` = 0.28,
  `Good` = 0.34,
  `Fair` = 0.20,
  `Poor` = 0.06,
  `Very Poor` = 0.02
)
q_mbqual <- c(
  `Excellent` = 0.10,
  `Very Good` = 0.28,
  `Good` = 0.40,
  `Fair` = 0.17,
  `Poor` = 0.04,
  `Very Poor` = 0.01
)

# Function to sample from a categorical distribution
sample_cat <- function(setting, samp_size = n) {
  sample(names(setting), prob = setting, size = samp_size, replace = TRUE)
}

convert_cat_to_num <- function(categories) {
  lim <- str_split(categories, "-")[[1]]
  # runif(1, as.numeric(lim[1]), as.numeric(lim[2]))
  mu <- mean(as.numeric(lim))
  sd <- (as.numeric(lim[2]) - as.numeric(lim[1])) / (2 * qnorm(1-0.2/2))
  rnorm(1, mu, sd)
}


## ----- Create the data -------------------------------------------------------
set.seed(123)
dat <-
  list(
    gender = set_gender,
    age = set_age,
    district = set_region,
    education = set_educ,
    q_limiting = q_limiting,
    q_fbspeed = q_fbspeed,
    q_fbusage = q_fbusage,
    q_fbexpend = q_fbexpend,
    q_fbqual = q_fbqual,
    q_mbqual = q_mbqual
  ) |>
  map(sample_cat) |>
  bind_cols() |>
  mutate(id = seq_len(n), .before = gender)

dat <-
  dat |>
  mutate(
    age = as.integer(unlist(map(age, convert_cat_to_num))),
    q_fbspeed = as.integer(unlist(map(q_fbspeed, convert_cat_to_num))),
    q_fbexpend = as.integer(unlist(map(q_fbexpend, convert_cat_to_num))),
    q_fbusage = as.integer(unlist(map(q_fbusage, convert_cat_to_num)))
  )

# Induce correlation
z <- rnorm(n)
ifbexpend <- c(scale(0.7 * z + rnorm(n, sd = 0.2)))
ifbusage  <- c(scale(-0.9 * z + rnorm(n, sd = 0.1)))
cor(ifbexpend, ifbusage)

tau <- qnorm(cumsum(q_fbexpend))
fbexpend <- 
  cut(ifbexpend, breaks = c(-Inf, tau), labels = names(tau),
      right = FALSE, include.lowest = TRUE) |>
  map_dbl(convert_cat_to_num) |>
  as.integer()
  
tau <- qnorm(cumsum(q_fbusage))
fbusage <- 
  cut(ifbusage, breaks = c(-Inf, tau), labels = names(tau),
      right = FALSE, include.lowest = TRUE) |>
  map_dbl(convert_cat_to_num) |>
  as.integer()

tibble(x = fbusage, y = fbexpend) |>
  ggplot(aes(x, y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) 

dat <-
  dat |>
  mutate(district = factor(district, levels = c("Brunei-Muara", "Belait", 
                                                "Tutong", "Temburong"))) |>
  arrange(district) |>
  select(-q_fbexpend, -q_fbusage) |>
  bind_cols(
    tibble(
      q_fbexpend = fbexpend,
      q_fbusage = round(scales::rescale(fbusage, c(10, 1000)), -1)
    ) |>
      arrange(desc(fbexpend))
  )

set.seed(4)
dat <-
  dat |>
  select(-district) |>
  arrange(desc(fbexpend)) |>
  bind_cols(
    census2021 |>
      mutate(district = case_when(
        district == "Brunei Muara" ~ "Brunei-Muara",
        TRUE ~ district
      )) |>
      filter(mukim %in% mukims) |>
      select(kampong, mukim, district, population) |>
      slice_sample(n = n, weight_by = population, replace = TRUE) |>
      arrange(desc(population))
  )

dat |>
  summarise(expend = mean(q_fbexpend), .by = mukim) |>
  right_join(x = mkm_sf) |>
  ggplot() +
  geom_sf(data = brn_sf) +
  geom_sf(aes(fill = expend)) +
  scale_fill_viridis_c(option = "magma", labels = scales::dollar, name = "Mean\nmonthly\nexpenditure") +
  theme_minimal()


  
# Generate comments
res <- map_chr(dat$q_limiting, \(x) {
  chat <- chat_ollama(
    system_prompt = 'You are a survey comment generator.  
When given a single short phrase describing a respondent’s top limiting factor for their internet access, you must return **one** brief, natural-sounding user comment (in first person) that a real person might write in a survey.  

Requirements:
- Output exactly one sentence.
- Use a casual, conversational tone.
- Mention the factor in context (“I avoid… because…”, “It’s hard when…”, etc.).
- Do not add any extra text, bullet points, or explanations.
- No need to include quotation marks at the beginning and end of sentence.

Example:
Input: Concerns about internet (virus, spyware, phishing)
Output: I barely go online because I’m always worried about downloading a virus or spyware onto my laptop.

Now, given the factor label after the prompt, generate a matching user comment.',
    model = "llama3.2"
  )  
  chat$chat(x, echo = FALSE)
}, .progress = TRUE)
dat$q_limiting2 <- res

# Cleanup
dat <- 
  arrange(dat, id) |>
  select(-population, -q_limiting) |>
  rename(q_limiting = q_limiting2) |>
  select(id, kampong, mukim, district, everything())
write_csv(dat, file = "fake_survey.csv")

# Wordcloud
library(tm)
library(wordcloud)
library(RColorBrewer)
corpus <- Corpus(VectorSource(dat$q_limiting))

# 4. Clean the text
corpus <- tm_map(corpus, content_transformer(tolower))       
corpus <- tm_map(corpus, removePunctuation)                  
corpus <- tm_map(corpus, removeNumbers)                      
corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "just", "really", "get", "ive", "every"))
corpus <- tm_map(corpus, stripWhitespace)     
# corpus <- tm_map(corpus, stemDocument, language = "en")

# 5. Create a term‐document matrix and get word frequencies
tdm <- TermDocumentMatrix(corpus)
m   <- as.matrix(tdm)
freq <- sort(rowSums(m), decreasing = TRUE)
word_freqs <- data.frame(word = names(freq), freq = freq)

# 6. Plot the word cloud
set.seed(123)  # for reproducibility
wordcloud(
  words      = word_freqs$word,
  freq       = word_freqs$freq,
  min.freq   = 2,               # only words with freq >= 2
  max.words  = 100,             # draw up to 100 words
  random.order = FALSE,         # plot most frequent words in center
  colors     = brewer.pal(8, "Dark2")
)

# Bigrams
library(tidytext)

bigram_counts <- 
  tibble(text = dat$q_limiting)  |>
  unnest_tokens(bigram, text, token = "ngrams", n = 2) |>
  separate(bigram, into = c("word1", "word2"), sep = " ") |>
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word) |>
  unite(bigram, word1, word2, sep = " ") |>
  count(bigram, sort = TRUE) |>
  filter(!bigram %in% c("wi fi")) |>
  mutate(bigram = str_replace_all(bigram, "wi fi", "wifi"))

# Adjust a bit
bigram_counts$n[2] <- bigram_counts$n[1] + bigram_counts$n[2]
bigram_counts <- bigram_counts[-1, ]

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
