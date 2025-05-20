## ----- Some basics --------------------------------------------------------
my_string <- "Hello, World!"
print(my_string)

# Create a vector, manipulate it
x <- c(1, 2, 3, 4, 5)
sum(x) / length(x)
for (i in x) print(i)

# ----- Load library ---------------------------------------------------
library(tidyverse)  # data wrangling tools
library(tinyplot)   # for quick plotting
library(tidytext)   # bigrams
library(tm)         # text mining
library(wordcloud)  # word clouds
library(gtsummary)  # pretty summary tables
library(bruneimap)  # for mapping

theme_set(theme_bw())  # ggplot2
tinytheme("clean2")    # tinyplot

## ----- Import data -------------------------------------------------------------
dat <- read_csv("R/fake_survey.csv")
glimpse(dat)

## ----- Transform data ----------------------------------------------------------
dat <-
  dat |>
  mutate(
    gender = factor(gender, levels = c("Male", "Female")),
    # Convert education to factors
    education = factor(education, levels = c(
      "Primary School", "Lower Secondary", "O Level", "A Level", 
      "National Certificate", "Diploma", "National Diploma", 
      "Higher National Diploma", "Bachelor Degree", "Master Degree", "PhD"
    )),
    # Convert Likert scale to ordered factors
    across(c(q_mbqual, q_fbqual), function(x) ordered(x, levels = c(
      "Very Poor", "Poor", "Fair", "Good", "Very Good", "Excellent"
    )))
  )

glimpse(dat)
head(as.numeric(dat$q_mbqual), 15)
head(as.numeric(dat$kampong), 15)

## ----- Variability -----------------------------------------------------------------------
N <- 200
bind_rows(
  tibble(x = scales::rescale(rbeta(N, 5, 2), c(1, 5)), case = "Variability present"),
  tibble(x = rep(3, N), case = "No variability")
) |>
  ggplot(aes(x, y = 1)) +
  geom_violin(fill = "red3", alpha = 0.15, col = NA) +
  geom_jitter(height = 0.03, width = 0, alpha = 0.5) +
  facet_grid(case ~ .) +
  theme_bw() +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  coord_cartesian(ylim = c(0.5, 1.5), xlim = c(1, 5)) +
  labs(title = "How satisfied are you with the quality of your fixed broadband connection?",
       x = NULL) 

## ----- Summary statistics: AGE -------------------------------------------------------------------------
x <- dat$age
head(x)

# Mean and standard deviation
mean(x)
sd(x)

# Quick summary of the data
summary(x)


## ------ Box plot --------------------------------------------------------------
boxplot(x, horizontal = TRUE, main = "Boxplot of Age", ylab = "Age", 
        col = "lightblue")


## ----- Histogram -------------------------------------------------------
hist(x, main = "Histogram of Age", xlab = "Age", ylab = "Frequency", 
     col = "lightblue", breaks = 10)


## ----- Histograms and density plots ------------------------------------
hist(x, main = "Histogram of Age with density overlaid", xlab = "Age", 
     ylab = "Density", col = "lightblue", breaks = 10, prob = TRUE)
lines(density(x), lwd = 3, col = "red3")

## ----- Summary statistics: GENDER -------------------------------------------------
x <- dat$gender
head(x)

# No such thing as the 'mean' of character vectors!
mean(x)

# Instead, do this:
table(x)
prop.table(table(x))

# If you're fancy:
chisq.test(table(x))


## ----- Bar plot -------------------------------------------------------
x <- dat$education
barplot(table(x), las = 2, cex.names = 0.8, main = "Barplot of Education", 
        ylab = "Frequency", col = "lightblue")


## ----- Scatter plot ----------------------------------------------------
plot(q_fbexpend ~ q_fbusage, data = dat,
     main = "Monthly expenditure vs data usage", 
     xlab = "Data usage (GB)", ylab = "Monthly expenditure (BND)")


## ----- Correlation ----------------------------------------------
cor(dat$q_fbexpend, dat$q_fbusage) 

# plot
# install.packages("mvtnorm")
set.seed(221222)
n <- 50
dat1 <- mvtnorm::rmvnorm(n, sigma = matrix(c(1, 1, 1, 1), ncol = 2))
dat2 <- mvtnorm::rmvnorm(n, sigma = matrix(c(1, 0.8, 0.8, 1), ncol = 2))
dat3 <- mvtnorm::rmvnorm(n, sigma = matrix(c(1, 0.5, 0.5, 1), ncol = 2))
dat4 <- mvtnorm::rmvnorm(n, sigma = matrix(c(1, 0.2, 0.2, 1), ncol = 2))
dat5 <- mvtnorm::rmvnorm(n, sigma = matrix(c(1, 0, 0, 1), ncol = 2))
dat6 <- mvtnorm::rmvnorm(n, sigma = matrix(c(1, -0.2, -0.2, 1), ncol = 2))
dat7 <- mvtnorm::rmvnorm(n, sigma = matrix(c(1, -0.8, -0.8, 1), ncol = 2))
dat8 <- mvtnorm::rmvnorm(n, sigma = matrix(c(1, -1, -1, 1), ncol = 2))
plot.df <- rbind(
  data.frame(dat1, rho = 1),
  data.frame(dat2, rho = 2),
  data.frame(dat3, rho = 3),
  data.frame(dat4, rho = 4),
  data.frame(dat5, rho = 5),
  data.frame(dat6, rho = 6),
  data.frame(dat7, rho = 7),
  data.frame(dat8, rho = 8)
)
plot.df$rho <- factor(plot.df$rho)
levels(plot.df$rho)[1] <- expression(rho * " = 1")
levels(plot.df$rho)[2] <- expression(rho * " = 0.8")
levels(plot.df$rho)[3] <- expression(rho * " = 0.5")
levels(plot.df$rho)[4] <- expression(rho * " = 0.2")
levels(plot.df$rho)[5] <- expression(rho * " = 0")
levels(plot.df$rho)[6] <- expression(rho * " = -0.2")
levels(plot.df$rho)[7] <- expression(rho * " = -0.8")
levels(plot.df$rho)[8] <- expression(rho * " = -1")

ggplot(plot.df, aes(x = X1, y = X2, group = rho)) +
  geom_point(size = 1.1) +
  facet_wrap(. ~ rho, nrow = 2, scales = "free", labeller = label_parsed) +
  labs(x = "X", y = "Y") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) 


## ------ Linear regression ----------------------------------------------
fit <- lm(q_fbexpend ~ q_fbusage, data = dat)
summary(fit)


## ----- scatter plot cont. ----------------------------------------------
abline(fit, col = "red3", lwd = 2)

plot(q_fbexpend ~ q_fbusage, data = dat,
     main = "Monthly expenditure vs data usage", 
     xlab = "Data usage (GB)", ylab = "Monthly expenditure (BND)")
abline(fit, col = "red3", lwd = 2)


## ----- Five-number summary by group ------------------------------------
by(dat$q_fbexpend, dat$gender, summary)
boxplot(q_fbexpend ~ gender, dat, range = 5, col = "lightblue", horizontal = TRUE,
        ylab = NULL, xlab = NULL, main = "Monthly expenditure (BND)")


## ----- Contingency tables ----------------------------------------------
tab1 <- table(dat$gender, dat$q_fbqual)
print(tab1)
tab2 <- prop.table(tab1, margin = 1)  # row proportions
round(tab2, 2)
chisq.test(tab1)


## ----- Side-by-side bar charts -----------------------------------------
dat |>
  pivot_longer(c(q_fbqual, q_mbqual), names_to = "qual_type", 
               values_to = "quality") |>
  mutate(qual_type = recode(qual_type, q_fbqual = "Broadband", 
                            q_mbqual = "Mobile")) |>
  ggplot(aes(x = quality, fill = qual_type)) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = NULL, y = "Frequency", fill = NULL, title = "Comparison between fixed vs mobile broadband quality satisfaction") +
  theme_bw()


## ----- Mosaic plots ----------------------------------------------------
# install.packages("devtools")
# devtools::install_github("haleyjeppson/ggmosaic")
library(ggmosaic)
dat |>
  mutate(
    q_fbqual = forcats::fct_recode(q_fbqual, 
                                   "l\nPoor" = "Poor", 
                                   "Very Poor        " = "Very Poor"
    )) |>
  ggplot() +
  geom_mosaic(aes(x = q_fbqual, fill = q_mbqual), show.legend = FALSE) +
  theme_mosaic() +
  labs(x = "Fixed broadband quality", y = "Mobile broadband quality")

# plot 2: titanic dataset
# install.packages("titanic")
library(titanic)
titanic |>
  # mutate(
  #   q_fbqual = forcats::fct_recode(q_fbqual, 
  #     "l\nPoor" = "Poor", 
  #     "Very Poor        " = "Very Poor"
  # )) |>
  ggplot() +
  geom_mosaic(aes(x = product(Sex, Class, Survived), fill = Class, alpha = Sex)) +
  scale_alpha_manual(values = c(0.5, 1.0)) + 
  scale_fill_viridis_d() +
  theme_mosaic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(, y = "Passenger class") 


## ----- Co-variability ---------------------------------------------------
dat |>
  mutate(
    age = cut(age, breaks = c(0, 18, 40, 60, Inf),
              labels = paste0("Age: ", c("< 18", "18-40", "40-60", "60+"))),
    education = fct_collapse(
      education,
      `Secondary\nor lower` = c("Primary School", "Lower Secondary", "O Level", "A Level"),
      `Post-\nsecondary` = c("National Certificate", "Diploma", "National Diploma", "Higher National Diploma"),
      Tertiary = c("Bachelor Degree", "Master Degree", "PhD")
    )
  ) |>
  ggplot(aes(q_fbusage, q_fbexpend, col = gender)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, linewidth = 0.8) +
  facet_grid(education ~ age) +
  labs(x = "Data usage (GB)", y = "Monthly expenditure (BND)", col = "Gender")


## ----- ggplot: breakdown -----------------------------------------------
dat |>
  mutate(
    # Categorise age
    age = cut(age, breaks = c(0, 18, 40, 60, Inf),
              labels = paste0("Age: ", c("< 18", "18-40", "40-60", "60+"))),
    # Collapse education levels into three groups
    education = fct_collapse(
      education,
      `Secondary\nor lower` = c("Primary School", "Lower Secondary", "O Level", "A Level"),
      `Post-\nsecondary` = c("National Certificate", "Diploma", "National Diploma", "Higher National Diploma"),
      Tertiary = c("Bachelor Degree", "Master Degree", "PhD")
    )
  ) |>
  ggplot(aes(x = q_fbusage, y = q_fbexpend, col = gender)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, fullrange = TRUE, linewidth = 0.8) +
  facet_grid(education ~ age) +
  labs(x = "Data usage (GB)", y = "Monthly expenditure (BND)", col = "Gender")


## ----- ggplot: themes --------------------------------------------------
left_join(
  mutate(bruneimap::census2021, district = gsub("Brunei Muara", "Brunei-Muara", district)),
  kpg_sf
) |>
  mutate(
    area = as.numeric(area) / 1000 ^ 2,
    popdens = population / area,
    district = factor(district, levels = rev(c("Brunei-Muara", "Belait", "Tutong", "Temburong")))
  ) |> 
  ggplot(aes(area, population)) +
  geom_point(aes(col = district, size = popdens), alpha = 0.9) +
  scale_x_log10() +
  scale_y_log10(labels = scales::comma) +
  scale_size_continuous(range = c(2, 15)) +
  ggthemes::theme_excel() +
  ggthemes::scale_colour_excel() +
  guides(colour = guide_legend(reverse = TRUE)) +
  labs(x = "Area (square kilometres)", y = "Population", 
       title = "Kampongs in Brunei Darussalam: Area vs Population",
       subtitle = "Population density is represented by point size",
       col = "District", size = "Population\ndensity") 


## ----- Spatial: points ---------------------------------------------------------
ggplot() +
  geom_sf(data = brn_sf, fill = NA) +
  geom_point(data = masjid, aes(latitude, longitude), col = viridis::mako(10)[4], size = 1) +
  theme_void()


## ----- spatial: lines --------------------------------------------------
load("R/aiti_rd.RData")
ggplot() +
  geom_sf(data = aiti_sf, fill = NA, col = NA) +
  geom_sf(data = aiti_rd, aes(col = highway, linewidth = highway), show.legend = FALSE) +
  scale_linewidth_manual(values = c(0.3, 0.3, 0.9, 0.9, 0.3, 0.6,
                                    0.6, 0.3, 0.3, 0.4, 0.4, 0.3)) +
  scale_colour_viridis_d(option = "mako") +
  theme_void()


## ----- Spatial: polygons -----------------------------------------------
ggplot(kpg_sf) +
  geom_sf(aes(fill = mukim), col = "gray50", alpha = 0.8, show.legend = FALSE) +
  geom_sf(data = mkm_sf, col = "black", lwd = 0.5, fill = NA) +
  geom_sf(data = dplyr::filter(kpg_sf, is.na(mukim)), fill = "gray70", col = "gray70") +
  ggrepel::geom_label_repel(
    data = mutate(mkm_sf, mukim = gsub("Mukim ", "", mukim)),    
    aes(label = mukim, geometry = geometry),
    size = 2,
    alpha = 0.9,
    stat = "sf_coordinates",
    box.padding = 0.3,
    max.overlaps = Inf,
    min.segment.length = 0,       
    segment.size = 0.3,           
    segment.curvature = 0.1,      
    force = 5                     
  ) +
  scale_fill_viridis_d(option = "mako") +
  theme_void()


## ----- Spatial patterns ------------------------------------------------
spend_mkm_df <-
  dat |>
  summarise(spend = mean(q_fbexpend), .by = mukim)
head(spend_mkm_df, 8)

left_join(
  mkm_sf,  # spatial
  spend_mkm_df  # study
) |>
  ggplot() +
  geom_sf(aes(fill = spend)) 

left_join(mkm_sf, spend_mkm_df) |>
  ggplot() +
  geom_sf(aes(fill = spend)) +
  scale_fill_viridis_c(
    name = "Monthly\nexpenditure",
    na.value = "white",
    labels = scales::dollar,
  ) +
  theme_void()

## ----- Word clouds -----------------------------------------------------------
head(dat$q_limiting, 5)

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