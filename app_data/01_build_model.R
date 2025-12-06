library(tidyverse)
library(tidytext)
library(data.table)

# 1. Load Data (assuming you downloaded the HC Corpora dataset)
# Only read a sample! The full dataset is too big for a free Shiny app.
blogs <- readLines("/home/deekshith/Downloads/final/en_US/en_US.blogs.txt", warn = FALSE, n = 20000)
news <- readLines("/home/deekshith/Downloads/final/en_US/en_US.news.txt", warn = FALSE, n = 20000)
twitter <- readLines("/home/deekshith/Downloads/final/en_US/en_US.twitter.txt", warn = FALSE, n = 20000)

raw_data <- c(blogs, news, twitter)

# 2. Clean Data (Basic cleaning)
clean_data <- tibble(text = raw_data) %>%
  mutate(text = tolower(text)) %>%
  mutate(text = str_replace_all(text, "[0-9]", "")) %>% # You might need 'tm' library logic here or regex
  mutate(text = str_replace_all(text, "[^a-z\\s]", "")) # Keep only letters

# 3. Create N-Grams (Bigrams, Trigrams, Quadgrams)

# Bigrams (2 words)
bigrams <- clean_data %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  count(word1, word2, sort = TRUE)

# Trigrams (3 words)
trigrams <- clean_data %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  count(word1, word2, word3, sort = TRUE)

# Quadgrams (4 words)
quadgrams <- clean_data %>%
  unnest_tokens(quadgram, text, token = "ngrams", n = 4) %>%
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  count(word1, word2, word3, word4, sort = TRUE)

# 4. Pruning (Crucial for ShinyApps.io speed)
# Drop n-grams that appear only once (n=1) to save memory
bigrams <- bigrams %>% filter(n > 1)
trigrams <- trigrams %>% filter(n > 1)
quadgrams <- quadgrams %>% filter(n > 1)

# 5. Save the models
saveRDS(bigrams, "app_data/bigrams.rds")
saveRDS(trigrams, "app_data/trigrams.rds")
saveRDS(quadgrams, "app_data/quadgrams.rds")