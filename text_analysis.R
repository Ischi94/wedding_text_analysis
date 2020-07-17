# load libraries
library(tidyverse)
library(tidytext)
library(ggwordcloud)

# read in data. Each entry is a recommendation from a guest given at our wedding.
text_data <- read_delim("recommendations.txt", delim = ".", col_names = c("text", "NA"))

# unnest each token
word_data <- text_data %>% 
  select(text) %>% 
  unnest_tokens(word, text) %>% 
  count(word, sort = TRUE)

# load german stop words
stop_german <- tibble(word = stopwords::stopwords("de"))

# remove stop words from data set
word_clean <- word_data %>% 
  anti_join(stop_german, by = c('word')) 

# make wordcloud
# define outline
mask <- png::readPNG(system.file("extdata/hearth.png", package = "ggwordcloud", 
                         mustWork = TRUE))
# set seed to wedding date
set.seed(270419)

# plot wordcloud
wcloud <- ggplot(word_clean, aes(label = word, size = n, color = n)) +
  geom_text_wordcloud(mask = mask, rm_outside = TRUE) +
  scale_radius(range = c(0, 22), limits = c(0, NA)) +
  scale_color_gradient(low = "#FBCEB1", high = "#efebdc") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "grey40",
                                    colour = "grey40",
                                    size = 0.5, linetype = "solid"))

# save in good resolution for printing
# for other purposes, set dpi lower
ggsave(wcloud, file = "wordcloud.png", dpi = 1000)
