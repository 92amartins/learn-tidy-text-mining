library(dplyr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(topicmodels)

# Load news dtm
data("AssociatedPress")
AssociatedPress

# Fit a two-topic LDA model
ap_lda <- LDA(AssociatedPress, k = 2, control = list(seed = 1234))
ap_lda

## Interpret LDA model

# Retrieve per-topic-per-word probabilities
ap_topics <- tidy(ap_lda, matrix = "beta")
ap_topics

# Select top-10 terms most common within each topic
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

# Highlight topics by log ratio
beta_spread <- ap_topics %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .001 | topic2 > .001) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread

# Plot top terms for each topic (Top-11 (topic 2), Bottom-9 (topic 1))
beta_spread %>%
  top_n(20, abs(log_ratio)) %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(term, log_ratio)) +
  geom_col() +
  coord_flip()