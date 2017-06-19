library(dplyr)
library(janeaustenr)
library(tidytext)
library(ggplot2)

# What are the most commonly used words in Jane Austen's novels?
book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

# Number of terms by book
total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))

# Combine individual words and total words by book in the same data frame
book_words <- left_join(book_words, total_words)

book_words

# Take a look at term frequencies (n terms / total terms)
ggplot(book_words, aes(n/total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# Examine Zipf's law
freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)

freq_by_rank

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_line(size = 1.2, alpha = 0.8) +
  scale_x_log10() +
  scale_y_log10()

# Examine slope
rank_subset <- freq_by_rank %>%
  filter(rank > 10,
         rank < 500)
lm(log10(`term frequency`) ~ log10(rank), data = rank_subset)

freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color = book)) +
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.2, alpha = 0.8) +
  scale_x_log10() + 
  scale_y_log10()
