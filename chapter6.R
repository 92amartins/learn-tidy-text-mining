library(dplyr)
library(ggplot2)
library(gutenbergr)
library(stringr)
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

## Document-topic probabilities
ap_documents <- tidy(ap_lda, matrix = "gamma")
ap_documents

# Document 6 seen to come from topic 2 almost exclusively. Check that
tidy(AssociatedPress) %>%
  filter(document == 6) %>%
  arrange(desc(count))

## Great library heist
titles <- c("Twenty Thousand Leagues under the Sea", 
            "The War of the Worlds",
            "Pride and Prejudice",
            "Great Expectations")
books <- gutenberg_works(title %in% titles) %>%
  gutenberg_download(meta_fields = "title")

# Divide into documents, each representing one chapter
by_chapter <- books %>%
  group_by(title) %>%
  mutate(chapter = cumsum(
    str_detect(text, regex("^chapter", ignore_case = TRUE)))) %>%
  ungroup() %>%
  filter(chapter > 0) %>%
  unite(document, title, chapter)

# Split into words
by_chapter_word <- by_chapter %>%
  unnest_tokens(word, text)

# Find document-word counts
word_counts <- by_chapter_word %>%
  anti_join(stop_words) %>%
  count(document, word, sort = TRUE) %>%
  ungroup()

word_counts

## LDA on chapters

chapters_dtm <- word_counts %>%
  cast_dtm(document, word, n)

chapters_dtm

chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda

# Word-Topic probabilities
chapter_topics <- tidy(chapters_lda, matrix = "beta")
chapter_topics

# Top-5 terms within each topic
top_terms <- chapter_topics %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

# Document-Topic probabilities
chapters_gamma <- tidy(chapters_lda, matrix = "gamma")
chapters_gamma

chapters_gamma <- chapters_gamma %>%
  separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

chapters_gamma

# Reorder titles in order of topic before plotting
chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~title)

# Chapter classifications
chapter_classifications <- chapters_gamma %>%
  group_by(title, chapter) %>%
  top_n(1, gamma) %>%
  ungroup()

chapter_classifications

# Most common topics among chapters
book_topics <- chapter_classifications %>%
  count(title, topic) %>%
  group_by(title) %>%
  top_n(1, n) %>%
  ungroup() %>%
  transmute(consensus = title, topic)

# Chapters misclassified
chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)