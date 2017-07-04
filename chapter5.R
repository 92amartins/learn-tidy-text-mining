library(dplyr)
library(ggplot2)
library(janeaustenr)
library(methods)
library(quanteda)
library(tidyr)
library(tidytext)
library(tm)

data("AssociatedPress", package = "topicmodels")

AssociatedPress

# Inspect terms
terms <- Terms(AssociatedPress)
head(terms)

# Tidying DTM
ap_td <- tidy(AssociatedPress)
ap_td

# Sentiment analysis
ap_sentiments <- ap_td %>%
  inner_join(get_sentiments("bing"), by = c(term = "word"))

ap_sentiments

# Contribution to sentiment
ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(term, n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  ylab("Contribution to sentiment") +
  coord_flip()

# Load sample of DFM (Document-Feature Matrix)
data("data_corpus_inaugural", package = "quanteda")
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)

inaug_dfm

# Tidying DFM
inaug_td <- tidy(inaug_dfm)
inaug_td

# Calculate tf-idf
inaug_tf_idf <- inaug_td %>%
  bind_tf_idf(term, document, count) %>%
  arrange(desc(tf_idf))

inaug_tf_idf

# Pick four notable speeches (Lincoln, Roosevelt, Kennedy, Obama)
plot_notable <- inaug_tf_idf %>%
  filter(document %in% c("1861-Lincoln", 
                         "1933-Roosevelt", 
                         "1961-Kennedy", 
                         "2009-Obama")) %>%
  arrange(desc(tf_idf)) %>%
  mutate(term = factor(term, levels = rev(unique(term))))

plot_notable %>%
  group_by(document) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  ggplot(aes(term, tf_idf, fill = document)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~document, scales = "free") +
  coord_flip()

# Words by year
year_term_counts <- inaug_td %>%
  extract(document, "year", "(\\d+)", convert = TRUE) %>%
  complete(year, term, fill = list(count = 0)) %>%
  group_by(year) %>%
  mutate(year_total = sum(count))

# How use of words change over time
year_term_counts %>%
  filter(term %in% c("god", 
                     "america", 
                     "foreign", 
                     "union", 
                     "constitution", 
                     "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  ylab("% frequency of word in inaugural address")

# Cast AP news back into a dtm
ap_td %>%
  cast_dtm(document, term, count)

# Cast AP news into a dfm
ap_td %>%
  cast_dfm(term, document, count)

# Cast a sparse matrix
m <- ap_td %>%
  cast_sparse(document, term, count)

class(m)
dim(m)

# DTM from Jane Austen's books
austen_dtm <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

austen_dtm