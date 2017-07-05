library(dplyr)
library(ggplot2)
library(janeaustenr)
library(methods)
library(purrr)
library(quanteda)
library(stringr)
library(tidyr)
library(tidytext)
library(tm)
library(tm.plugin.webmining)

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

# Tidying corpus objects with metadata
data("acq")
acq

# first document
acq[[1]]

# Tidying corpus
acq_td <- tidy(acq)
acq_td

# Tokenize
acq_tokens <- acq_td %>%
  select(-places) %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words, by = "word")

# Most common words
acq_tokens %>%
  count(word, sort = TRUE)

# Tf-idf
acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))

# Mining financial articles
company <- c("Microsoft", "Apple", "Google", "Amazon", "Facebook",
             "Netflix")
symbol <- c("MSFT", "AAPL", "GOOG", "AMZN", "FB", "NFLX")

download_articles <- function(symbol){
  WebCorpus(GoogleFinanceSource(paste0("NASDAQ:", symbol)))
}

stock_articles <- data_frame(company = company,
                             symbol = symbol) %>%
  mutate(corpus = map(symbol, download_articles))

stock_articles

# Tokenize articles
stock_tokens <- stock_articles %>%
  unnest(map(corpus, tidy)) %>%
  unnest_tokens(word, text) %>%
  select(company, datetimestamp, word, id, heading)

stock_tokens

# Tf-idf
stock_tf_idf <- stock_tokens %>%
  count(company, word) %>%
  filter(!str_detect(word, "\\d+")) %>%
  bind_tf_idf(word, company, n) %>%
  arrange(-tf_idf)

# Plot top-words by company
stock_tf_idf %>%
  group_by(company) %>%
  top_n(8, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = company)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~company, scales = "free") +
  coord_flip()

# Word's sentiment score
stock_tokens %>%
  anti_join(stop_words, by = "word") %>%
  count(word, id, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(contribution = sum(n * score)) %>%
  top_n(12, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution)) +
  geom_col() +
  coord_flip() +
  labs(y = "Frequency of word * AFINN score")

##AFINN et al don't seem to fit financial data. Try loughran instead
# Most common words belonging to each sentiment
stock_tokens %>%
  count(word) %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  group_by(sentiment) %>%
  top_n(5, n) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~sentiment, scales = "free") +
  ylab("Frequency of this word in the recent financial articles")

# Company x Sentiment scores
stock_sentiment_count <- stock_tokens %>%
  inner_join(get_sentiments("loughran"), by = "word") %>%
  count(sentiment, company) %>%
  spread(sentiment, n, fill = 0)

stock_sentiment_count

# Positivity of each company
stock_sentiment_count %>%
  mutate(score = (positive - negative) / (positive + negative)) %>%
  mutate(company = reorder(company, score)) %>%
  ggplot(aes(company, score, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = "Company",
       y = "Positivity score among 20 recent news articles")