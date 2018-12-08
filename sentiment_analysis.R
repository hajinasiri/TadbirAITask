library(SentimentAnalysis)
library(tidyverse)
library(tidytext)

# We will use Loughran and McDonald dictionary of financial sentiment terms
# for sentiment analysis
loughran <- loadDictionaryLM()
loughran_negs <- loughran$negativeWords
loughran_pos <- loughran$positiveWords
false_negs <- c("er", "su", "0")
negs <- loughran_negs[!loughran_negs %in% false_negs]

# Some entries in dictionary would cause conflicts if we use them in their
# raw form. For example "plea" would match to "please"
negs[which(str_detect(negs, "^eas$"))] <- 
    negs[which(str_detect(negs, "^eas$"))] %>% paste0("$")

negs[which(str_detect(negs, "^plea$"))] <- 
    negs[which(str_detect(negs, "^plea$"))] %>% paste0("$")

negs[which(str_detect(negs, "^break$"))] <- 
    negs[which(str_detect(negs, "^break$"))] %>% paste0("(?!through)")

negs <- paste0("^", negs)

pos <- loughran_pos %>% 
    paste0("^", .)

# We create a very long regex by combining all the words in the dictionary.
# This would help to filter out redundant words from `news`` which in turn
# would speed-up the rest of the analyses.
loughran_regex <- union(negs, pos) %>% paste0(collapse = "|")
news_words <- news %>% 
    unnest_tokens(word, headline_and_summary) %>% 
    filter(str_detect(word, loughran_regex))

str_dtct_any <- function(...) {
    str_detect(...) %>% 
        any()
}

# Determining whether a given word is positive or negative:
pos_or_neg <- mutate(
    news_words,
    positive = map_lgl(word, ~str_dtct_any(.x, pattern = pos)),
    negative = map_lgl(word, ~str_dtct_any(.x, pattern = negs))
)

# This checks if there is any word which would be classified as both positive
# or negative. There shouldn't be any such word.
mutate(pos_or_neg, net = positive + negative) %>% 
    filter(net != 1)

words_with_score <- mutate(
    pos_or_neg,
    score = positive * 1 + negative * (-1)
) %>% 
    select(-positive, -negative)

score_to_sentiment <- function(x) {
    case_when(
        x > 0 ~ "positive",
        x < 0 ~ "negative",
        TRUE ~ "neutral"
    )
}

news_with_sentiment <- words_with_score %>% 
    group_by(news_id) %>% 
    summarise(score = sum(score)) %>% 
    left_join(news, ., by = "news_id") %>% 
    mutate(
        score = ifelse(is.na(score), 0, score),
        sentiment = score_to_sentiment(score)
        ) %>% 
    select(company, headline_and_summary, sentiment)
