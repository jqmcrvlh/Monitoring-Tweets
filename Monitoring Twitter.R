library(dplyr)
library(lubridate)
library(rtweet)

my_tweets_raw <- get_timeline(user = "LifeonData", n = 2500)

my_tweets <- my_tweets_raw %>%
  select(status_id, created_at, text, is_retweet, reply_to_screen_name, favorite_count,retweet_count, hashtags, mentions_screen_name)

my_tweets <- my_tweets %>%
  mutate(created_at = with_tz(created_at, tzone = "Brazil/East")) %>%
  filter(created_at >= "2018-01-01") %>%
  mutate_at(vars(created_at), funs(date = date, month = floor_date(., unit = "month"))) %>%
  mutate_at(vars(date, month), as.Date)



library(forcats)

my_tweets <- my_tweets %>%
  mutate(tweet_type = case_when(is_retweet ~ "Retweet",!is.na(reply_to_screen_name) & reply_to_screen_name != "LifeonData" ~ "Reply", TRUE ~ "Original"),tweet_type = fct_relevel(as_factor(tweet_type),c("Original", "Reply", "Retweet"))) %>%
  select(-is_retweet)

january_tweets <- my_tweets %>%
  filter(month == "2019-01-01")

library(ggplot2)
theme_set(theme_minimal())

january_tweets %>%
  ggplot(aes(x = tweet_type, fill = tweet_type)) + geom_histogram(stat = "count") + coord_flip() +  theme(legend.position = "none", axis.title = element_blank(), axis.text = element_text(size = 10))

january_tweets %>%
  ggplot(aes(x = date, fill = tweet_type)) + geom_histogram(binwidth = 1) + theme(legend.position = "bottom", axis.title = element_blank(), axis.text = element_text(size = 10), legend.title = element_blank())


library(tidyr)
library(stringr)

january_tweets %>%
  filter(tweet_type != "Retweet") %>%
  group_by(date) %>%
  summarise_at(vars(favorite_count, retweet_count), sum) %>%
  gather(measure, count, ends_with("count")) %>%
  mutate(measure = str_replace(measure, "_count", "")) %>%
  ggplot(aes(x = date, y = count, colour = measure)) +  geom_line() + scale_x_discrete("Date of original tweet") + theme(legend.position = "bottom", axis.title.y = element_blank(), axis.text = element_text(size = 10), legend.title = element_blank())



january_tweets %>% 
  filter(tweet_type != "Retweet") %>%
  filter(favorite_count == max(favorite_count)) %>%
  pull(status_id) %>%
  blogdown::shortcode("tweet", .)



mentions <- january_tweets %>% 
  filter(tweet_type != "Retweet") %>%
  select(username = mentions_screen_name) %>% 
  unnest() %>% 
  filter(!is.na(username)) %>% 
  count(username, sort = TRUE)

mentions %>%
  head(10) %>%
  ggplot(aes(x = fct_reorder(username, n), y = n, fill = username)) + labs(x = NULL, y = "Mentions") + geom_col() + coord_flip() + theme(legend.position = "none", axis.text = element_text(size = 10))

library(purrr)

friends_tweets <- mentions %>%
  filter(row_number() <= 8) %>%
  mutate(tweets = map(username, get_timeline, n = 1000)) %>%
  unnest() %>%
  filter(!is_retweet) %>%
  select(username, created_at, text)


friends_tweets <- friends_tweets %>%
  mutate(created_at = with_tz(created_at, "Brazil/East")) %>%
  filter(created_at >= "2019-01-01" & created_at <= "2019-01-31")

library(tidytext)

tidy_words <- friends_tweets %>%
  select(username, text) %>%
  bind_rows( january_tweets %>%
      select(text) %>%
      mutate(username = "LifeonData")) %>%
  mutate(text = str_replace_all(text, "'", "'")) %>% 
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!str_detect(word, "^@"), !str_detect(word, "[\\uD83C-\\uDBFF\\uDC00-\\uDFFF]+")) %>%
  anti_join(stop_words, by = "word") %>%
  count(username, word)

total_words <- tidy_words %>%
  group_by(username) %>%
  summarize(total = sum(n))

tf_idf_words <- tidy_words %>%
  left_join(total_words, by = "username") %>%
  bind_tf_idf(word, username, n) %>%
  group_by(username) %>%
  top_n(5, wt = tf_idf) %>%
  ungroup() 

tf_idf_words %>%
  ggplot(aes(x = fct_reorder(word, tf_idf), y = tf_idf, fill = username)) + geom_col() + labs(x = NULL, y = "tf-idf") + facet_wrap(~username, ncol = 3, scales = "free") + coord_flip() + theme(axis.text.x = element_blank(),legend.position = "none", axis.text = element_text(size = 10), strip.text = element_text(size = 10))


my_tweets %>%
  ggplot(aes(x = month,  fill = tweet_type)) +  geom_histogram(stat = "count") +  theme(legend.position = "bottom", legend.title = element_blank(), axis.title = element_blank(), axis.text = element_text(size = 10))
