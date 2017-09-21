library(tidyverse)

imdb <- read.csv("movie_metadata.csv")

glimpse(imdb)

imdb %>% 
  group_by(title_year) %>% 
  filter(!is.na(num_critic_for_reviews)) %>% 
  summarise(nreview=n()) %>% 
  ggplot(aes(title_year, nreview)) + geom_line()

imdb %>% 
  group_by(title_year) %>% 
  filter(!is.na(num_critic_for_reviews)) %>% 
  summarise(nreview=n()) %>% 
  ggplot(aes(title_year, nreview)) + geom_line()

imdb %>% 
  group_by(title_year) %>% 
  filter(!is.na(imdb_score)) %>% 
  summarise(avgreview=mean(imdb_score)) %>% 
  ggplot(aes(title_year, avgreview)) + geom_line()

imdb %>% 
  group_by(content_rating) %>% 
  filter(!is.na(content_rating)) %>% 
  arrange(imdb_score) %>% 
  ggplot(aes(content_rating, imdb_score)) + geom_boxplot()
