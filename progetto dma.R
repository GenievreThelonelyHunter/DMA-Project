
library(ggplot2)
library(tidyverse)


rescale_to_1_10 <- function(x) {
  # Normalize the values to a 0-1 range
  scaled <- (x /10)
  # Rescale to a 1-10 range!
  return(scaled)
}




netflix <- read.csv("per il progetto di dma/Netflix.csv")
amazon_prime <- read.csv("per il progetto di dma/Amazon Prime.csv")
hbo<-read.csv("per il progetto di dma/HBO.csv")
disney<-read.csv("per il progetto di dma/Disney+.csv")
games<-read.csv("per il progetto di dma/Video_Games.csv")


netflix_scores <- netflix%>%drop_na()%>%select(title,imdb_score,tmdb_score,tmdb_popularity,release_year) %>% arrange(release_year)

amazon_scores <- amazon_prime%>%drop_na()%>%select(title,imdb_score,tmdb_score,tmdb_popularity,release_year) %>% arrange(release_year)

hbo_scores <- hbo%>%drop_na()%>%select(title,imdb_score,tmdb_score,tmdb_popularity,release_year) %>% arrange(release_year)

disney_scores <- disney%>%drop_na()%>%select(title,imdb_score,tmdb_score,tmdb_popularity,release_year) %>% arrange(release_year)

games_scores <- games%>%drop_na()%>%select(Name,Critic_Score,User_Score,User_Count,Year_of_Release) %>% arrange(Year_of_Release) %>% mutate (Critic_Score = rescale_to_1_10(Critic_Score))


ggplot(data = netflix_scores, mapping = aes(x=tmdb_score)) +geom_histogram(color = "red")

ggplot(data = netflix_scores, mapping = aes(x=imdb_score)) +
geom_histogram(color = "red") +
labs(x="netflix imdb score")

#
ggplot(data = hbo_scores, mapping = aes(x=tmdb_score))+geom_histogram(color = "purple")+labs(x="netflix tmdb score")

ggplot(data = hbo_scores, mapping = aes(x=imdb_score)) +
geom_histogram(color = "purple")

#
ggplot(data = amazon_scores, mapping = aes(x=tmdb_score)) +
  geom_histogram(color = "azure")


ggplot(data = amazon_scores, mapping = aes(x=imdb_score)) +
geom_histogram(color = "azure")
#

ggplot(data = disney_scores, mapping = aes(x=tmdb_score)) +
  geom_histogram(color = "blue")


ggplot(data = disney_scores, mapping = aes(x=imdb_score)) +
geom_histogram(color = "blue")
#


# 
ggplot(data = games_scores, mapping = aes(as.numeric(as.character(User_Score)))) +
  geom_histogram(color = "gold")


ggplot(data = games_scores, mapping = aes(x=Critic_Score)) +
geom_histogram(color = 'gold')

