library(ggplot2)
library(tidyverse)


netflix <- read.csv("per il progetto di dma/Netflix.csv")
amazon_prime <- read.csv("per il progetto di dma/Amazon Prime.csv")
hbo<-read.csv("per il progetto di dma/HBO.csv")
disney<-read.csv("per il progetto di dma/Disney+.csv")
games<-read.csv("per il progetto di dma/Video_Games.csv")


netflix_scores <- netflix%>%drop_na()%>%select(title,imdb_score,tmdb_score,tmdb_popularity)


amazon_scores <- amazon_prime%>%drop_na()%>%select(title,imdb_score,tmdb_score,tmdb_popularity)


hbo_scores <- hbo%>%drop_na()%>%select(title,imdb_score,tmdb_score,tmdb_popularity)
disney_scores <- disne%>%drop_na()%>%select(title,imdb_score,tmdb_score,tmdb_popularity)
games_scores <- games%>%drop_na()%>%select(Name,Critic_Score,User_Score,User_Count)

games_scores

rescale_to_1_10 <- function(x) {
  # Normalize the values to a 0-1 range
  normalized <- (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  # Rescale to a 1-10 range
  scaled <- normalized * 9 + 1
  return(scaled)
}

games_scores <- games_scores %>% mutate (Critic_Score = rescale_to_1_10(Critic_Score))
games_scores

ggplot(data = scores, mapping = aes(x=tmdb_score)) +
  geom_histogram()


ggplot(data = scores, mapping = aes(x=imdb_score)) +
geom_histogram()


ggsave("scatterplot.png", width = 9, height = 7, dpi = 1000)
ggplot(data = scores, mapping = aes(x=tmdb_score)) +
  geom_histogram()

ggplot(data = scores, mapping = aes(x=imdb_score,y =tmdb_score)) +
geom_point()



