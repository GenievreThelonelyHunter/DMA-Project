
library(ggplot2)
library(tidyverse)
library(igraph)


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

games_scores <- games%>%drop_na()%>%select(Name,Critic_Score,User_Score,User_Count,Year_of_Release,Publisher) %>% arrange(Year_of_Release) %>% mutate (Critic_Score = rescale_to_1_10(Critic_Score)) %>% mutate(User_Score = as.numeric(as.character(User_Score)))


ggplot(data = netflix_scores, mapping = aes(x=tmdb_score)) 
+geom_histogram(color = "red") + labs(x = "Netflix User Score")

ggplot(data = netflix_scores, mapping = aes(x=imdb_score)) +
geom_histogram(color = "red") +
labs(x="netflix Critic score")

#
ggplot(data = hbo_scores, mapping = aes(x=tmdb_score))+
geom_histogram(color = "purple")
+labs(x="HBO User score")

ggplot(data = hbo_scores, mapping = aes(x=imdb_score)) +
geom_histogram(color = "purple") + labs(x = "HBO Critic Score")

#
ggplot(data = amazon_scores, mapping = aes(x=tmdb_score)) +
  geom_histogram(color = "#1399FF") + 
  labs(x = "Amazon Prime Video User Score")


ggplot(data = amazon_scores, mapping = aes(x=imdb_score)) +
geom_histogram(color = "#1399FF") +
labs(x = "Amazon Prime Video Critic Score")
#

ggplot(data = disney_scores, mapping = aes(x=tmdb_score)) +
  geom_histogram(color = "blue") + 
  labs(x = "Disney+ User Score")


ggplot(data = disney_scores, mapping = aes(x=imdb_score)) +
geom_histogram(color = "blue") +
labs(x = "Disney+ Critic Score")
# 
ggplot(data = games_scores, mapping = aes(User_Score)) +
  geom_histogram(color = "gold") +
  labs(x = "Video Games User score")


ggplot(data = games_scores, mapping = aes(x=Critic_Score)) +
geom_histogram(color = 'gold')+
labs(x = "Video Games Critic Score")

ggplot(games_scores, mapping = aes(x = User_Score, y= User_Count)) +
geom_point() + geom_smooth(method = "loess") 

ggplot(games, mapping = aes(x = Publisher, y= User_Count)) +
geom_point() + geom_smooth(method = "loess") 


a <- games_scores %>% group_by(Publisher)%>% summarise(Total = sum(User_Count))
a

b<-graph_from_data_frame(a)
plot(b)
read_table(b)
