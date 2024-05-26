
library(ggplot2)
library(tidyverse)
library(igraph)
library(ggpubr)
library(patchwork)
library(plotly)    
library(ggraph)
library(tidygraph)


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

imdb_netflix <- ggplot(netflix_scores, mapping = aes(x=imdb_score, y=release_year))+ 
geom_point(fill= "darkred", color = "darkred")

tmdb_netflix <- ggplot(netflix_scores, mapping = aes(x=tmdb_score, y=release_year))+ geom_point(fill = "red", color="red") 
compare_score <- imdb_netflix + tmdb_netflix
compare_score
foo <- imdb_netflix + geom_point(netflix_scores, mapping = aes(x=tmdb_score, y=release_year),fill = "red", color="red")
foo

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



Total_publisher_count <- games_scores %>% group_by(Publisher)%>% summarise(Total = sum(User_Count)) 

plot_ly(Total_publisher_count, x = Total_publisher_count$Publisher, y = Total_publisher_count$Total)

most_famous <- Total_publisher_count %>% filter(Total > 20000)
plot_ly(most_famous, x = most_famous$Publisher, y = most_famous$Total)
graph_list <- list()
for (instance in unique(games_scores$Publisher)) {
  # Filter the data for the current Publisher
  patchworks <- games_scores %>% filter(Publisher == instance)
  
  # Create the graph for the current Publisher
  g <- graph_from_data_frame(d = patchworks, directed = FALSE)
  tbl_graph <- as_tbl_graph(g)
  graph <- ggraph(tbl_graph, layout = "fr") +
    geom_edge_link(aes(width = 1), edge_alpha = 0.5) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
  
  # Store the graph object in the list
  graph_list[[instance]] <- graph
}
combined_plots <- wrap_plots(graph_list)
dev.new(width = 32000, height = 3200)
combined_plots
ordered_graph_list <- graph_list[order(most_famous$Publisher)]
combined_ordered_graph_list <- wrap_plots(ordered_graph_list)
combined_ordered_graph_list

