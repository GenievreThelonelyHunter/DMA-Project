
library(ggplot2)
library(tidyverse)
library(igraph)
library(ggpubr)
library(patchwork)
library(plotly)    
library(ggraph)
library(tidygraph)

#functions
rescale_to_1_10 <- function(x) {
  # Normalize the values to a 0-1 range
  scaled <- (x /10)
  # Rescale to a 1-10 range!
  return(scaled)
}


#variables

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




#plots
#Netflix
ggplot(data = netflix_scores, mapping = aes(x=tmdb_score)) 
+geom_histogram(color = "red") + labs(x = "Netflix User Score")

ggplot(data = netflix_scores, mapping = aes(x=imdb_score)) +
geom_histogram(color = "red") +
labs(x="netflix Critic score")



imdb_netflix <- ggplot(netflix_scores, mapping = aes(x=imdb_score, y=release_year))+ 
geom_point(fill= "darkred", color = "darkred")

tmdb_netflix <- ggplot(netflix_scores, mapping = aes(x=tmdb_score, y=release_year))+ geom_point(fill = "red", color="red") 
compare_score_netflix <- imdb_netflix + tmdb_netflix

combined_imdb_and_tmbd_netflix <- imdb_netflix + geom_point(netflix_scores, mapping = aes(x=tmdb_score, y=release_year),fill = "red", color="red")

compare_score_netflix
combined_imdb_and_tmbd_netflix

g <- graph_from_data_frame(d = netflix_scores, directed = FALSE)
tbl_graph <- as_tbl_graph(g)
network <- ggraph(tbl_graph, layout = "fr") +
    geom_edge_link(aes(width =0.5), edge_alpha = 0.5) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
network  


#HBO
ggplot(data = hbo_scores, mapping = aes(x=tmdb_score))+
geom_histogram(color = "purple")
+labs(x="HBO User score")

ggplot(data = hbo_scores, mapping = aes(x=imdb_score)) +
geom_histogram(color = "purple") + labs(x = "HBO Critic Score")


imdb_hbo <- ggplot(hbo_scores, mapping = aes(x=imdb_score, y=release_year))+ 
geom_point(fill= "#5c1588", color = "#5c1588")

tmdb_hbo <- ggplot(hbo_scores, mapping = aes(x=tmdb_score, y=release_year))+ geom_point(fill = "purple", color="purple") 
compare_score_hbo <- imdb_hbo + tmdb_hbo

combined_imdb_and_tmbd_hbo <- imdb_hbo + geom_point(hbo_scores, mapping = aes(x=tmdb_score, y=release_year),fill = "purple", color="purple")

compare_score_hbo
combined_imdb_and_tmbd_hbo

g <- graph_from_data_frame(d = hbo_scores, directed = FALSE)
tbl_graph <- as_tbl_graph(g)
network <- ggraph(tbl_graph, layout = "fr") +
    geom_edge_link(aes(width =0.5), edge_alpha = 0.5) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
network  


#amazon prime video
ggplot(data = amazon_scores, mapping = aes(x=tmdb_score)) +
  geom_histogram(color = "#1399FF") + 
  labs(x = "Amazon Prime Video User Score")


ggplot(data = amazon_scores, mapping = aes(x=imdb_score)) +
geom_histogram(color = "#1399FF") +
labs(x = "Amazon Prime Video Critic Score")



imdb_amazon <- ggplot(amazon_scores, mapping = aes(x=imdb_score, y=release_year))+ 
geom_point(fill= "#0f77c7", color = "#0f77c7")

tmdb_amazon <- ggplot(amazon_scores, mapping = aes(x=tmdb_score, y=release_year))+ geom_point(fill = "#1399FF", color="#1399FF") 
compare_score_amazon <- imdb_amazon + tmdb_amazon

combined_imdb_and_tmbd_amazon <- imdb_amazon + geom_point(amazon_scores, mapping = aes(x=tmdb_score, y=release_year),fill = "#1399FF", color="#1399FF")


compare_score_amazon
combined_imdb_and_tmbd_amazon

g <- graph_from_data_frame(d = amazon_scores, directed = FALSE)
tbl_graph <- as_tbl_graph(g)
network <- ggraph(tbl_graph, layout = "fr") +
    geom_edge_link(aes(width =0.5), edge_alpha = 0.5) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
network  


#disney+

ggplot(data = disney_scores, mapping = aes(x=tmdb_score)) +
  geom_histogram(color = "blue") + 
  labs(x = "Disney+ User Score")


ggplot(data = disney_scores, mapping = aes(x=imdb_score)) +
geom_histogram(color = "blue") +
labs(x = "Disney+ Critic Score")


imdb_disney <- ggplot(hbo_scores, mapping = aes(x=imdb_score, y=release_year))+ 
geom_point(fill= "darkblue", color = "darkblue")

tmdb_disney <- ggplot(disney_scores, mapping = aes(x=tmdb_score, y=release_year))+ geom_point(fill = "blue", color="blue") 
compare_score_hbo <- imdb_disney + tmdb_disney

combined_imdb_and_tmbd_disney <- imdb_disney + geom_point(disney_scores, mapping = aes(x=tmdb_score, y=release_year),fill = "blue", color="blue")


compare_score_disney
combined_imdb_and_tmbd_disney

g <- graph_from_data_frame(d = disney_scores, directed = FALSE)
tbl_graph <- as_tbl_graph(g)
network <- ggraph(tbl_graph, layout = "fr") +
    geom_edge_link(aes(width =0.5), edge_alpha = 0.5) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
network  

# games
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


critc_score<- ggplot(games_scores, mapping = aes(x=Critic_Score, y=Year_of_Release))+ 
geom_point(fill= "#7e6c0a", color = "#7e6c0a")

user_score <- ggplot(games_scores, mapping = aes(x=User_Score, y=Year_of_Release))+ geom_point(fill = "gold", color="gold") 
compare_score_critic_and_user <- critc_score + user_score

combined_user_and_critic_score <- critc_score + geom_point(games_scores, mapping = aes(x=User_Score, y=Year_of_Release),fill = "gold", color="gold")


compare_score_critic_and_user
combined_user_and_critic_score

Total_publisher_count <- games_scores %>% group_by(Publisher)%>% summarise(Total = sum(User_Count)) 
most_famous <- Total_publisher_count %>% filter(Total > 20000)

plot_ly(Total_publisher_count, x = Total_publisher_count$Publisher, y = Total_publisher_count$Total)
plot_ly(most_famous, x = most_famous$Publisher, y = most_famous$Total)


#for (instance in unique(games_scores$Publisher)) {
  # Filter the data for the current Publisher
 # patchworks <- games_scores %>% filter(Publisher == instance)
  
  # Create the graph for the current Publisher
  #g <- graph_from_data_frame(d = patchworks, directed = FALSE)
  #tbl_graph <- as_tbl_graph(g)
  #graph <- ggraph(tbl_graph, layout = "fr") +
   # geom_edge_link(aes(width =0.5), edge_alpha = 0.5) +
    #geom_node_point(color = "lightblue", size = 5) +
    #geom_node_text(aes(label = name), repel = TRUE) +
    #theme_void()
  
  # Store the graph object in the list
  #graph_list[[instance]] <- graph
#}

#dev.new(width = 1600000, height = 1600000)
#combined_plots <- wrap_plots(graph_list)
#combined_plots
#ordered_graph_list <- graph_list[order(most_famous$Publisher)]
#combined_ordered_graph_list <- wrap_plots(ordered_graph_list)
#combined_ordered_graph_list

# Group by Publisher and Score, and count the frequencies
score_frequencies <- games_scores %>%
  group_by(Publisher, User_Score) %>%
  summarise(Frequency = n(), .groups = 'drop')

# Create a complete grid of all publishers and scores from 1 to 10 with decimals
all_scores <- expand.grid(Publisher = unique(games_scores$Publisher), User_Score = seq(1, 10, by = 0.1))

# Join the complete grid with the calculated frequencies, filling in missing values with 0
score_frequencies_complete <- all_scores %>%
  left_join(score_frequencies, by = c("Publisher", "User_Score")) %>% drop_na()


g <- graph_from_data_frame(d = score_frequencies_complete, directed = FALSE)
  tbl_graph <- as_tbl_graph(g)
  graph <- ggraph(tbl_graph, layout = "kk") +
    geom_edge_link(edge_alpha = 0.5) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE, max.overlaps = 300) +
    theme_void()
graph



# Calculate score frequencies for all publishers
score_frequencies_most_famous <- games_scores %>%
  group_by(Publisher, User_Score) %>%
  summarise(Frequency = n(), .groups = 'drop')

# Create a complete grid of all publishers from 'most_famous' and scores from 1 to 10 with decimals
all_scores_most_famous <- expand.grid(
  Publisher = unique(most_famous$Publisher),
  User_Score = seq(1, 10, by = 0.1)
)

# Join the complete grid with the calculated frequencies, filling in missing values with 0
score_frequencies_complete_most_famous <- all_scores_most_famous %>%
  left_join(score_frequencies_most_famous, by = c("Publisher", "User_Score")) %>%
  replace_na(list(Frequency = 0))

# Filter the data to include only publishers in 'most_famous'
filtered_data <- score_frequencies_complete_most_famous %>%
  filter(Publisher %in% most_famous$Publisher)

# Create a graph from the filtered data frame
g <- graph_from_data_frame(d = filtered_data, directed = FALSE)

# Convert to tbl_graph for ggraph
tbl_graph <- as_tbl_graph(g)



# Create a visual representation of the graph
graph <- ggraph(tbl_graph, layout = "kk") +
  geom_edge_link(edge_alpha = 0.5) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  coord_flip() +
  theme_void()

# Plot the graph
graph