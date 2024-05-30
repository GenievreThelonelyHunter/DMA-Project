
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


#variance
sqrt(var(x = netflix_scores$imdb_score, y = netflix_scores$tmdb_score))
sqrt(var(x = hbo_scores$imdb_score, y = hbo_scores$tmdb_score))
sqrt(var(x = amazon_scores$imdb_score, y = amazon_scores$tmdb_score))
sqrt(var(x = disney_scores$imdb_score, y = disney_scores$tmdb_score))
sqrt(var(x = games_scores$Critic_Score, y = games_scores$User_Score))

#plots
#Netflix
ggplot(data = netflix_scores, mapping = aes(x=tmdb_score)) +
geom_boxplot(color = "red") + labs(x = "Netflix User Score")

ggsave("Netflix User Score.png")

ggplot(data = netflix_scores, mapping = aes(x=imdb_score)) +
geom_boxplot(color = "darkred") +
labs(x="netflix Critic score")


ggsave("Netflix Critic Score.png")

imdb_netflix <- ggplot(netflix_scores, mapping = aes(x=imdb_score, y=release_year))+ 
geom_point(fill= "darkred", color = "darkred") + labs(x = "Netflix Critic scores", y ="Year of Release")

tmdb_netflix <- ggplot(netflix_scores, mapping = aes(x=tmdb_score, y=release_year))+ geom_point(fill = "red", color="red") + labs(x = "Netflix User scores", y ="Year of Release")
compare_score_netflix <- imdb_netflix + tmdb_netflix 


ggsave( "Netflix Compare Score.png")
combined_imdb_and_tmbd_netflix <- imdb_netflix + geom_point(netflix_scores, mapping = aes(x=tmdb_score, y=release_year),fill = "red", color="red") + labs(x = "combined scores", y ="Year of Release")
combined_imdb_and_tmbd_netflix <- combined_imdb_and_tmbd_netflix

ggsave("Netflix Combined Scores.png")



ggplot(netflix_scores, mapping = aes(x = imdb_score, y= tmdb_score)) + geom_point(fill = "red", color= "red") + geom_smooth(method = "lm") 
ggsave("Netflix Scores with regression.png")
netflix_user_scores <- netflix_scores %>% select(title, tmdb_score, tmdb_popularity)
netflix_crit_scores <- netflix_scores %>% select(title, imdb_score, tmdb_popularity)


g <- graph_from_data_frame(d = netflix_user_scores, directed = FALSE)
tbl_graph <- as_tbl_graph(g)
network <- ggraph(tbl_graph, layout = "fr") +
    geom_edge_link(aes(width =tmdb_popularity), edge_alpha = 0.5) +
    geom_node_point(color = "red", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
  
ggsave( "Network Netflix User Score.png")

g <- graph_from_data_frame(d = netflix_crit_scores, directed = FALSE)
tbl_graph <- as_tbl_graph(g)
network <- ggraph(tbl_graph, layout = "fr") +
    geom_edge_link(aes(width =tmdb_popularity), edge_alpha = 0.5) +
    geom_node_point(color = "darkred", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
  
ggsave( "Network Netflix User Score.png")
plot_ly(netflix_scores, x= netflix_scores$title, y = netflix_scores$tmdb_popularity, color = I("red"))

#HBO
ggplot(data = hbo_scores, mapping = aes(x=tmdb_score))+
geom_boxplot(color = "purple")
+labs(x="HBO User score")
ggsave("HBO User Score.png")
ggplot(data = hbo_scores, mapping = aes(x=imdb_score)) +
geom_boxplot(color = "purple") + labs(x = "HBO Critic Score")
ggsave("HBO Critic score.png")

imdb_hbo <- ggplot(hbo_scores, mapping = aes(x=imdb_score, y=release_year))+ 
geom_point(fill= "#5c1588", color = "#5c1588") +labs(x = "HBO Critic scores", y ="Year of Release")

tmdb_hbo <- ggplot(hbo_scores, mapping = aes(x=tmdb_score, y=release_year))+ geom_point(fill = "purple", color="purple") + labs(x = "HBO User scores", y ="Year of Release")
compare_score_hbo <- imdb_hbo + tmdb_hbo
ggsave( "HBO Compared score.png")
combined_imdb_and_tmbd_hbo <- imdb_hbo + geom_point(hbo_scores, mapping = aes(x=tmdb_score, y=release_year),fill = "purple", color="purple") + labs(x = "combined scores", y ="Year of Release")


ggsave( "HBO combined scores.png")
ggplot(hbo_scores, mapping = aes(x = imdb_score, y= tmdb_score)) + geom_point(fill = "purple", color= "purple") + geom_smooth(method = "lm") 
ggsave("HBO Scores with regression.png")

hbo_user_scores <- hbo_scores %>% select(title, tmdb_score, tmdb_popularity)
hbo_crit_scores <- hbo_scores %>% select(title, imdb_score, tmdb_popularity)


g <- graph_from_data_frame(d = hbo_user_scores, directed = FALSE)
tbl_graph <- as_tbl_graph(g)
network <- ggraph(tbl_graph, layout = "fr") +
    geom_edge_link(aes(width =tmdb_popularity), edge_alpha = 0.5) +
    geom_node_point(color = "purple", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
  
ggsave("HBO User Network.png")
g <- graph_from_data_frame(d = hbo_crit_scores, directed = FALSE)
tbl_graph <- as_tbl_graph(g)
network <- ggraph(tbl_graph, layout = "fr") +
    geom_edge_link(aes(width =tmdb_popularity), edge_alpha = 0.5) +
    geom_node_point(color = "#5c1588", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
  

ggsave( "HBO Critic Network.png")
plot_ly(hbo_scores, x = hbo_scores$title, y = hbo_scores$tmdb_popularity, color = I("purple"))
#amazon prime video
ggplot(data = amazon_scores, mapping = aes(x=tmdb_score)) +
  geom_boxplot(color = "#1399FF") + 
  labs(x = "Amazon Prime Video User Score")
ggsave("Amazon Prime User Score.png")

ggplot(data = amazon_scores, mapping = aes(x=imdb_score)) +
geom_boxplot(color = "#1399FF") +
labs(x = "Amazon Prime Video Critic Score")
ggsave("Amazon Prime Critic Score.png")


imdb_amazon <- ggplot(amazon_scores, mapping = aes(x=imdb_score, y=release_year))+ 
geom_point(fill= "#0f77c7", color = "#0f77c7") + labs(x = "Amazon Critic scores", y ="Year of Release")

tmdb_amazon <- ggplot(amazon_scores, mapping = aes(x=tmdb_score, y=release_year))+ geom_point(fill = "#1399FF", color="#1399FF")  + labs(x = "Amazon User scores", y ="Year of Release")
compare_score_amazon <- imdb_amazon + tmdb_amazon
ggsave( "Amazon Comparing Score.png")
combined_imdb_and_tmbd_amazon <- imdb_amazon + geom_point(amazon_scores, mapping = aes(x=tmdb_score, y=release_year),fill = "#1399FF", color="#1399FF") + labs(x = "combined scores", y ="Year of Release")



ggsave("Amazon Combined Score.png")

ggplot(amazon_scores, mapping = aes(x = imdb_score, y= tmdb_score)) + geom_point(fill = "#1399FF", color= "#1399FF") + geom_smooth(method = "lm") 
ggsave("Amazon Scores with regression.png")
amazon_user_scores <- amazon_scores %>% select(title, tmdb_score, tmdb_popularity)
amazon_crit_scores <- amazon_scores %>% select(title, imdb_score, tmdb_popularity)


g <- graph_from_data_frame(d = amazon_user_scores, directed = FALSE)
tbl_graph <- as_tbl_graph(g)
network <- ggraph(tbl_graph, layout = "fr") +
    geom_edge_link(aes(width =tmdb_popularity), edge_alpha = 0.5) +
    geom_node_point(color = "#1399FF", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
  
ggsave( "Amazon User Network.png")
g <- graph_from_data_frame(d = amazon_crit_scores, directed = FALSE)
tbl_graph <- as_tbl_graph(g)
network <- ggraph(tbl_graph, layout = "fr") +
    geom_edge_link(aes(width =tmdb_popularity), edge_alpha = 0.5) +
    geom_node_point(color = "#0f77c7", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
  
ggsave( "Amazon Critic Network.png")

plot_ly(amazon_scores, x = amazon_scores$title, y = amazon_scores$tmdb_popularity, color = I("#1399FF"))

#disney+

ggplot(data = disney_scores, mapping = aes(x=tmdb_score)) +
  geom_boxplot(color = "blue") + 
  labs(x = "Disney+ User Score")
ggsave("Disney+ User Score.png")

ggplot(data = disney_scores, mapping = aes(x=imdb_score)) +
geom_boxplot(color = "blue") +
labs(x = "Disney+ Critic Score")
ggsave("Disney+ Critic Score.png")

imdb_disney <- ggplot(hbo_scores, mapping = aes(x=imdb_score, y=release_year))+ 
geom_point(fill= "darkblue", color = "darkblue")  + labs(x = "Disney Critic scores", y ="Year of Release")

tmdb_disney <- ggplot(disney_scores, mapping = aes(x=tmdb_score, y=release_year))+ geom_point(fill = "blue", color="blue")  + labs(x = "Disney User scores", y ="Year of Release")
compare_score_hbo <- imdb_disney + tmdb_disney
ggsave( "Disney compare score.png")

combined_imdb_and_tmbd_disney <- imdb_disney + geom_point(disney_scores, mapping = aes(x=tmdb_score, y=release_year),fill = "blue", color="blue") + labs(x = "combined scores", y ="Year of Release")



ggsave( "Disney Combined Score.png")

ggplot(disney_scores, mapping = aes(x = imdb_score, y= tmdb_score)) + geom_point(fill = "blue", color= "blue") + geom_smooth(method = "lm") 
ggsave("Disney+ Scores with regression.png")
disney_user_scores <- disney_scores %>% select(title, tmdb_score, tmdb_popularity)
disney_crit_scores <- disney_scores %>% select(title, imdb_score, tmdb_popularity)


g <- graph_from_data_frame(d = disney_user_scores, directed = FALSE)
tbl_graph <- as_tbl_graph(g)
network <- ggraph(tbl_graph, layout = "fr") +
    geom_edge_link(aes(width =tmdb_popularity), edge_alpha = 0.5) +
    geom_node_point(color = "blue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
  
ggsave( "Disney User Network.png")

g <- graph_from_data_frame(d = disney_crit_scores, directed = FALSE)
tbl_graph <- as_tbl_graph(g)
network <- ggraph(tbl_graph, layout = "fr") +
    geom_edge_link(aes(width =tmdb_popularity), edge_alpha = 0.5) +
    geom_node_point(color = "darkblue", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE) +
    theme_void()
  
ggsave( "Disney Critic Network.png")

plot_ly(disney_scores, x= disney_scores$title, y = disney_scores$tmdb_popularity, color = I("blue"))
# games
ggplot(data = games_scores, mapping = aes(User_Score)) +
  geom_boxplot(color = "gold") +
  labs(x = "Video Games User score")
ggsave("Videogames User Score.png")

ggplot(data = games_scores, mapping = aes(x=Critic_Score)) +
geom_boxplot(color = 'gold')+
labs(x = "Video Games Critic Score")
ggsave("Videogames Critic Score.png")


critc_score<- ggplot(games_scores, mapping = aes(x=Critic_Score, y=Year_of_Release))+ 
geom_point(fill= "#7e6c0a", color = "#7e6c0a")  + labs(x = "Videogames Critic scores", y ="Year of Release")

user_score <- ggplot(games_scores, mapping = aes(x=User_Score, y=Year_of_Release))+ geom_point(fill = "gold", color="gold") + labs(x = "Videogames User scores", y ="Year of Release")
compare_score_critic_and_user <- critc_score + user_score

ggsave("Videogames Comparing Score.png")
combined_user_and_critic_score <- critc_score + geom_point(games_scores, mapping = aes(x=User_Score, y=Year_of_Release),fill = "gold", color="gold") + labs(x = "Combined scores", y ="Year of Release")


ggsave( "Videogames Combined Score.png")

Total_publisher_count <- games_scores %>% group_by(Publisher)%>% summarise(Total = sum(User_Count)) 
most_famous <- Total_publisher_count %>% filter(Total > 20000)
 
plot_ly(Total_publisher_count, x = Total_publisher_count$Publisher, y = Total_publisher_count$Total, color = I("gold"))
plot_ly(most_famous, x = most_famous$Publisher, y = most_famous$Total, color = I("gold"))

# Group by Publisher and Score, and count the frequencies
score_frequencies <- games_scores %>%
  group_by(Publisher, User_Score) %>%
  summarise(Frequency = n(), .groups = 'drop')

# Create a complete grid of all publishers and scores from 1 to 10 with decimals
all_scores <- expand.grid(Publisher = unique(games_scores$Publisher), User_Score = seq(0, 10, by = 1))

# Join the complete grid with the calculated frequencies, filling in missing values with 0
score_frequencies_complete <- all_scores %>%
  left_join(score_frequencies, by = c("Publisher", "User_Score")) %>% drop_na()


g <- graph_from_data_frame(d = score_frequencies_complete, directed = FALSE)
  tbl_graph <- as_tbl_graph(g)
  graph <- ggraph(tbl_graph, layout = "kk") +
    geom_edge_link(aes(width = Frequency),edge_alpha = 0.5) +
    geom_node_point(color = "gold", size = 20) +
    geom_node_text(aes(label = name), repel = TRUE, max.overlaps = 300) +
    theme_void()

ggsave("Complete Video Games User Score.png")


# Calculate score frequencies for all publishers
score_frequencies_most_famous <- games_scores %>%
  group_by(Publisher, User_Score) %>%
  summarise(Frequency = n(), .groups = 'drop')

# Create a complete grid of all publishers from 'most_famous' and scores from 1 to 10 with decimals
all_scores_most_famous <- expand.grid(
  Publisher = unique(most_famous$Publisher),
  User_Score = seq(0, 10, by = 1)
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
graph <- ggraph(tbl_graph, layout = "fr") +
  geom_edge_link(aes(width = Frequency),edge_alpha = 0.5) +
  geom_node_point(color = "gold", size = 20) +
  geom_node_text(aes(label = name), position = "identity",repel = TRUE) +
  theme_void()

# Plot the graph

ggsave( "Most Famous Video Games User Score.png")

# Group by Publisher and Score, and count the frequencies
score_frequencies <- games_scores %>%
  group_by(Publisher,Critic_Score) %>%
  summarise(Frequency = n(), .groups = 'drop')

# Create a complete grid of all publishers and scores from 1 to 10 with decimals
all_scores <- expand.grid(Publisher = unique(games_scores$Publisher), Critic_Score = seq(0, 10, by = 1))

# Join the complete grid with the calculated frequencies, filling in missing values with 0
score_frequencies_complete <- all_scores %>%
  left_join(score_frequencies, by = c("Publisher", "Critic_Score")) %>% drop_na()


g <- graph_from_data_frame(d = score_frequencies_complete, directed = FALSE)
  tbl_graph <- as_tbl_graph(g)
  graph <- ggraph(tbl_graph, layout = "kk") +
    geom_edge_link(aes(width = Frequency/4),edge_alpha = 0.5) +
    geom_node_point(color = "#7e6c0a", size = 5) +
    geom_node_text(aes(label = name), repel = TRUE, max.overlaps = 300) +
    theme_void()

ggsave("Complete Video Games Critic Score.png")


# Calculate score frequencies for all publishers
score_frequencies_most_famous <- games_scores %>%
  group_by(Publisher, Critic_Score) %>%
  summarise(Frequency = n(), .groups = 'drop')

# Create a complete grid of all publishers from 'most_famous' and scores from 1 to 10 with decimals
all_scores_most_famous <- expand.grid(
  Publisher = unique(most_famous$Publisher),
  Critic_Score = seq(0, 10, by = 1)
)

# Join the complete grid with the calculated frequencies, filling in missing values with 0
score_frequencies_complete_most_famous <- all_scores_most_famous %>%
  left_join(score_frequencies_most_famous, by = c("Publisher", "Critic_Score")) %>%
  replace_na(list(Frequency = 0))

# Filter the data to include only publishers in 'most_famous'
filtered_data <- score_frequencies_complete_most_famous %>%
  filter(Publisher %in% most_famous$Publisher)

# Create a graph from the filtered data frame
g <- graph_from_data_frame(d = filtered_data, directed = FALSE)
# Convert to tbl_graph for ggraph
tbl_graph <- as_tbl_graph(g)




# Create a visual representation of the graph
graph <- ggraph(tbl_graph, layout = "fr") +
  geom_edge_link(aes(width = Frequency/4),edge_alpha = 0.5) +
  geom_node_point(color = "#7e6c0a", size = 20) +
  geom_node_text(aes(label = name), position = "identity",repel = TRUE) +
  theme_void()
ggsave( "Most Famous Video Games Critic Score.png")