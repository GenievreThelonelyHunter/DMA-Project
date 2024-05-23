library(ggplot2)
library(tidyverse)


netflix <- read.csv("C:/Users/Alessandro Caminiti/Desktop/per il progetto di dma/Netflix.csv")
names(netflix)
scores <- netflix%>%select(title,imdb_score,tmdb_score,tmdb_popularity)

ggplot(data = scores, mapping = aes(x=imdb_score,y =tmdb_score)) +
  geom_point()


ggplot(data = scores, mapping = aes(x=imdb_score)) +
  geom_histogram()

ggplot(data = scores, mapping = aes(x=tmdb_score)) +
  geom_histogram()

scores <- scores%>%drop_na()

ggplot(data = scores, mapping = aes(x=tmdb_score)) +
  geom_histogram()


ggplot(data = scores, mapping = aes(x=imdb_score)) +
geom_histogram()


ggsave("scatterplot.png", width = 9, height = 7, dpi = 1000)
ggplot(data = scores, mapping = aes(x=tmdb_score)) +
  geom_histogram()
ggplot(data = scores, mapping = aes(x=imdb_score,y =tmdb_score)) +
geom_point()



