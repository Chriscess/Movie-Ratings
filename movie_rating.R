
library(tidyverse)
library(dslabs)
library(dplyr)
library(patchwork)


movie_ratings<- read.csv("Movie-Ratings.csv")
str(movie_ratings)
view(movie_ratings)
sum(is.na(movie_ratings))
movie_ratings$Year_release<- factor(movie_ratings$Year_release)

#### Rename columns
movie_ratings<- movie_ratings %>% 
  rename(Film="Film",
         Genre="Genre",
         Rot_tomato_ratings="Rotten.Tomatoes.Ratings..",
        Audience_ratings="Audience.Ratings..",
        Budget_mil="Budget..million...",
        Year_release="Year.of.release")

### Rotten tomato ratings by genre and year of release
m1<- movie_ratings %>% 
  ggplot(aes(x=reorder(Year_release, Genre), y=Rot_tomato_ratings, fill = Genre)) +
  geom_boxplot(show.legend = TRUE)+
  labs(x= "Genre", y="Rotten tomatoes ratings", title = " Genres Tomates Ratings")+
  theme_minimal()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 17, hjust = 0.5))


###Audience ratings by genre and year of release
m2<- movie_ratings %>% 
  ggplot(aes(x=reorder(Year_release, Genre), y= Audience_ratings, fill= Genre))+
  geom_boxplot()+
  labs(x="Genre", y="Audience Ratings",
       title = "Genres Audience Ratings")+
  theme_minimal()+
  theme(axis.text = element_text(size = 10),
        axis.title=element_text(size=10),
        plot.title=element_text(size=17, hjust = 0.5))

m1 /m2



Movie_count<- movie_ratings %>% 
  group_by(Genre) %>% 
  summarise(mean= mean(Budget_mil),
            median= median(Budget_mil),
            std=sd(Budget_mil),
            maximum= max(Budget_mil),
            minimum= min(Budget_mil),
            count=n())
  
### Between 2007 and 2011 the highest genre released is comedy, however it does not have the highest budget

ggplot(Movie_count, aes(x = Genre, y = count, group = 1)) +
  geom_point(size=4)+
  geom_line(color="blue")+
  labs(title = "Movie Counts by Genre", x = "Count", y = "Genre")+
  labs(x="Genre", y= "Number of Movies",
       title= "Total Number of Movies Released between 2007-2011")+
  theme_minimal()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 17, hjust = 0.5))

### Between 2007 and 20011 the Genre with the highest budget is Action movies 
ggplot(Movie_count, aes(x = mean, y=fct_reorder(Genre, mean, .desc = TRUE),fill = Genre,
)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = " Movie Budget in Millions", x = " Budget (in millions)", y = " Genre")+
  theme_minimal()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 17, hjust = 0.5))

 #### Barvchart showing movie budget by year of release
ggplot(movie_ratings,aes(x=Year_release, y= Budget_mil,fill=Genre))+
  geom_bar(stat="identity", position = "dodge")+
scale_fill_brewer(palette = "Dark2")+
  labs(title = " Movie Budget in Millions", x = " Year of Release", y = " Budget(in millions)")+
  theme_minimal()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 17, hjust = 0.5))


#### Number of movies released in a year
num_movies_per_year <- movie_ratings %>%
  group_by(Year_release) %>%
  summarize(num_movies = n())

ggplot(num_movies_per_year,aes(x=Year_release,y= num_movies, fill = Year_release))+
  geom_bar(stat = 'identity', show.legend = FALSE, width = 0.7)+
  labs(x="Year of Release", y= "Number of Movies",
       title= "Total Number of Movies Released in a Year")+
  theme_minimal()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 17, hjust = 0.5))

####### Number of Genre released per year

ggplot(movie_ratings,aes(x=Year_release,fill=Genre))+
  geom_bar(position = "dodge")+
  scale_fill_brewer(palette = "Dark2")+
  labs(x="Year of Release", y= "Number of Genre",
       title= "Total Number of Genre Released in a Year")+
  theme_minimal()+
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.title = element_text(size = 17, hjust = 0.5))