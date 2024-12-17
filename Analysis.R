#Final Project Analysis

library(tidyverse)
library(dplyr)
library(modelr)


view(films)

#Cleaning data:
films_tidy<-films%>%
  #removing any complete repeats:
  distinct() %>% 
  #Renaming column headings:
  rename(Title=MOVIES,Genre=GENRE,Rating=RATING,Votes=VOTES,OneLine=`ONE-LINE`) %>%
  #Creating a new column called DIRECTOR; if there is no "Director:..." then row will say "Not listed"
  mutate( DIRECTOR=if_else(str_detect(STARS, "(?i)Director:"), str_extract(STARS, "(?<=Director:)[^\\|]+"), "Not listed"),
         #modifying the STARS column: removing the director info, the and "Star(s):..."
         STARS=if_else(str_detect(STARS, "\\|"), str_split(STARS, "\\|", simplify = TRUE)[, 2], STARS),
         #addresses the case if there are no stars listed:
         STARS = if_else(!str_detect(STARS, "(?i)star") & !is.na(DIRECTOR), "Not listed", STARS),
         STARS=gsub("Stars:","",STARS),
         STARS=gsub("Star:","",STARS),
         #creating a new column (Type) that tells you if the piece of media is a show or a movie based on if there is a director listed or not.
         Type=if_else(DIRECTOR!="Not listed","Movie","TV Show"),
         #removing the weird symbols and any letters from YEAR:
         YEAR=(gsub("[^0-9]", "", YEAR)),
         #creating two new columns, using year; if it is a movie or only aired for a year the StartYear and EndYear are the same.
         StartYear=ifelse(nchar(YEAR)==4, YEAR, substr(YEAR, 1, 4)),
         EndYear=ifelse(nchar(YEAR)==4, YEAR, substr(YEAR, 5, 8)),
         #addressing the NA values in Gross; if it is a TV show I put "Not applicable" and if it is a movie with NA gross I put "Unknown":
         Gross=ifelse(Type == "TV Show", "Not applicable", ifelse(Type == "Movie" & is.na(Gross), "Unknown", as.character(Gross)))) %>%
  #renaming to match the rest of the data:
  rename(Stars=STARS,Director=DIRECTOR)%>%
  #grouping multiple seasons of a show into one row:
  group_by(Title)%>%
  #setting all the values for the modified and unmodified rows:
  summarize(
    Type=first(Type),
    StartYear=first(StartYear),
    EndYear=first(EndYear),
    Genre=first(Genre), 
    OneLine=first(OneLine),
    #if the seasons have different ratings: just used the mean
    Rating=round(mean(Rating, na.rm = TRUE), 2), 
    Votes=first(Votes),
    Gross=first(Gross),
    Director=first(Director),
    #don't want to repeat stars:
    Stars=paste(unique(Stars), collapse = ", "),
    RunTime=first(RunTime))%>%
  ungroup() %>%
  #looking at the success of movies and actors; don't need to consider movies that rank extremely low on popularity
  #When ranking by highest votes, all media after 1000 I had never heard of, but I showed top 2000 just for more data:
  arrange(desc(Votes)) %>%
  head(2000)
view(films_tidy)


#Analysis:

#Variable analysis:
#Title: title of the piece of media.(chr)
#Type: specifies whether the piece of media is a movie or a TV show.(chr)
#StartYear: The year the movie was released or the TV aired.(chr)
#EndYear:The year the TV show stopped airing.(chr)
#Genre: List of categories that the piece of media falls into.(chr)
#OneLine:A concise description of the TV show or movie.(chr)
#Rating:The average rating of the TV show or movie. (dbl)
#Votes: How many individuals voted for the rating. (dbl)
#Gross: The total earnings of the media, seems to be box-office gross since only movies have it.(chr)
#Director: The individual who directed the movie; TV shows do not have a director listed.(chr)
#Stars: The actors and actresses who starred in the TV show or movie. (chr)
#RunTime: The length of the movie or the average length of an episode if it is a show.

#StartYear and EndYear are characters because they were treated as strings when the columns were created.(dbl)

#Any correlation between variables?

#relationship between rating and votes:
ggplot(films_tidy, aes(x=Votes, y=Rating)) +
  geom_point() +
  #a lot of outliers; there doesn't seem to be any sort of relationship
  coord_cartesian(xlim=c(0,50000))+
  ggtitle("Relationship Between Votes and Rating")


#creating a new row that considers votes and rating:
films_tidy<-films_tidy %>%
  mutate(CompositeScore=round((Rating*Votes)/max(Votes),2)) %>%
  arrange(desc(CompositeScore))
view(films_tidy)

#is there a relationship between RunTime and Votes, how about Runtime and Rating?
ggplot(films_tidy, aes(x=RunTime, y=Votes))+
  geom_point()+
  coord_cartesian(xlim=c(0,250),ylim=c(0,55000))+
  ggtitle("Relationship Between RunTime and Votes")
ggplot(films_tidy, aes(x=RunTime, y=Rating))+
  geom_point()+
  coord_cartesian(xlim=c(0,250))+
  ggtitle("Relationship Between RunTime and Rating")
#no visible correlation

#relationship between Votes and gross:
gross_numeric <- films_tidy%>%
  mutate(Gross=as.numeric(gsub("[^0-9.]", "", films_tidy$Gross)))
  gross_numeric %>%
  ggplot(aes(x=Votes,y=Gross))+
  geom_point()+
    geom_smooth()+
    coord_cartesian(xlim=c(0,550000),ylim=c(0,100))+
    labs(x="Number of Votes",y="Gross (in millions)")+
    ggtitle("Relationship between Gross and Votes")
#it looks like there could potentially be a positive correlation between Votes and Gross.
#Votes is a good predictor for Gross.  

#Relationships between StartYear and Votes/Rating
films_tidy$StartYear<-as.numeric(films_tidy$StartYear)
films_tidy$EndYear<-as.numeric(films_tidy$EndYear)  
films_tidy_years <- films_tidy%>%
  mutate(StartYearInterval=cut(StartYear, breaks = seq(min(StartYear), max(StartYear) + 5, by = 5), include.lowest = TRUE)) %>%
  group_by(StartYearInterval) %>%
  summarise(avg_votes=mean(Votes, na.rm = TRUE),
            avg_rating=mean(Rating, na.rm=TRUE))
ggplot(films_tidy_years, aes(x = StartYearInterval, y = avg_votes)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Release Year", y = "Average Votes") +
  coord_flip()+
  ggtitle("Average Votes by Release Year")
#pieces of media that were released from 1972-1977 have the most votes; the interval with the second most votes is 1997-2002
ggplot(films_tidy_years, aes(x = StartYearInterval, y = avg_rating)) +
  geom_bar(stat = "identity", color = "black") +
  labs(x = "Release Year", y = "Average Rating") +
  coord_flip()+
  ggtitle("Average Rating by Release Year")
#the release year doesn't seem to have any affect on the rating

#distributions of ratings per genre (I look into this deeper further down): 
films_tidy_genre<-films_tidy%>%
  separate_rows(Genre, sep = ", ")
films_tidy_genre

ggplot(films_tidy_genre, aes(x=Genre, y=Rating)) +
  geom_boxplot() +
  coord_flip() 

#viewing the top rated pieces of media:
films_tidy_rating<-films_tidy%>%
  arrange(desc(Rating))

view(films_tidy_rating)

#compared to ranked by votes:
view(films_tidy)

#looking at how Type correlates to other variables:
#There is a higher percentage of movies when looking at media when the most votes
#This is a higher percentage of TV Shows when looking at media with the highest rankings.
#Further proving this:

#Making two separate data sets: one for movies one for shows:
movies<-films_tidy %>% filter(Type=="Movie")
tv_shows<-films_tidy %>% filter(Type=="TV Show")
view(movies)
view(tv_shows)

#how ratings differ between shows and movies:
summary(movies$Rating)
summary(tv_shows$Rating)


#how votes differ between shows and movies:
summary(movies$Votes)
summary(tv_shows$Votes)


#how composite scores differ between different types of media:
summary(movies$CompositeScore)
summary(tv_shows$CompositeScore)
films_tidy<-films_tidy%>%
  filter(!is.na(Type))
par(mfrow=c(1,3))
ggplot(films_tidy, aes(x=Type, y=Rating))+
  geom_boxplot()+
  labs(title="Ratings amongst Types")
ggplot(films_tidy, aes(x=Type, y=Votes))+
  geom_boxplot()+
  coord_cartesian(ylim=c(0,500000))+
  labs(title="Votes amongst Types")
ggplot(films_tidy, aes(x=Type, y=CompositeScore))+
  geom_boxplot()+
  labs(title="Composite Score amongst Types")+
  coord_cartesian(ylim=c(0,4.0))

#movies have higher votes, tv shows, have higher ratings, but movies have a higher composite score.
#overall, movies tend to perform just a little bit better than TV shows.
#is the duration of a TV show correlated with rating or votes?
tv_shows <- tv_shows %>%
  mutate(Duration=as.numeric(EndYear)-as.numeric(StartYear))
ggplot(tv_shows, aes(x = Duration, y = Rating)) +
  geom_point() +
  labs(x = "Duration (Years)") +
  ggtitle("Relationship between TV Show Duration and Rating")
ggplot(tv_shows, aes(x = Duration, y = Votes)) +
  geom_point() +
  coord_cartesian(ylim=c(0,55000))+
  labs(x = "Duration (Years)") +
  ggtitle("Relationship between TV Show Duration and Votes")

#distributions of ratings per genre: 
films_tidy_genre<-films_tidy%>%
  separate_rows(Genre, sep = ", ")%>%
  arrange(desc(Rating))
films_tidy_genre

ggplot(films_tidy_genre, aes(x=Votes, y=Genre)) +
  geom_boxplot() +
  coord_cartesian(xlim=c(0,300000))+
  labs(title="Votes by Genre")

#looking at genres with highest composite score medians:
genre_cs<-films_tidy_genre%>%
  group_by(Genre)%>%
  summarize(median_votes=median(Votes, na.rm=TRUE))%>%
  ungroup()%>%
  arrange(desc(median_votes))
#genres with highest composite score medians: musical, adventure, mystery, sci-fi, action, fantasy, etc.
genre_cs

#looking at ratings, rather than CS:
genre_ratings<-films_tidy_genre%>%
  group_by(Genre)%>%
  summarize(median_rating=median(Rating, na.rm=TRUE))%>%
  ungroup()%>%
  arrange(desc(median_rating))
#overall the genres with the highest ratings are: talk-shows, news, action, animation, reality-tv, documentaries, etc.
genre_ratings

#a lot of these are genres specific to shows; since movies perform overall better than shows, lets see what genres do best with movies:
movie_genre<-movies%>%
  separate_rows(Genre, sep = ", ")
movie_genre_ratings<-movie_genre%>%
  group_by(Genre)%>%
  summarize(median_votes=median(Votes, na.rm=TRUE))%>%
  ungroup()%>%
  arrange(desc(median_votes))
movie_genre %>%
  ggplot(aes(x=Votes,y=Genre))+
  geom_boxplot()+
  coord_cartesian(xlim=c(0,300000))+
  labs(title="Votes by Genre")

#the top rated movie genres are news, short, film-noir, action, animation, documentary, etc.
movie_genre_ratings
#lets look at the ranking of movie genres based on composite score:
movie_genre_cs<-movie_genre%>%
  group_by(Genre)%>%
  summarize(median_cs=median(CompositeScore, na.rm=TRUE))%>%
  ungroup()%>%
  arrange(desc(median_cs))
#the movie genres with the highest scores are: musical, family, film-noir, mystery, adventure, sci-fi,animation, and fantasy.
movie_genre_cs


#linear regression model of composite score: COME BACK TO THIS
#films_model<-lm(CompositeScore ~ Genre + Director + StartYear + EndYear, data = films_tidy)
#summary(films_model)

#analyzing individual actors and directors:

#Doing an overall look at actors:
films_tidy_stars<-films_tidy%>%
  separate_rows(Stars, sep = ", ")
films_tidy_stars

actor_performance<-films_tidy_stars%>%
  group_by(Stars) %>%
  summarize(appearances = n(),                         
    avg_rating=mean(Rating, na.rm = TRUE),  
    total_votes=sum(Votes, na.rm = TRUE),  
    Genre=Genre,
    StartYear=StartYear,
    Director=Director,
    total_gross=sum(as.numeric(gsub("[^0-9.]", "", Gross)), na.rm = TRUE)) %>%
  arrange(desc(appearances))
actor_performance

#no correlation between number of appearances and high ratings and high votes.
#playing many roles does not make a successful actor
#rating reflects on talent while number of votes represents how famous the person is

actor_performance%>%
  ggplot(aes(x=appearances,y=avg_rating))+
  geom_point()+
  coord_cartesian(xlim=c(0,10))+
  labs(x = "Number of Appearances", y = "Average Rating", title = "Actor Performance: Appearances vs Average Rating")

actor_performance%>%
  ggplot(aes(x=appearances,y=total_votes))+
  geom_point()+
  coord_cartesian(ylim=c(0,400000),xlim=c(0,10))+
  labs(x = "Number of Appearances", y = "Total Votes", title = "Actor Performance: Appearances vs Total Votes")


actor_performance%>%
  arrange(desc(avg_rating))

actor_performance%>%
  arrange(desc(total_votes))

actor_performance%>%
  arrange(desc(total_gross))

top_actors <- actor_performance %>%
  arrange(desc(total_votes)) %>%
  head(100)
view(top_actors)
actor_genre <- top_actors %>%
  separate_rows(Genre, sep = ", ") %>%
  group_by(Stars, Genre) %>%
  summarise(Appearances = n())
genre_frequencies <- actor_genre %>%
  group_by(Genre) %>%
  summarise(Frequency = sum(Appearances)) %>%
  arrange(desc(Frequency))
genre_frequencies%>%
  ggplot(aes(x = reorder(Genre, Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Genre Frequencies among Top Actors by Votes",
       x = "Genre",
       y = "Frequency")

actor_years<-top_actors%>%
  group_by(Stars,StartYear)%>%
  summarise(Appearances=n())
year_frequencies<-actor_years%>%
  group_by(StartYear) %>%
  summarise(Frequency = sum(Appearances)) %>%
  arrange(desc(Frequency))
year_frequencies%>%
  ggplot(aes(x = reorder(StartYear, Frequency), y = Frequency)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title = "Release Year Frequencies among Top Actors by Votes",
       x = "Year",
       y = "Frequency")
#Looking at data from the top 100 actors with the most votes; a high proportion of these shows and movies were released in 2009. 
#There is a correlation between being in a piece of media released in 2017 and being considered a famous actor (ranked with the 30 highest votes)

actor_director<-top_actors%>%
  group_by(Stars,Director)%>%
  summarise(Appearances=n())
director_frequencies<-actor_director%>%
  group_by(Director) %>%
  summarise(Frequency = sum(Appearances)) %>%
  arrange(desc(Frequency))
director_frequencies%>%
  ggplot(aes(x = reorder(Director, Frequency), y = Frequency)) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title = "Director Frequencies among Top Actors by Votes",
       x = "Director",
       y = "Frequency")

#Most of these pieces of media are TV shows because there is no director listed.
#However, starring in a film directed by Peter Jackson, Quentin Tarantino, Martin Scorsese, Kazuya Murata, Zack Snyder, etc. will increase a stars success.


#looking at specific actors and directors:
#Peter Jackson directs the top three movies with the highest composite score:
director_name<-"Peter Jackson"
director_data<-movies%>%
  filter(str_detect(Director, director_name))
director_data

ggplot(director_data, aes(x=Genre))+
  geom_bar()
#action, adventure, fantasy, and drama are Jackson's focused genres.

#Elijah Wood also stars in these three successful movies:
actor_name<-"Elijah Wood"
actor_data<-films_tidy%>%
  filter(str_detect(Stars, actor_name))
actor_data
#his most frequent genres are action, adventure, and drama; but he also stars in comedy and romance movies.
ggplot(actor_data, aes(x=Type,y=Votes))+
  geom_bar(stat="identity")+
  coord_cartesian(ylim=c(0,300000))+
  labs(title="Elijah Wood: Votes by Types of Media")
ggplot(actor_data, aes(x=))
#lets see if he has starred in any shows:
actor_data_shows<-tv_shows%>%
  filter(str_detect(Stars, actor_name))
#he has acted in one TV show on this list:
actor_data_shows
actor_data_shows$CompositeScore #low composite score
actor_data_shows$Rating #has a great rating, but
actor_data_shows$Votes #low votes
#his movies highly outperformed his tv shows

#creating model from actor_performance data set:
cor_df<- cor(actor_performance[, c( 'avg_rating','total_votes', 'total_gross')])
success_model<-lm(total_votes ~ avg_rating + total_gross + Genre + StartYear + Director, data = actor_performance)
summary(success_model)
grid <- actor_performance %>% 
  data_grid(avg_rating, .model = success_model) %>% 
  add_predictions(success_model)
ggplot(grid,aes(avg_rating, pred))+
  geom_point()+
  labs(title="Actor Success: A Linear Regression Model")
