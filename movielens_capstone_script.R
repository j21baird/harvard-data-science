# Note: this process could take a couple of minutes

# Install all necessary libraries if it is not present

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

#load all necessary libraries
library(tidyverse)
library(caret)
library(data.table)

#######

# Create 'edx' training dataset and 'validation' test dataset

#######

# Note: this process could take a couple of minutes

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

### Exploratory Data Analysis
## Understand the Data

dim(edx)
dim(validation)

# look at top 10 rows

head(edx, n = 10)
head(validation, n = 10)

#Missing Data check
sum(complete.cases(edx))
sum(complete.cases(validation))

#how many users and how many movies
edx %>% summarize(userids = n_distinct(userId),
                 movieids = n_distinct(movieId))

#determine class and further review dataset
glimpse(edx)

##Preprocess data to begin exploratory visualizations
#Convert timestamp to date and time for both test and validation set
edx$date <- as.POSIXct(edx$timestamp, origin="1970-01-01")
validation$date <- as.POSIXct(validation$timestamp, origin="1970-01-01")

#grab year, month, and hour of day from date and time variables
edx$rating_year <- as.numeric(format(edx$date, "%Y"))
edx$rating_month <- as.numeric(format(edx$date, "%m")) 
edx$rating_hour <- as.numeric(format(edx$date, "%H"))
validation$rating_year <- as.numeric(format(validation$date, "%Y"))
validation$rating_month <- as.numeric(format(validation$date, "%m")) 
validation$rating_hour <- as.numeric(format(validation$date, "%H"))

#Extract the movie's release year from the title
edx <- edx %>%
  mutate(title = str_trim(title)) %>%
  extract(title,
          into = c("Title", "movie_release"),             
          regex = "^(.*) \\(([0-9]{4})\\)$",
          remove = FALSE ) %>%
  mutate(Title = str_trim(Title, side = "both")) %>%
  select(-title)

validation <- validation %>%
  mutate(title = str_trim(title)) %>%
  extract(title,
          into = c("Title", "movie_release"),             
          regex = "^(.*) \\(([0-9]{4})\\)$",
          remove = FALSE ) %>%
  mutate(Title = str_trim(Title, side = "both")) %>%
  select(-title)


#trim whitespace for movie and new title (if any was there)
edx$Title <- str_trim(edx$Title, side = "both")
edx$movie_release <- str_trim(edx$movie_release, side = "both")
validation$Title <- str_trim(validation$Title, side = "both")
validation$movie_release <- str_trim(validation$movie_release, side = "both")

#convert movie release to numeric vector
edx$movie_release <- as.numeric(edx$movie_release)
validation$movie_release <- as.numeric(validation$movie_release)

# Create new column indicating how many years between the release of the movie and the rating
edx <- edx %>%
     mutate(review_minus_release_year = rating_year - movie_release)
validation <- validation %>%
     mutate(review_minus_release_year = rating_year - movie_release)

# separate genre rows based based on vertical bar
edx <- edx %>%
       separate_rows(genres,
                     sep = "\\|")

validation <- validation %>%
  separate_rows(genres,
                sep = "\\|")

#How many rows now?
dim(edx)
dim(validation)

# Remove unwanted columns
edx <- edx %>%
  select(userId, movieId, rating, Title, movie_release, genres, rating_year, rating_month, rating_hour, review_minus_release_year)

validation <- validation %>%
  select(userId, movieId, rating, Title, movie_release, genres, rating_year, rating_month, rating_hour, review_minus_release_year)


### Data Visualization - analyzing frequency distribution and distance between categorical variables
## Users affect on rating
# how many users
length(unique(edx$userId))
#average number of views per user
edx_userid_ratings <- edx %>%
  group_by(userId) %>%
  summarize(total_ratings = n())
mean(edx_userid_ratings$total_ratings)
median(edx_userid_ratings$total_ratings)

edx %>%
  group_by(userId) %>%
  summarize(count_userId = n()) %>%
  ggplot(aes(x = count_userId)) +
  geom_histogram(bins = 30)

# avg rating from users
edx %>%
  group_by(userId) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x = mean_rating)) + 
  geom_histogram(bins = 20)

#top 35 users based on number of ratings - table
top_35_users_table <- edx %>%
  group_by(userId) %>%
  summarize(avg_rating = mean(rating),
            total_ratings = n()) %>%
  arrange(desc(total_ratings)) %>%
  top_n( 35)
#filter users that are in this top table
top_35_users_data <- edx %>%
     filter(userId %in% top_35_users_table$userId)

#top 35 users based on number of ratings - bar chart
top_35_users_data %>%
  ggplot(aes(y = rating)) + 
  geom_bar()
#top 35 users based on number of ratings - boxplot with mean highlighted
top_35_users_data %>%
  ggplot(aes(x = as.factor(userId), y = rating)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="red", fill="red") 

# 30 users around the median number of ratings per user - showcase variability by user with a normal number of film ratings 
median_30_users_table <- edx %>%
  group_by(userId) %>%
  summarize(avg_rating = mean(rating),
            total_ratings = n()) %>%
  filter(total_ratings >= 162 & total_ratings <= 165) %>%
  arrange(desc(total_ratings)) 
median_30_users_table <- median_30_users_table[c(1:17, 601:613),]
# View avg rating table
median_30_users_table

#filter rows including 30 avg users ratings
median_30_users_data <- edx %>%
  filter(userId %in% median_30_users_table$userId)

#30 avg users based on number of ratings - bar chart
median_30_users_data %>%
  ggplot(aes(y = rating)) + 
  geom_bar()

#30 avg users based on number of ratings - boxplot with mean highlighted
median_30_users_data %>%
  ggplot(aes(x = as.factor(userId), y = rating)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="red", fill="red") 
##### Based on User ID exploratory data, we can conclude that UserID does explain some of the variabiality in ratings - add to model

## movie affect on rating
#how many movies
length(unique(edx$movieId))
#reviews per movie
edx %>%
  group_by(movieId) %>%
  summarize(count_movieId = n()) %>%
  ggplot(aes(x = count_movieId)) +
  geom_histogram(bins = 10)

#top 35 movies - table
top_35_movies_table <- edx %>%
  group_by(movieId) %>%
  summarize(avg_rating = mean(rating),
            total_ratings = n()) %>%
  arrange(desc(total_ratings)) %>%
  top_n( 35)

#all rows from edx on top 35 movies
top_35_movies_data <- edx %>%
  filter(movieId %in% top_35_movies_table$movieId)

#top 35 movies based on number of reviews - bar chart
top_35_movies_data %>%
  ggplot(aes(y = rating)) + 
  geom_bar()
#top 35 movies based on number of reviews - boxplot with mean highlighted
top_35_movies_data %>%
  ggplot(aes(x = as.factor(movieId), y = rating)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="red", fill="red") 

# median number of views per movie
movie_info <- edx %>%
  group_by(movieId) %>%
  summarize(total_ratings = n())
median(movie_info$total_ratings)

# 30 movies around the median number of ratings per movie - showcase variability by movie with a normal number of film ratings 
median_30_movie_table <- edx %>%
  group_by(movieId) %>%
  summarize(avg_rating = mean(rating),
            total_ratings = n()) %>%
  filter(total_ratings >= 200 & total_ratings <= 206) %>%
  arrange(desc(total_ratings)) 

median_30_movie_table <- median_30_movie_table[c(1:17, 59:72),]
# View median movie rating table
median_30_movie_table

#filter rows including 30 avg movies ratings
median_30_movie_data <- edx %>%
  filter(movieId %in% median_30_movie_table$movieId)

#30 avg movies based on number of ratings - bar chart
median_30_movie_data %>%
  ggplot(aes(y = rating)) + 
  geom_bar()

#30 avg movies based on number of ratings - boxplot with mean highlighted
median_30_movie_data %>%
  ggplot(aes(x = as.factor(movieId), y = rating)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="red", fill="red")

# avg rating by movie title
edx %>%
  group_by(movieId) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x = mean_rating)) + 
  geom_histogram(bins = 20)

## Genre Analysis 
#how many genres
length(unique(edx$genres))

#total reviews per genre
edx %>%
  group_by(genres) %>%
  summarize(total_ratings = n()) %>%
  ggplot(aes(x = reorder(genres, -total_ratings), y = total_ratings)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90))

#avg rating by genre
edx %>%
  group_by(genres) %>%
  summarize(mean_rating = mean(rating)) %>%
  ggplot(aes(x = mean_rating)) + 
  geom_histogram(bins = 5)

edx %>%
  ggplot(aes(x = as.factor(genres), y = rating)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="red", fill="red")

## Time difference between review and movie release analysis
# how many unique years between the review of a movie and release of a movie
length(unique(edx$review_minus_release_year))
# what is the longest time between a review and the release of a movie (in years)
max(edx$review_minus_release_year)
#what is the shortest amount of time between a review and the release of a movie (in years)
min(edx$review_minus_release_year)
#### Odd that some movies were reviewed prior to their release date
#median amount of time between a movie review and movies release
median(edx$review_minus_release_year)

# top 15 years of difference between rating and movie release - showcase variability
top_15years_release_table <- edx %>%
  group_by(review_minus_release_year) %>%
  summarize(avg_rating = mean(rating),
            total_ratings = n()) %>%
  arrange(desc(total_ratings)) %>%
  top_n(15)

# View top 15 year difference table
top_15years_release_table

#filter rows including top 15 years
top_15years_release_data <- edx %>%
  filter(review_minus_release_year %in% top_15years_release_table$review_minus_release_year)

#top 15 years rating based on number of reviews - bar chart
top_15years_release_data %>%
  ggplot(aes(y = rating)) + 
  geom_bar()

#top 15 years difference  - boxplot with mean highlighted
top_15years_release_data %>%
  ggplot(aes(x = as.factor(review_minus_release_year), y = rating)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="red", fill="red")

# bottom 15 years of difference between rating and movie release - showcase variability
bot_15years_release_table <- edx %>%
  group_by(review_minus_release_year) %>%
  summarize(avg_rating = mean(rating),
            total_ratings = n()) %>%
  arrange(desc(total_ratings)) %>%
  top_n(-15)

# View bottom 15 year difference table
bot_15years_release_table

#filter rows including bottom 15 years
bot_15years_release_data <- edx %>%
  filter(review_minus_release_year %in% bot_15years_release_table$review_minus_release_year)

#bottom 15 years rating based on number of reviews - bar chart
bot_15years_release_data %>%
  ggplot(aes(y = rating)) + 
  geom_bar()

#bottom 15 years difference  - boxplot with mean highlighted
bot_15years_release_data %>%
  ggplot(aes(x = as.factor(review_minus_release_year), y = rating)) + 
  geom_boxplot() + 
  stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="red", fill="red")


### Reducing RMSE on the validation set
## Define RMSE
# The RMSE function that will be used in this project is:
RMSE <- function(true_ratings = NULL, predicted_ratings = NULL) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Naive - Mean Model - 1.05
mu_hat <- mean(edx$rating)
rmse_mean_model <- RMSE(validation$rating, mu_hat)

#store results in a dataframe
results_df <- data.frame(model_name = "Mean Model", RMSE = rmse_mean_model, beta_hat = 0)
results_final <- data.frame(model_name = "Mean Model", RMSE = rmse_mean_model, beta_hat = 0)

### variable selection method - Forward Selection
## Determine the first variable that reduces RMSE by greatest amount 
# userId
avg_user_rating <- edx %>%
  group_by(userId) %>%
  summarize(b1 = mean(rating - mu_hat))

rmse_mean_user_model <- validation %>%
  left_join(avg_user_rating, by = "userId") %>%
  mutate(y_hat = mu_hat + b1) %>%
  select(y_hat)

rmse_mean_user_model_result <- RMSE(validation$rating, rmse_mean_user_model$y_hat)

results_df <- results_df %>% add_row(model_name = "mean_userId", RMSE = rmse_mean_user_model_result, beta_hat = 1)

#movieId

avg_movie_rating <- edx %>%
  group_by(movieId) %>%
  summarize(b1 = mean(rating - mu_hat))

rmse_mean_movie_model <- validation %>%
  left_join(avg_movie_rating, by = "movieId") %>%
  mutate(y_hat = mu_hat + b1) %>%
  select(y_hat)

rmse_mean_movie_model_result <- RMSE(validation$rating, rmse_mean_movie_model$y_hat)
results_df <- results_df %>% add_row(model_name = "mean_movieId", RMSE = rmse_mean_movie_model_result, beta_hat = 1)

#genres

avg_genre_rating <- edx %>%
  group_by(genres) %>%
  summarize(b1 = mean(rating - mu_hat))

rmse_mean_genre_model <- validation %>%
  left_join(avg_genre_rating, by = "genres") %>%
  mutate(y_hat = mu_hat + b1) %>%
  select(y_hat)

rmse_mean_genres_model_result <- RMSE(validation$rating, rmse_mean_genre_model$y_hat)
results_df <- results_df %>% add_row(model_name = "mean_genre", RMSE = rmse_mean_genres_model_result, beta_hat = 1)

# release date vs review date 

avg_releasereview_rating <- edx %>%
  group_by(review_minus_release_year) %>%
  summarize(b1 = mean(rating - mu_hat))

rmse_mean_releasereview_model <- validation %>%
  left_join(avg_releasereview_rating, by = "review_minus_release_year") %>%
  mutate(y_hat = mu_hat + b1) %>%
  select(y_hat)

rmse_mean_releasereview_model_result <- RMSE(validation$rating, rmse_mean_releasereview_model$y_hat)
results_df <- results_df %>% add_row(model_name = "mean_releasereview", RMSE = rmse_mean_releasereview_model_result, beta_hat = 1)

#evaluate B1 model performances
results_df
# First variable in algorithm is movieId
avg_movie_rating <- edx %>%
  group_by(movieId) %>%
  summarize(b1 = mean(rating - mu_hat))

rmse_mean_movie_model <- validation %>%
  left_join(avg_movie_rating, by = "movieId") %>%
  mutate(y_hat = mu_hat + b1) %>%
  select(y_hat)

rmse_mean_movie_model_result <- RMSE(validation$rating, rmse_mean_movie_model$y_hat)

results_final <- results_final %>% add_row(model_name = "mean_movie", RMSE = rmse_mean_movie_model_result, beta_hat = 1)

## Determine second variable in algorithm that reduces RMSE by greatest amount

# userId
avg_user_rating <- edx %>%
  left_join(avg_movie_rating, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b2 = mean(rating - mu_hat - b1))

rmse_mean_movie_user_model_pred <- validation %>%
  left_join(avg_movie_rating, by = "movieId") %>%
  left_join(avg_user_rating, by = "userId") %>%
  mutate(y_hat = mu_hat + b1 + b2) %>%
  select(y_hat)

rmse_mean_movie_user_model_result <- RMSE(validation$rating, rmse_mean_movie_user_model_pred$y_hat)

results_df <- results_df %>% add_row(model_name = "mean_movie_user", RMSE = rmse_mean_movie_user_model_result, beta_hat = 2)

# genres
avg_genre_rating <- edx %>%
  left_join(avg_movie_rating, by = "movieId") %>%
  group_by(genres) %>%
  summarize(b2 = mean(rating - mu_hat - b1))

rmse_mean_movie_genre_model_pred <- validation %>%
  left_join(avg_movie_rating, by = "movieId") %>%
  left_join(avg_genre_rating, by = "genres") %>%
  mutate(y_hat = mu_hat + b1 + b2) %>%
  select(y_hat)

rmse_mean_movie_genre_model_result <- RMSE(validation$rating, rmse_mean_movie_genre_model_pred$y_hat)

results_df <- results_df %>% add_row(model_name = "mean_movie_genre", RMSE = rmse_mean_movie_genre_model_result, beta_hat = 2)

#release date vs review date
avg_releasereview_rating <- edx %>%
  left_join(avg_movie_rating, by = "movieId") %>%
  group_by(review_minus_release_year) %>%
  summarize(b2 = mean(rating - mu_hat - b1))

rmse_mean_movie_releasereview_model_pred <- validation %>%
  left_join(avg_movie_rating, by = "movieId") %>%
  left_join(avg_releasereview_rating, by = "review_minus_release_year") %>%
  mutate(y_hat = mu_hat + b1 + b2) %>%
  select(y_hat)

rmse_mean_movie_releasereview_model_result <- RMSE(validation$rating, rmse_mean_movie_releasereview_model_pred$y_hat)

results_df <- results_df %>% add_row(model_name = "mean_movie_releasereview", RMSE = rmse_mean_movie_releasereview_model_result, beta_hat = 2)

#Review 2nd variable affect on algorithm
results_df

# Second variable in model is userId
avg_user_rating <- edx %>%
  left_join(avg_movie_rating, by = "movieId") %>%
  group_by(userId) %>%
  summarize(b2 = mean(rating - mu_hat - b1))

rmse_mean_movie_user_model_pred <- validation %>%
  left_join(avg_movie_rating, by = "movieId") %>%
  left_join(avg_user_rating, by = "userId") %>%
  mutate(y_hat = mu_hat + b1 + b2) %>%
  select(y_hat)

rmse_mean_movie_user_model_result <- RMSE(validation$rating, rmse_mean_movie_user_model_pred$y_hat)

results_final <- results_final %>% add_row(model_name = "mean_movie_user", RMSE = rmse_mean_movie_user_model_result, beta_hat = 2)

### Determine if regularization is needed - since we saw large discrepencies between ratings per user and ratings per movie
movie_titles <- edx %>% 
  select(movieId, Title) %>%
  distinct()

# Best performing predictions via movieId model
edx %>% count(movieId) %>%
  left_join(avg_movie_rating) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b1)) %>% 
  select(Title, b1, n) %>% 
  slice(1:10)
# worst performing predictions via movieId model
edx %>% count(movieId) %>%
  left_join(avg_movie_rating) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(b1) %>% 
  select(Title, b1, n) %>% 
  slice(1:10)

#should apply regularization

### Regularization using Lambda to penalize movies and users with minimal ratings
#specify lambda values
lambdas <- seq(0, 10, by = 0.1)
rmses <- sapply(lambdas, function(lambda) {
  # Calculate the average by movie
  
  b_m <- edx %>% 
    distinct(movieId, userId, .keep_all = TRUE) %>%
    group_by(movieId) %>%
    summarize(b_m = sum(rating - mu_hat) / (n() + lambda))
  
  # Calculate the average by user
  
  b_u <- edx %>% 
    distinct(movieId, userId, .keep_all = TRUE) %>%
    left_join(b_m, by='movieId') %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_m - mu_hat) / (n() + lambda))
  
  # Compute the predicted ratings on validation dataset
  
  predicted_ratings <- validation %>%
    left_join(b_m, by='movieId') %>%
    left_join(b_u, by='userId') %>%
    mutate(pred = mu_hat + b_m + b_u) %>%
    pull(pred)
  
  # Predict the RMSE on the validation set
  
  return(RMSE(validation$rating, predicted_ratings))
})

#view results 
qplot(lambdas, rmses)

#select lambda with lowest rmse

lambda <- lambdas[which.min(rmses)]
lambda

#Final Results
results_final$beta_hat <- as.character(results_final$beta_hat)
results_final <- results_final %>% add_row(model_name = "Regularized Mean, Movie, and UserId model", RMSE = min(rmses), beta_hat = "lambda")