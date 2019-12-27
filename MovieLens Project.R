####################################
# MovieLens Machine Learning Project
####################################

head(movielens,10)

# Install Necessary Packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(simEd)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(rstream)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(stats)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(lattice)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(rmarkdown)) install.packages("caTools", repos = "http://cran.us.r-project.org")

# Download MovieLens data file
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Convert the data file into dataframe
library(data.table)
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
library(stringr)
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
library(dplyr)
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")

# Create training set and test set, test set will be 10% of data

library(caret)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
train <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in test set are also in train set
test <- temp %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test)
train <- rbind(train, removed)

# Remove unwanted tables
rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Create RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# First Model - Predict the same rating for all movies regardless of user (Y = mu + random error)
mu <- mean(train$rating)
mu


# Predict all unknown ratings with mu and obtain the following RMSE
model_1_rmse <- RMSE(test$rating, mu)
model_1_rmse

# Create table recording RMSE of differnt models
rmse_results <- tibble(method = "Just the average", RMSE = model_1_rmse)

# Second Model - Modeling movie effect (Y = mu + b_i + random error)
movie_avgs <- train %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))

# Histogram charting for movie effect b_i
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

# Find predicted ratings in test set
predicted_ratings <- mu + test %>% left_join(movie_avgs, by='movieId') %>% pull(b_i)

# Obtain the RMSE of the Second Model
model_2_rmse <- RMSE(predicted_ratings, test$rating)
model_2_rmse

# Put the RMSE of Second Model to the table
rmse_results <- bind_rows(rmse_results, tibble(method="Movie Effect Model",RMSE=model_2_rmse))

# Third Model - Modeling User effects (Y = mu + b_i + b_u + random error)
user_avgs <- train %>% left_join(movie_avgs, by='movieId') %>% group_by(userId) %>% summarize(b_u = mean(rating - mu - b_i))

# Histogram charting for movie effect b_u
train %>% group_by(userId) %>% summarize(b_u = mean(rating)) %>% filter(n()>=100) %>% ggplot(aes(b_u)) + geom_histogram(bins = 30, color = "black")

# Find predicted ratings in test set for Third Model
predicted_ratings <- test %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% mutate(pred = mu + b_i + b_u) %>% pull(pred)

# Obtain the RMSE of the Third Model
model_3_rmse <- RMSE(predicted_ratings, test$rating)
model_3_rmse

# Put the RMSE of Third Model to the table
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Movie + User Effects Model",  
                                     RMSE = model_3_rmse))
