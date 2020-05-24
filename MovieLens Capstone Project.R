################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
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

#checking dimensions of edx 
dim(edx)

#checking the 5 rows of edx
head(edx,5)

#number of distinct movies and users in edx 
edx %>% summarize(n_movieId=n_distinct(movieId),
                  n_userId=n_distinct(userId))

#movie titles with total number of ratings
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#modifying data sets to add a year column and date column 
#year column is taken from title and date is modified from timestamp column 
library(lubridate)
edx<- edx %>% mutate(year = as.numeric(str_sub(title,-5,-2)),date= as_datetime(timestamp))
validation<- validation %>% mutate(year = as.numeric(str_sub(title,-5,-2)),date= as_datetime(timestamp))

# Viewing the first five columns of data sets
head(edx,5)
head(validation,5)

edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) + geom_line()+ ggtitle("Ratings Frequency")

edx %>% 
  count(movieId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 40, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies Distribution")

edx %>% 
  count(userId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 40, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users Distribution")

#plot to show total ratings given by year 
edx %>% group_by(year) %>% summarize(n=n()) %>% ggplot(aes(year,n))+ geom_point()+geom_line() +ggtitle("Number of ratings given every year")


edx %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#2018 is used as the end year to calculate the number of ratings per year
edx %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

#analysis to check if there is any effect of date on the rating patterns
#for easier analysis date columns in the datasets is rounded to the nearest week 
edx<- edx %>% mutate(date = round_date(date, unit = "week"))
validation<- validation %>% mutate(date = round_date(date, unit = "week"))

edx %>% 
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()

#Creating training set to train the algorithm and test set for assessment from edx data
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#Defining RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu<- mean(train_set$rating)
mu

#RMSE with just the average
naive_rmse<- RMSE(test_set$rating,mu)
naive_rmse

#Tibble to compare RMSE
rmse_results <- tibble(model = "Average", RMSE = naive_rmse)
rmse_results

#Movie effect bias (b_i) estimating b_i
movie_avg<- train_set %>% group_by(movieId) %>% summarize(b_i=mean(rating-mu))

qplot(b_i, data = movie_avg, bins = 30, color = I("black"))

#RMSE for Movie effect
predicted_ratings <- mu + test_set %>% 
  left_join(movie_avg, by='movieId') %>%
  pull(b_i)
movie_rmse<- RMSE(test_set$rating,predicted_ratings)

rmse_results<- bind_rows(rmse_results,
                         tibble(model="Movie Effect",RMSE=movie_rmse))
rmse_results

#Analysing user effect
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")

# User effect bias, estimating b_u
user_avg <- train_set %>% 
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

qplot(b_u, data = user_avg, bins = 30, color = I("black"))


predicted_ratings <- test_set %>% 
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avg, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

movieuser_rmse<- RMSE(test_set$rating,predicted_ratings)

rmse_results<- bind_rows(rmse_results,
                         tibble(model="Movie and User Effect",RMSE=movieuser_rmse))
rmse_results

# Perform cross-validation to choose optimal tuning parameter, lambda
lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

# RMSE versus lambda plot to select lambda at which RMSE is smallest 
qplot(lambdas, rmses)  
lambda<- lambdas[which.min(rmses)]
lambda

# Use lambda to evaluate regularized b_i and b_u 
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

#apply regularized model to predict ratings on test set
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

#Compute RMSE and add it to results tibble 
reg_movieuser_rmse<- RMSE(test_set$rating,predicted_ratings)
rmse_results<- bind_rows(rmse_results,
                         tibble(model="Regularized Movie and User Effect",RMSE=reg_movieuser_rmse))

print.data.frame(rmse_results)

lambdas <- seq(0, 10, 0.25)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_r<- train_set %>% 
    left_join(b_i,by="movieId") %>%
    left_join(b_u,by="userId") %>% 
    group_by(year) %>%
    summarize(b_r=sum(rating - b_i - b_u - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_r,by="year") %>% 
    mutate(pred = mu + b_i + b_u + b_r) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

# RMSE versus lambda plot to select lambda at which RMSE is smallest 
qplot(lambdas, rmses)  
lambda<- lambdas[which.min(rmses)]
lambda

# Use lambda to evaluate regularized b_i, b_u and b_r
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_r<- train_set %>% 
  left_join(b_i,by="movieId") %>%
  left_join(b_u,by="userId") %>% 
  group_by(year) %>%
  summarize(b_r=sum(rating - b_i - b_u - mu)/(n()+lambda))


#apply regularized model to predict ratings on test set
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_r,by="year") %>% 
  mutate(pred = mu + b_i + b_u + b_r) %>%
  pull(pred)

#Compute RMSE and add it to results tibble 
reg_movieuseryear_rmse<- RMSE(test_set$rating,predicted_ratings)
rmse_results<- bind_rows(rmse_results,
                         tibble(model="Regularized Movie, User and Year Effect",RMSE=reg_movieuseryear_rmse))
print.data.frame(rmse_results)

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_r<- train_set %>% 
    left_join(b_i,by="movieId") %>%
    left_join(b_u,by="userId") %>% 
    group_by(year) %>%
    summarize(b_r=sum(rating - b_i - b_u - mu)/(n()+l))
  
  b_t<- train_set %>% 
    left_join(b_i,by="movieId") %>%
    left_join(b_u,by="userId") %>% 
    left_join(b_r,by="year") %>%
    group_by(date) %>%
    summarize(b_t=sum(rating - b_i - b_u - b_r - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_r,by="year") %>% 
    left_join(b_t,by="date") %>% 
    mutate(pred = mu + b_i + b_u + b_r + b_t) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

# RMSE versus lambda plot to select lambda at which RMSE is smallest 
qplot(lambdas, rmses)  
lambda<- lambdas[which.min(rmses)]
lambda


# Use lambda to evaluate regularized b_i, b_u, b_r and b_t
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_r<- train_set %>% 
  left_join(b_i,by="movieId") %>%
  left_join(b_u,by="userId") %>% 
  group_by(year) %>%
  summarize(b_r=sum(rating - b_i - b_u - mu)/(n()+lambda))

b_t<- train_set %>% 
  left_join(b_i,by="movieId") %>%
  left_join(b_u,by="userId") %>% 
  left_join(b_r,by="year") %>%
  group_by(date) %>%
  summarize(b_t=sum(rating - b_i - b_u - b_r - mu)/(n()+lambda))


#apply regularized model to predict ratings on test set
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_r,by="year") %>% 
  left_join(b_t,by="date") %>% 
  mutate(pred = mu + b_i + b_u + b_r + b_t) %>%
  pull(pred)


#Compute RMSE and add it to results tibble 
reg_movieuseryeardate_rmse<- RMSE(test_set$rating,predicted_ratings)
rmse_results<- bind_rows(rmse_results,
                         tibble(model="Regularized Movie, User, Year and Date Effect",RMSE=reg_movieuseryeardate_rmse))
print.data.frame(rmse_results) #Choose the model with the lowest RMSE

print.data.frame(rmse_results)

#Predict ratings of Validation
lambda

mu <- mean(edx$rating)

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_r<- edx %>% 
  left_join(b_i,by="movieId") %>%
  left_join(b_u,by="userId") %>% 
  group_by(year) %>%
  summarize(b_r=sum(rating - b_i - b_u - mu)/(n()+lambda))

b_t<- edx %>% 
  left_join(b_i,by="movieId") %>%
  left_join(b_u,by="userId") %>% 
  left_join(b_r,by="year") %>%
  group_by(date) %>%
  summarize(b_t=sum(rating - b_i - b_u - b_r - mu)/(n()+lambda))


#apply regularized model to predict ratings on test set
predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_r,by="year") %>% 
  left_join(b_t,by="date") %>% 
  mutate(pred = mu + b_i + b_u + b_r + b_t) %>%
  pull(pred)

#Final RMSE
RMSE(validation$rating,predicted_ratings)
















