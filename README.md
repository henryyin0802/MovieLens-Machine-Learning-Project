# MovieLens-Machine-Learning-Project
Movie recommendation system using the MovieLens dataset
MovieLens Project is about creating a movie recommendation system using the well known MovieLens dataset which is included in the dslab package To make the computation easier, in this project the 10M version of the MovieLens dataset will be used which is just a small subset of a much larger latest entire MovieLens dataset with millions of ratings.
The dataset consists of 10 millions of rows and 6 columns. The columns include userID, movieID, rating, timestamp, title and genres.
The foundation of the recommendation system is built by developing an algorithm to predict the ratings of movies to that the users haven't given. Movies with high predicted ratings will be recommended to the users.
```{r Download MovieLens data file}
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
```
