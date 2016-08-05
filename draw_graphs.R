library(readr)
library(stringr)
library(dplyr)
library(tidyr)
years = 2000:2015
movies_imdb = data.frame()
for (year in years){
  df = read_csv(str_c("./moviesData/movies_imdb_", year, ".csv", sep = ""))
  movies_imdb = rbind(movies_imdb, df)
}

write_csv(movies_imdb, "movies_imdb.csv")
movies_rt = read_csv("movies_df.csv")
movies_imdb$contentRating[7600] = str_c("Rated", movies_imdb$contentRating[7600], sep=" ")
movies_imdb = movies_imdb %>% 
  mutate(general_rating_val = str_match(ratingValue, "[0-9]\\.[0-9]")) %>% 
  mutate(review_users = str_match(review_users, "[0-9]\\.[0-9]") %>% 
           str_replace(",", "")) %>% 
  mutate(review_critcs = str_match(review_users, "[0-9]\\.[0-9]") %>% 
           str_replace(",", "")) %>% 
  mutate(contentRatingLevel = str_replace(contentRating, "(rated )|(Rated )", "")) %>% 
  mutate(contentRatingLevel = str_replace(contentRatingLevel, " for.*", "")) %>% 
  filter(str_detect(country, "USA"))

View(movies_imdb)