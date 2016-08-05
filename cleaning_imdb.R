library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
years = 2000:2015
movies_imdb = data.frame()
for (year in years){
  df = read_csv(str_c("./moviesData/movies_imdb_", year, ".csv", sep = ""))
  movies_imdb = rbind(movies_imdb, df)
}

movies_rt = read_csv("movies_df.csv")
movies_imdb$contentRating[7600] = str_c("Rated", movies_imdb$contentRating[7600], sep=" ")
movies_imdb = movies_imdb %>% 
  mutate(general_rating_val = str_match(ratingValue, "[0-9]\\.[0-9]")) %>% 
  mutate(review_users = str_match(review_users, "[0-9]\\.[0-9]") %>% 
           str_replace(",", "")) %>% 
  mutate(review_critcs = str_match(review_users, "[0-9]\\.[0-9]") %>% 
           str_replace(",", "")) %>% 
  mutate(contentRatingLevel = str_replace(contentRating, "(rated )|(Rated )", "")) %>% 
  mutate(contentRatingLevel = str_replace(contentRatingLevel, "( for.*)|( on.*)|( For.*)|(thematic.*)|( material.*)", "")) %>%
  mutate(contentRatingLevel = str_replace_all(contentRatingLevel, " ", "")) %>% 
  filter(str_detect(country, "USA")) %>% 
  filter(date > mdy("01-01-2000")) %>% 
  filter(date < mdy("01-01-2016")) %>% 
  mutate(name = str_replace_all(name, "\\([0-9]{4}\\)", "")) %>% 
  mutate(year = year(date))

colnames(movies_imdb)[which(names(movies_imdb) == "general_rating_val")] <- "rating"

min(movies_imdb$date, na.rm = TRUE)
max(movies_imdb$date, na.rm = T)

cad_us_rate = read_csv("rates_data.csv")
colnames(cad_us_rate) = c("date", "rate", as.character(1:9))
View(cad_us_rate)
cad_us_rate = cad_us_rate %>% 
  mutate(date = ymd(date)) %>% 
  select(date, rate)

write_csv(movies_imdb, "movies_imdb.csv")
View(movies_imdb)