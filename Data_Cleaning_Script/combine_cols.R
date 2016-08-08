library(dplyr)
library(readr)
library(tidyr)
imdb_dat = read_csv("../movies/movies_imdb.csv")
rt_dat = read_csv("../movies/movies_rotten.csv")
View(imdb_dat)
View(rt_dat)
common_data_rt = rt_dat %>% 
  filter(year >= 2000) %>% 
  select(name, rating, Audience_Reviews, Critics_rating, Audience_rating, Reviews_count, Fresh, Rotten, User_rating)
View(common_data_rt)
combined = left_join(common_data_rt, imdb_dat)
View(combined)