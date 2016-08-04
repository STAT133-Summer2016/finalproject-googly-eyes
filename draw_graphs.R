library(readr)
years = 2000:2015
movies_imdb = data.frame()
for (year in years){
  df = read_csv(str_c("./moviesData/movies_imdb_", year, ".csv", sep = ""))
  movies_imdb = rbind(movies_imdb, df)
}
View(movies_imdb)
write_csv(movies_imdb, "movies_imdb.csv")
movies_rt = read_csv("movies_df.csv")