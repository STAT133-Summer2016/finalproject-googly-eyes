library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Convert gross and budget currency to USD
cad_us_rate = read_csv("../Data/exchange rate/rates_data_cad.csv")
colnames(cad_us_rate) = c("date", "rate", as.character(1:9))
cad_us_rate = cad_us_rate %>% 
  mutate(date = ymd(date)) %>% 
  select(date, rate)

aud_us_rate = read_csv("../Data/exchange rate/rates_data_aud.csv")
colnames(aud_us_rate) = c("date", "rate", as.character(1:9))
aud_us_rate = aud_us_rate %>% 
  mutate(date = ymd(date)) %>% 
  select(date, rate)

CurrencyExchange = function(date, money, currency){
  if (currency == "CAD"){
    money = str_match(money, "[0-9]+") %>% as.numeric()
    for (i in 1:nrow(cad_us_rate)){
      if (cad_us_rate[i,1] < date){
        return(money / cad_us_rate[i,2])
      }
    }
    NA
  }
  if (currency == "AUD"){
    money = str_match(money, "[0-9]+") %>% as.numeric()
    for (i in 1:nrow(aud_us_rate)){
      if (aud_us_rate[i,1] < date){
        return(money / aud_us_rate[i,2])
      }
    }
    NA
  }
  if (currency == "DKK"){
    return(str_match(money, "[0-9]+") %>% as.numeric() / 6.99181)
  }
  NA
}

# Row bind data of each year to a large data set
years = 2000:2015
movies_imdb = data.frame()
for (year in years){
  df = read_csv(str_c("../Data/imdb/movies_imdb_", year, ".csv", sep = ""))
  movies_imdb = rbind(movies_imdb, df)
}

# Clean the movie data from imdb
movies_imdb$contentRating[7600] = str_c("Rated", movies_imdb$contentRating[7600], sep=" ")
movies_imdb = movies_imdb %>% 
  mutate(general_rating_val = str_match(ratingValue, "[0-9]\\.[0-9]")) %>% 
  mutate(general_rating_val = as.numeric(general_rating_val)) %>% 
  mutate(general_rating_user = str_match(ratingValue, "based on .* user ratings") %>% 
           str_replace("based on ", "") %>% 
           str_replace(" user ratings", "") %>% 
           str_replace(",", "") %>% 
         as.numeric()) %>% 
  mutate(review_users = str_replace(review_users, "user", "") %>% 
           str_replace(",", "") %>% str_replace(" ", "") %>% as.numeric()) %>% 
  mutate(review_critcs = str_replace(review_critcs, "critic", "") %>% 
           str_replace(",", "") %>% str_replace(" ", "") %>% as.numeric()) %>% 
  mutate(contentRatingLevel = str_replace(contentRating, "(rated )|(Rated )", "")) %>% 
  mutate(contentRatingLevel = str_replace(contentRatingLevel, "( for.*)|( on.*)|( For.*)|(thematic.*)|( material.*)", "")) %>%
  mutate(contentRatingLevel = str_replace_all(contentRatingLevel, " ", "")) %>% 
  filter(str_detect(country, "USA")) %>% 
  filter(date > mdy("01-01-2000")) %>% 
  filter(date < mdy("01-01-2016")) %>% 
  mutate(gross = as.numeric(gross)) %>% 
  mutate(name = str_replace_all(name, "\\([0-9]{4}\\)", "")) %>% 
  mutate(year = year(date))
colnames(movies_imdb)[which(names(movies_imdb) == "general_rating_val")] <- "rating"
movies_imdb = movies_imdb %>% 
  mutate(budget = ifelse(str_detect(budget, "CAD"), CurrencyExchange(date, budget, "CAD"), as.numeric(budget))) %>% 
  mutate(budget = ifelse(str_detect(budget, "AUD"), CurrencyExchange(date, budget, "AUD"), as.numeric(budget))) %>% 
  mutate(budget = ifelse(str_detect(budget, "DKK"), CurrencyExchange(date, budget, "DKK"), as.numeric(budget)))

# Read data from rotten tomatoes
movies_rt = read_csv("../movies/movies_rotten.csv")

# Write imdb movie data
write_csv(movies_imdb, "movies_imdb.csv")
write_csv(movies_imdb, "../movies/movies_imdb.csv")

# movies_by_director = movies_imdb %>%
#   select(director, budget, gross) %>% 
#   mutate(director = as.character(director)) %>% 
#   mutate(temp = 1) %>% 
#   group_by(director) %>% 
#   summarise(mean_gross = mean(gross, na.rm = T),
#             mean_budget = mean(budget, na.rm = T),
#             number_of_movies = sum(temp))
# write_csv(movies_by_director, "movies_by_director.csv")
# 
# # ggplot(movies_by_director) + geom_smooth(aes(x=mean_budget, y=mean_gross, size=number_of_movies))
# 
# d = movies_imdb$director %>% 
#   str_split(",") %>% 
#   unlist() %>% 
#   unique()
# d=d[!is.na(d)]
# 
# director_and_movie = data.frame(matrix(d, nrow = length(d), ncol = 1)) %>% 
#   mutate(mean_gross = 0) %>% 
#   mutate(mean_budget = 0) %>% 
#   mutate(num = 0)
# colnames(director_and_movie) = c("director", "mean_gross", "mean_budget", "num")
# 
# n = unique(movies_imdb$name)
# ndf = matrix(n, nrow = length(n), ncol = 1) %>% data.frame()
# colnames(ndf) = c("name")
a = movies_imdb$stars %>% 
  str_split(",") %>% 
  unlist() %>% 
  unique()
a=a[!is.na(a)]
a=a[str_detect(a, " ")]
actors_and_movies = data.frame(matrix(NA, nrow = 0, ncol = 3))
names(actors_and_movies) <- c("actor", "gross", "budget")

temp_actor <- movies_imdb %>%
  select(stars, gross, budget)

for(i in 1 : length(a)) {
  temp <- temp_actor %>%
    filter(str_detect(stars, a[i]))
  actors_and_movies <- rbind(actors_and_movies, 
                                data.frame(actor = a[i], 
                                           gross = sum(temp$gross, 
                                                       na.rm = T), 
                                           budget = sum(temp$budget,
                                                        na.rm = T)))
}
actors_and_movies %>% 
  arrange(desc(gross)) %>% 
  head(300) %>% 
  ggplot() + 
  geom_point(aes(x=budget, y=gross))
write_csv(actors_and_movies, "../movies/actors_and_movies.csv")

# Some test drawing
all_genres=c("Animation",
          "Comedy",
          "Documentary",
          "Family",
          "Horror",
          "Musical",
          "Romance",
          "Sport",
          "War",
          "Adventure",
          "Biography",
          "Crime",
          "Drama",
          "Fantasy",
          "History",
          "Music",
          "Mystery",
          "Sci-Fi",
          "Thriller",
          "Western")

# 
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[1])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+labs(title = genrres[1])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1,800000000))
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[2])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[2])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[3])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[3])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[4])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[4])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[5])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[5])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[6])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[6])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[7])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[7])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[8])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[8])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[9])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[9])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
# 
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[10])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[10])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[11])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[11])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
# 
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[12])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[12])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[13])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross)) +ggtitle(genrres[13])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[14])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[14])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[15])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[15])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[16])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross)) +ggtitle(genrres[16])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[17])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[17])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[18])) %>% 
#     ggplot() +
#       geom_smooth(aes(x=budget, y=gross), color = "red")+ggtitle(genrres[18]) +
#       scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000)) +
#       geom_point(aes(x=budget, y=gross), color = "blue", alpha = 0.5)
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[19])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[19])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   
#   
#   
#   movies_imdb %>% 
#     filter(str_detect(genres, genrres[20])) %>% 
#     ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[20])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
#   



#-------------------------------------------------------------------------------------------------------------
#Rotten Tomatoes cleaning
#-------------------------------------------------------------------------------------------------------------


movies_rt = read_csv("./movies_df.csv")
colnames(movies_rt)[which(names(movies_rt) == "Action & Adventure")] <- "Action"
colnames(movies_rt)[which(names(movies_rt) == "Kids & Family")] <- "Family"
colnames(movies_rt)[which(names(movies_rt) == "Musical & Performing Arts")] <- "Musical"
colnames(movies_rt)[which(names(movies_rt) == "Mystery & Suspense")] <- "Mystery"
colnames(movies_rt)[which(names(movies_rt) == "Science Fiction & Fantasy")] <- "Fantasy"
colnames(movies_rt)[which(names(movies_rt) == "Sports & Fitness")] <- "Sport"
colnames(movies_rt)[which(names(movies_rt) == "Cult Movies")] <- "Thriller"
colnames(movies_rt)[which(names(movies_rt) == "Classics")] <- "History"
colnames(movies_rt)[which(names(movies_rt) == "Classification")] <- "contentRatingLevel"
colnames(movies_rt)[which(names(movies_rt) == "Tomato_meter")] <- "rating"
colnames(movies_rt)[which(names(movies_rt) == "Directed_by")] <- "director"
colnames(movies_rt)[which(names(movies_rt) == "Cast")] <- "stars"
colnames(movies_rt)[which(names(movies_rt) == "Title")] <- "name"

movies_rt = movies_rt %>% 
  mutate(SciFi = Fantasy) %>% 
  mutate(Adventure = Action) %>% 
  mutate(year = str_match(name, "[0-9]{4}")) %>% 
  mutate(name = str_replace_all(name, "[0-9]{4}", "")) %>% 
  mutate(name = str_replace_all(name, "\\(\\)", "")) %>% 
  mutate(contentRatingLevel = str_replace_all(contentRatingLevel, "\\(.*\\)", "")) %>% 
  mutate(contentRatingLevel = str_replace_all(contentRatingLevel, " $", "")) %>% 
  mutate(genres = "default") %>% 
  mutate(name = str_replace_all(name, "�", "")) %>% 
  mutate(name = str_replace_all(name, ":", ""))

#collapse genre boolean column into single column containing a single vector
movies_rt = mutate(movies_rt, genres = ifelse(Action == TRUE, str_c(genres, ",Action"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Adeventure == TRUE, str_c(genres, ",Adventure"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Animation == TRUE, str_c(genres, ",Animation"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(History == TRUE, str_c(genres, ",History"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Comedy == TRUE, str_c(genres, ",Comedy"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Documentary == TRUE, str_c(genres, ",Documentary"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Family == TRUE, str_c(genres, ",Family"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Horror == TRUE, str_c(genres, ",Horror"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Musical == TRUE, str_c(genres, ",Musical"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Romance == TRUE, str_c(genres, ",Romance"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Sport == TRUE, str_c(genres, ",Sport"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Thriller == TRUE, str_c(genres, ",Thriller"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Drama == TRUE, str_c(genres, ",Drama"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Horror == TRUE, str_c(genres, ",Horror"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Mystery == TRUE, str_c(genres, ",Mystery"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Fantasy == TRUE, str_c(genres, ",Fantasy"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(Western == TRUE, str_c(genres, ",Western"), genres))
movies_rt = mutate(movies_rt, genres = ifelse(SciFi == TRUE, str_c(genres, ",Sci-Fi"), genres)) 
movies_rt = mutate(movies_rt, genres = str_replace_all(genres, "default,", ""))
# movies_rt = mutate(movies_rt, genres = str_split(genres, ","))
write_csv(movies_rt, "movies_rotten.csv")
View(movies_rt)


#-------------------------------------------------------------------------------------------------------------
#genre counting
#-------------------------------------------------------------------------------------------------------------
imdb = read_csv("movies_imdb.csv") 
  
genreCounting = function(genre, years){
result = c()
for( y in years){
  genre_imdb = imdb %>% 
    filter(y == year) %>%
    mutate(genres = str_split(genres, ",")) %>% 
    filter(str_detect(genres, genre))
    result = c(result, nrow(genre_imdb))
}  
return(result)
}

ActionL = genreCounting("Action", 2000:2015)
ComedyL = genreCounting("Comedy", 2000:2015)
DocumentaryL = genreCounting("Documentary", 2000:2015)
FamilyL = genreCounting("Family", 2000:2015)
HorrorL = genreCounting("Horror", 2000:2015)
MusicalL = genreCounting("Musical", 2000:2015)
RomanceL = genreCounting("Romance", 2000:2015)
SportL = genreCounting("Sport", 2000:2015)
WarL = genreCounting("War", 2000:2015)
AdventureL = genreCounting("Adventure", 2000:2015)
BiograhyL = genreCounting("Biography", 2000:2015)
CrimeL = genreCounting("Crime", 2000:2015)
DramaL = genreCounting("Drama", 2000:2015)
FantasyL = genreCounting("Fantasy", 2000:2015)
HistoryL = genreCounting("History", 2000:2015)
MusicL = genreCounting("Music", 2000:2015)
MysteryL = genreCounting("Mystery", 2000:2015)
SciFiL = genreCounting("Sci-Fi", 2000:2015)
WesternL = genreCounting("Western", 2000:2015)

genre_count_df = as.data.frame(matrix(ncol = 20, nrow = 16))
names(genre_count_df) = c("year", "Action", "Comedy", "Documentary",
                          "Family", "Horror", "Musical", "Romance", "Sport", "War",
                          "Adventure", "Biography", "Crime",
                          "Drama", "Fantasy", "History",
                          "Music", "Mystery", "Sci-Fi", "Western")

genre_count_df = genre_count_df %>% 
  mutate(Action = ActionL) %>% 
  mutate(Comedy = ComedyL) %>% 
  mutate(Documentary = DocumentaryL) %>% 
  mutate(Family = FamilyL) %>% 
  mutate(Horror = HorrorL) %>% 
  mutate(Musical = MusicalL) %>% 
  mutate(Romance = RomanceL) %>% 
  mutate(Sport = SportL) %>% 
  mutate(War = WarL) %>% 
  mutate(Adventure = AdventureL) %>% 
  mutate(Biography = BiograhyL) %>% 
  mutate(Crime = CrimeL) %>% 
  mutate(Drama = DramaL) %>% 
  mutate(Fantasy = FantasyL) %>% 
  mutate(History = HistoryL) %>% 
  mutate(Music = MusicL) %>% 
  mutate(Mystery = MysteryL) %>% 
  mutate(SciFi = SciFiL) %>% 
  mutate(Western = WesternL) %>% 
  mutate(year = 2000:2015) %>% 
  gather(genre, number, -year)

write_csv(genre_count_df, "genre_counting.csv")


#-------------------------------------------------------------------------------------------------------------
#box and month
#-------------------------------------------------------------------------------------------------------------
imdb_box_month = read_csv("movies_imdb.csv") %>% 
  mutate(month = str_match(date, "-[0-9]{2}-")) %>% 
  mutate(month = str_replace_all(month, "-", "")) %>%
  mutate(month = str_replace(month, "^[0]", "")) %>% 
  mutate(opening_week = str_match(opening_week, "[0-9]+")) %>% 
  mutate(opening_week = as.numeric(opening_week)) %>%  na.omit()

averageBoxMonthCalculate = function(mon){
  imdb_box_month = imdb_box_month %>% 
    filter(m == month) 
  total = sum(imdb_box_month$opening_week)
  average = as.numeric(total / nrow(imdb_box_month))
  average
}

average_box_per_month = c()
for (m in 1:12){
  average_box_per_month = c(average_box_per_month, averageBoxMonthCalculate(m))
}

average_box_per_month_df = as.data.frame(matrix(ncol = 2, nrow = 12))
names(average_box_per_month_df) = c("month", "average")
average_box_per_month_df = average_box_per_month_df %>% 
  mutate(month = 1:12) %>% 
  mutate(average = average_box_per_month)

write_csv(average_box_per_month_df, "average_box_per_month.csv")