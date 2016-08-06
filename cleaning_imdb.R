library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
cad_us_rate = read_csv("rates_data_cad.csv")
colnames(cad_us_rate) = c("date", "rate", as.character(1:9))
cad_us_rate = cad_us_rate %>% 
  mutate(date = ymd(date)) %>% 
  select(date, rate)

aud_us_rate = read_csv("rates_data_cad.csv")
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
  mutate(general_rating_val = as.numeric(general_rating_val)) %>% 
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


cad_us_rate = read_csv("rates_data_cad.csv")
colnames(cad_us_rate) = c("date", "rate", as.character(1:9))

cad_us_rate = cad_us_rate %>% 
  mutate(date = ymd(date)) %>% 
  select(date, rate)

movies_imdb = movies_imdb %>% 
  mutate(budget = ifelse(str_detect(budget, "CAD"), CurrencyExchange(date, budget, "CAD"), as.numeric(budget))) %>% 
  mutate(budget = ifelse(str_detect(budget, "AUD"), CurrencyExchange(date, budget, "AUD"), as.numeric(budget))) %>% 
  mutate(budget = ifelse(str_detect(budget, "DKK"), CurrencyExchange(date, budget, "DKK"), as.numeric(budget)))


genrres=c("Animation",
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


  movies_imdb %>% 
    filter(str_detect(genres, genrres[1])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+labs(title = genrres[1])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1,800000000))
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[2])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[2])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[3])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[3])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[4])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[4])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[5])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[5])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[6])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[6])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[7])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[7])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[8])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[8])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[9])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[9])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  

  movies_imdb %>% 
    filter(str_detect(genres, genrres[10])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[10])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[11])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[11])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  

  movies_imdb %>% 
    filter(str_detect(genres, genrres[12])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[12])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[13])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross)) +ggtitle(genrres[13])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[14])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[14])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[15])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[15])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[16])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross)) +ggtitle(genrres[16])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[17])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[17])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[18])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[18])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[19])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[19])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
  
  
  movies_imdb %>% 
    filter(str_detect(genres, genrres[20])) %>% 
    ggplot() + geom_smooth(aes(x=budget, y=gross))+ggtitle(genrres[20])+scale_x_continuous(limits = c(0,300000000))+scale_y_continuous(limits = c(1, 800000000))
  
library(dplyr)
movies_by_director = movies_imdb %>%
  select(director, budget, gross) %>% 
  mutate(director = as.character(director)) %>% 
  mutate(temp = 1) %>% 
  group_by(director) %>% 
  summarise(mean_gross = mean(gross, na.rm = T),
            mean_budget = mean(budget, na.rm = T),
            number_of_movies = sum(temp))
write_csv(movies_by_director, "movies_by_director.csv")
            
ggplot(movies_by_director) + geom_smooth(aes(x=mean_budget, y=mean_gross, size=number_of_movies))

d = movies_imdb$director %>% 
  str_split(",") %>% 
  unlist() %>% 
  unique()
d=d[!is.na(d)]

director_and_movie = data.frame(matrix(d, nrow = length(d), ncol = 1)) %>% 
  mutate(mean_gross = 0) %>% 
  mutate(mean_budget = 0) %>% 
  mutate(num = 0)
colnames(director_and_movie) = c("director", "mean_gross", "mean_budget", "num")

n = unique(movies_imdb$name)
ndf = matrix(n, nrow = length(n), ncol = 1) %>% data.frame()
colnames(ndf) = c("name")



