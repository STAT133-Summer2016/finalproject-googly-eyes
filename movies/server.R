library(dplyr)
library(stringr)
library(xml2)
library(rvest)
library(readr)
library(tidyr)
library(plyr)
library(scales)
library(ggplot2)
library(RColorBrewer)
library(lubridate)

imdb_dat = read.csv("./Data/movies_imdb.csv", stringsAsFactors = FALSE)
rt_dat = read.csv("./Data/movies_rt.csv", stringsAsFactors = FALSE)
genre_dat = read.csv("./Data/genre_counting.csv")
average_gross_per_month_dat = read.csv("./Data/average_gross_per_month.csv")
average_gross_per_weekday_dat = read.csv("./Data/average_gross_per_weekday.csv")
movies_by_director = read.csv("./Data/movies_by_director.csv", stringsAsFactors = FALSE)
actors_and_movies = read_csv("./Data/actors_and_movies.csv")

shinyServer(function(input, output) {
  
  imdb_display= reactive({
    imdb_dat %>% 
      filter( year >= input$year_range[1] & 
                year <= input$year_range[2] & 
                str_detect(genres, input$genre) &
                str_detect(contentRatingLevel, str_c("^", input$contentRating, "$", sep=""))) %>% 
      select(name, rating, director, stars, keywords)
  })
  
  rt_display= reactive({
    rt_dat %>% 
      mutate(year = Time %>% mdy() %>% year()) %>% 
      filter( year >= input$year_range[1] & 
                year <= input$year_range[2] & 
                str_detect(genres, input$genre) &
                str_detect(contentRatingLevel, str_c("^", input$contentRating, "$", sep=""))) %>% 
      select(name, rating, director, stars)
  })
  
  movie_by_genre_imdb = reactive({
    imdb_dat %>% 
      filter( year >= input$year_range[1] & 
                year <= input$year_range[2] & 
                str_detect(genres, input$genre) &
                str_detect(contentRatingLevel, str_c("^", input$contentRating, "$", sep=""))) %>% 
      # select(name, rating, director, stars, keywords, genres, year, budget, gross, general_rating_user) %>% 
      mutate(budget = budget %>% as.numeric(),
             gross = gross %>% as.numeric())
  })
  
  movie_by_genre_rt = reactive({
    rt_dat %>% 
      mutate(year = Time %>% mdy() %>% year()) %>% 
      filter( year >= input$year_range[1] & 
                year <= input$year_range[2] & 
                str_detect(genres, input$genre) &
                str_detect(Classification, str_c("^", input$contentRating, "$", sep="")))
      # select(name, rating, director, stars, keywords, genres, year, budget, gross, general_rating_user) %>% 
#       mutate(budget = budget %>% as.numeric(),
#              gross = gross %>% as.numeric())
  })
  
  movie_by_genre_any = reactive({
    if (input$datasetSelection == "IMDB"){
      imdb_dat %>% 
        filter( year >= input$year_range[1] & 
                  year <= input$year_range[2] & 
                  str_detect(genres, input$genre) &
                  str_detect(contentRatingLevel, str_c("^", input$contentRating, "$", sep=""))) %>% 
        select(name, rating, director, stars, keywords, genres, year, budget, gross, general_rating_user) %>% 
        mutate(budget = budget %>% as.numeric(),
               gross = gross %>% as.numeric())
    }else if (input$datasetSelection == "RottenTomatoes"){
      rt_dat %>% 
        mutate(year = Time %>% mdy() %>% year()) %>% 
        filter( year >= input$year_range[1] & 
                  year <= input$year_range[2] & 
                  str_detect(genres, input$genre) &
                  str_detect(Classification, str_c("^", input$contentRating, "$", sep=""))) %>% 
        mutate(name = Title, rating = Tomato_meter, general_rating_user = Reviews_count)
    }
  })

    
  average_profit_by_genre = reactive({
    imdb_dat %>% 
      filter( year >= input$year_range[1] & 
                year <= input$year_range[2] & 
                str_detect(contentRatingLevel, str_c("^", input$contentRating, "$", sep="")))
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
    order=c("jAnimation",
             "dComedy",
             "aDocumentary",
             "gFamily",
             "fHorror",
             "iMusical",
             "eRomance",
             "kSport",
             "sWar",
             "nAdventure",
             "cBiography",
             "pCrime",
             "hDrama",
             "mFantasy",
             "rHistory",
             "bMusic",
             "lMystery",
             "qSci-Fi",
             "oThriller",
             "tWestern")
    num = c()
    profit_rate = c()
    profit_rate_sd = c()
    for(genre in all_genres){
      dat = imdb_dat %>% filter(str_detect(genres, genre)) %>% filter(!is.na(budget)) %>% filter(!is.na(gross))
      profit_rates = dat$gross / dat$budget
      profit_rates = profit_rates[profit_rates<10 & profit_rates>0.1]
      profit_rate = c(profit_rate, sum(profit_rates) / length(profit_rates)) 
      profit_rate_sd = c(profit_rate_sd, sd(profit_rates))
      # profit = bind_rows(profit, data.frame(genre, profit_rates))
    }
    
    profit = data.frame(all_genres, profit_rate, profit_rate_sd, order)

  })
  
  average_box_per_month_dat_reactive = reactive({
    imdb_box_month = imdb_dat %>% 
      filter( year >= input$year_range[1] & 
                year <= input$year_range[2] & 
                str_detect(genres, input$genre) &
                str_detect(contentRatingLevel, str_c("^", input$contentRating, "$", sep=""))) %>% 
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
    
    average_box_per_month_df
  })
  
  output$tbl <- renderDataTable({
    # input$datasetSelection
    if(input$datasetSelection == "IMDB"){
      imdb_display() 
    }
    else if(input$datasetSelection == "RottenTomatoes"){
      rt_display()
    }
  })
  
  
  output$Box_vs_Budget_by_genre = renderPlot({
    movie_by_genre_imdb() %>% 
      ggplot() +
      geom_smooth(aes(x=budget, y=gross), color = "red")+ggtitle(input$genre) +
      geom_point(aes(x=budget, y=gross), color = "blue", alpha = 0.4) +
      scale_x_continuous(limits = c(0,300000000))+
      scale_y_continuous(limits = c(1, 500000000))
    
  },height = 1000, width = 600)
  
  selec = reactive({input$datasetSelection})
  
  output$Rating_vs_Users_by_Genre = renderPlot({
    # input$datasetSelection_rug
    if(input$datasetSelection_rug == "IMDB"){
      movie_by_genre_imdb() %>%
        mutate(reviews = general_rating_user %/% 150000 ) %>% 
        filter(reviews <= 3) %>% 
        # filter(year == input$year_for_graph2) %>% 
        ggplot()+
        geom_boxplot(aes(x=factor(reviews), y = rating))
    }
    else if(input$datasetSelection_rug == "RottenTomatoes"){
      movie_by_genre_rt() %>% 
        mutate(reviews = Reviews_count %/% 80 ) %>% 
        filter(reviews <= 3) %>% 
        # filter(year == input$year_for_graph2) %>% 
        ggplot()+
        geom_boxplot(aes(x=factor(reviews), y = Tomato_meter))
    }
    
  })
  
  output$Box_vs_Rating_Users = renderPlot({
    movie_by_genre_imdb() %>% 
      ggplot() +
      geom_point(aes(x = log10(general_rating_user), y = log10(gross)))
  })
  output$Box_vs_MetaScore = renderPlot({
    movie_by_genre_imdb() %>% 
      ggplot() +
      geom_point(aes(x = budget, y = gross, size = metaScore), color = "blue")
  })
  output$Box_vs_Review_Users = renderPlot({
    movie_by_genre_imdb() %>% 
      ggplot() +
      geom_point(aes(x = review_users, y = gross))
  })
  output$Box_vs_Review_Critics = renderPlot({
    movie_by_genre_imdb() %>% 
      ggplot() +
      geom_point(aes(x = review_critcs, y = gross))
  })
  
  output$genre_trend_number = renderPlot({
    genre_dat %>% 
      filter(year == input$genre_year) %>% 
      group_by(year) %>% 
      mutate(portion = number / sum(number, na.rm = T)) %>% 
      mutate(alphabet = c("a", "b", "c", "d",
                          "e", "f", "g", "h",
                          "i", "j", "k", "l",
                          "m", "n", "o", "p",
                          "q", "r", "s", "t" )) %>% 
      ggplot() +


      geom_point(aes(x = genre, y = portion, fill = alphabet), shape = 21, colour = "black", alpha = 0.8, size = 5) + 
      guides(fill=FALSE) +
      scale_y_continuous(name = "Number of movies",
                         labels = c("25", "50", "75", "100", "125", "150", "175", "200", "225", "250", "275", "300"),
                         breaks = c(25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300),
                         limits = c(0, 1/3))

  })
    
    movies_by_director %>% 
      arrange(desc(mean_gross)) %>% 
      head(200) %>% 
      ggvis(~mean_budget, ~mean_gross, size = ~number_of_movies, stroke:= ~director, fill := "blue", fillOpacity:=0.6) %>% 
      layer_points() %>% 
      add_tooltip(function(data) {str_c(data$director)}, "hover") %>% 
      bind_shiny("ggvis1")
    
    
    
    actors_and_movies %>% 
      arrange(desc(gross)) %>% 
      head(300) %>% 
      ggvis(~budget, ~gross, stroke:= ~actor, fill := "blue", fillOpacity:=0.6) %>% 
      layer_points() %>% 
      add_tooltip(function(data) {str_c(data$actor)}, "hover") %>% 
      bind_shiny("ggvis2")
    
    output$graph_gross_month = renderPlot({
      average_gross_per_month_dat %>% 
        mutate(alphabet = c("a", "b", "c", "d",
                            "e", "f", "g", "h",
                            "i", "j", "k", "l")) %>% 
        ggplot() +
        geom_point(aes(x = month, y = average, fill = alphabet), shape = 21, colour = "black", alpha = 0.8, size = 6) + 
        #scale_fill_brewer(palette = "Spectral") +
        guides(fill=FALSE) +
        scale_y_continuous(name = "Average Gross",  labels = c("40000000", "45000000", "50000000", "80000000", "90000000", "10000000", "135000000"),
                           breaks = c(40000000, 45000000, 50000000, 80000000, 90000000, 100000000, 135000000),
                           limits = c(40000000, 131622148)) +
        scale_x_continuous(name = "Month",  labels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                                       "Sep", "Oct", "Nov", "Dec"),
                           breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                           limits = c(1, 12))
    })
    
    output$graph_gross_weekday = renderPlot({
      average_gross_per_weekday_dat %>% 
        mutate(alphabet = c("a", "b", "c", "d",
                            "e", "f", "g")) %>% 
        ggplot() +
        geom_point(aes(x = weekday, y = average, fill = alphabet), shape = 21, colour = "black", alpha = 0.8, size = 6) + 
        #scale_fill_brewer(palette = "Spectral") +
        guides(fill=FALSE) +
        scale_y_continuous(name = "Average Gross",  labels = c("40000000", "50000000", "65000000", "80000000", "80000000", "100000000"),
                           breaks = c(40000000, 50000000, 65000000, 80000000, 80000000, 100000000),
                           limits = c(40000000, 100622148)) +
        scale_x_continuous(name = "Weekday",  labels = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
                           breaks = c(1, 2, 3, 4, 5, 6, 7),
                           limits = c(1, 7))
    })
    
    output$Profit_vs_genre = renderPlot({
      average_profit_by_genre() %>%
        # bind_cols(c())
        # filter(profit_rates<5 & profit_rates>0.2) %>% 
        ggplot() +
        geom_point(aes(x = order, y = profit_rate), size = profit_rate_sd) +
        scale_x_discrete(labels = c("Doc", "Music", "Bio", "Comedy", "Romance", "Horror",
                                    "Family", "Drama", "Musical", "Ani", "Sport", "Mys", "Fant", "Adv",
                                    "Thril", "Crime", "Sci-Fi", "His", "War", "Western")) +
        scale_size_continuous(guide=FALSE, range=c(200,10000))
    })
    
  
  movies_by_director %>% 
    arrange(desc(mean_gross)) %>% 
    head(200) %>% 
    ggvis(~mean_budget, ~mean_gross, size = ~number_of_movies, stroke:= ~director, fill := "blue", fillOpacity:=0.6) %>% 
    layer_points() %>% 
    add_tooltip(function(data) {str_c(data$director)}, "hover") %>% 
    
    bind_shiny("Box_vs_Budget_of_Directors")
  
  
  
  actors_and_movies %>% 
    arrange(desc(gross)) %>% 
    head(300) %>% 
    ggvis(~budget, ~gross, stroke:= ~actor, fill := "blue", fillOpacity:=0.6) %>% 
    layer_points() %>% 
    add_tooltip(function(data) {str_c(data$actor)}, "hover") %>% 
    bind_shiny("Box_vs_Budget_of_Actors")
  
  
})

