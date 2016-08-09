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

imdb_dat = read.csv("./Data/movies_imdb.csv", stringsAsFactors = FALSE) %>% 
  mutate(profit_rate = gross / budget)
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
      select(name, rating, users = general_rating_user, metaScore, director, stars, genres, budget, gross,  keywords)
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
      filter(gross > 9.059220e+02 &
               gross < 4.061834e+08 &
               budget > 1000 &
               budget < 229115000 ) %>% 
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

    all_genres = c("Action",
                  "Anim", 
                  "Comedy",
                  "Doc",
                  "Family",
                  "Horror",
                  "Musical",
                  "Roman",
                  "Sport",
                  "War",
                  "Adv",
                  "Bio",
                  "Crime",
                  "Drama",
                  "Fantasy",
                  "History",
                  "Music",
                  "Mystery",
                  "Sci-Fi",
                  "Thriller",
                  "Western")
    num = c()
    profit_rate = c()
    profit_rate_sd = c()
    for(genre in all_genres){
      dat = imdb_dat %>% filter(str_detect(genres, genre)) %>% filter(!is.na(budget)) %>% filter(!is.na(gross))
      profit_rates = dat$gross / dat$budget
      profit_rates = profit_rates[profit_rates < 10 & profit_rates> 0.1]
      profit_rate = c(profit_rate, sum(profit_rates) / length(profit_rates)) 
      profit_rate_sd = c(profit_rate_sd, sd(profit_rates))
    }
    profit = data.frame(all_genres, profit_rate, profit_rate_sd)
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

  output$Most_Frequent_Key_Words = renderPlot({
    words = movie_by_genre_imdb() %>% .$keywords %>% str_split(",") %>% unlist() %>% str_replace_all("-", " ")
    top_words = sort(table(words),decreasing=TRUE)[1:10]   %>% data.frame() 
    top_words$words <- factor(top_words$words, levels = top_words$words[order(top_words$Freq)])
    ggplot(top_words, aes(x = words, y = Freq)) + theme_bw() + geom_bar(stat = "identity", fill = "blue", alpha = 0.6) +
      labs(title = "Most Frequnt Key Words")+
      theme(title = element_text(size = 16)) +
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12)) +
      theme(axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15)) 
      
  })
  

  output$Box_vs_Budget_by_genre = renderPlot({
    qtg = movie_by_genre_imdb() %>% .$gross %>% quantile(c(0.003, 0.997), na.rm = T)
    qtb = movie_by_genre_imdb() %>% .$budget %>% quantile(c(0.003, 0.997), na.rm = T)
    movie_by_genre_imdb() %>% 
      filter(gross > qtg[1] & gross < qtg[2]) %>% 
      filter(budget > qtb[1] & budget < qtb[2]) %>% 
      ggplot() +
      geom_smooth(aes(x=budget, y=gross), color = "red")+ggtitle(ifelse(input$genre == "[a-zA-Z]", "All Genres", input$genre)) +
      geom_point(aes(x=budget, y=gross), color = "blue", alpha = 0.4) +

      scale_x_continuous(limits = c(0, 250000000), 
                         labels = c("0", "50", "100", "150", "200", "250"),
                         name = "Budget (Millions)",
                         breaks = seq(0, 250000000, 50000000)) +
      scale_y_continuous(limits = c(0, 450000000),
                         labels = c("0", "50", "100", "150", "200", "250", "300", "350", "400", "450"),
                         breaks = seq(0, 450000000, 50000000),
                         name = "Gross (Millions)") 


    
  }, height = 800, width = 440)
  
  output$movie_by_genre_regression_line_helper = renderText({
    "Regression Line:\n"
  })
  output$movie_by_genre_regression_line = renderText({
    dat = movie_by_genre_imdb() 
    lr = lm(dat$gross~dat$budget)
    if (lr$coefficients[1] > 0){
      str_c("Gross = ", 
            lr$coefficients[2] %>% round(4) %>% as.character(), 
            " * Budget + ", 
            lr$coefficients[1] %>% round(0) %>% as.character(), 
            "\n")
    }else{
      str_c("Gross = ", 
            lr$coefficients[2] %>% round(4) %>% as.character(), 
            " * Budget - ", 
            lr$coefficients[1] %>% abs() %>% round(0) %>% as.character(), 
            "\n")
    }
    
  })
  output$movie_by_genre_regression_line_r = renderText({
    dat = movie_by_genre_imdb() 
    lr = lm(dat$gross~dat$budget)
    str_c( "R-squared: ", 
          lr %>% summary() %>% .[8] %>% as.numeric() %>% round(4))
  })
  
  selec = reactive({input$datasetSelection})
  

  output$box_vs_ratingNum_lb_regression_line = renderText({
    dat = movie_by_genre_imdb() %>% 
      filter(budget < 25000000) %>% 
      filter(gross > 5.347390e+02) %>% 
      filter(gross < 1.350719e+08) %>% 
      filter(general_rating_user > 11) %>% 
      filter(general_rating_user < 433936)
    lr = lm(dat$gross~dat$general_rating_user)
    if (lr$coefficients[1] > 0){
      str_c("Gross = ", 
            (lr$coefficients[2]  / 1000) %>% round(4) %>% as.character(), 
            " * NumberOfReviews + ", 
            lr$coefficients[1] %>% round(0) %>% as.character(), 
            ", R-squared = ",
            lr %>% summary() %>% .[8] %>% as.numeric() %>% round(4))
    }else{
      str_c("Gross = ", 
            (lr$coefficients[2]  / 1000) %>% round(4) %>% as.character(), 
            " * NumberOfReviews - ", 
            lr$coefficients[1] %>% abs() %>% round(0) %>% as.character(), 
            ", R-squared = ",
            lr %>% summary() %>% .[8] %>% as.numeric() %>% round(4))
    }
    
  })
  
  output$Box_vs_Rating_Users_1 = renderPlot({
    dat = movie_by_genre_imdb() %>% 
      mutate(general_rating_user_level = general_rating_user %/% 70000) %>% 
      filter(budget < 25000000) %>% 
      filter(gross > 5.347390e+02) %>% 
      filter(gross < 1.350719e+08) %>% 
      filter(general_rating_user > 11) %>% 
      filter(general_rating_user < 433936) %>% 
      filter(!is.na(general_rating_user_level)) 
    ggplot(dat) +
      # geom_point(aes(x = general_rating_user, y = gross))
      geom_boxplot(aes(x = factor(general_rating_user_level), y = gross), varwidth = T, color = "blue", alpha = 0.8) +
      scale_x_discrete(labels = c("0-70", "70-140", "140-210", "210-280", "280-350", "350-420", "420+"),
                       name = "Number of Ratings on IMDB (Thousands)") +
      scale_y_continuous(breaks = c(0, 50000000, 100000000),
                         labels = c("0", "50", "100"),
                         name = "Gross (Millions)")+
      labs(title = "Gross vs Number of Ratings")+
      theme(title = element_text(size = 16)) +
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12)) +
      theme(axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15)) 
  })
  
  output$box_vs_ratingNum_hb_regression_line = renderText({
    dat = movie_by_genre_imdb() %>% 
      filter(budget > 25000000) %>% 
      filter(gross > 35923) %>% 
      filter(gross < 517559275) %>%
      filter(general_rating_user > 3897) %>% 
      filter(general_rating_user < 914204)
    lr = lm(dat$gross~dat$general_rating_user)
    if (lr$coefficients[1] > 0){
      str_c("Gross = ", 
            (lr$coefficients[2] / 1000) %>% round(4) %>% as.character(), 
            " * NumberOfReviews + ", 
            lr$coefficients[1] %>% round(0) %>% as.character(), 
            ", R-squared = ",
            lr %>% summary() %>% .[8] %>% as.numeric() %>% round(4))
    }else{
      str_c("Gross = ", 
            (lr$coefficients[2] / 10000) %>% round(4) %>% as.character(), 
            " * NumberOfReviews - ", 
            lr$coefficients[1] %>% abs() %>% round(0) %>% as.character(), 
            ", R-squared = ",
            lr %>% summary() %>% .[8] %>% as.numeric() %>% round(4))
    }
    
  })
  
  output$Box_vs_Rating_Users_2 = renderPlot({
    dat = movie_by_genre_imdb() %>% 
      mutate(general_rating_user_level = general_rating_user %/% 131000) %>% 
      filter(gross > 35923) %>% 
      filter(gross < 517559275) %>%
      filter(general_rating_user > 3897) %>% 
      filter(general_rating_user < 914204) %>%
      filter(!is.na(general_rating_user_level)) %>%
      filter(budget > 25000000)
    ggplot(dat) +
      # geom_point(aes(x = general_rating_user, y = gross))
      geom_boxplot(aes(x = factor(general_rating_user_level), y = gross), varwidth = T, color = "blue", alpha = 0.8) +
      scale_x_discrete(labels = c("0-130", "130-260", "260-390", "390-520", "520-650", "650-780", "780+"),
                       name = "Number of Ratings on IMDB (Thousands)") +
      scale_y_continuous(name = "Gross (Millions)",
                         labels = c("0", "100", "200", "300", "400"))+
      labs(title = "Gross vs Number of Ratings")+
    theme(title = element_text(size = 16)) +
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12)) +
      theme(axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15)) 
  })
  
  
  output$box_vs_rating_lb_regression_line = renderText({
    dat = movie_by_genre_imdb() %>% 
      filter(budget < 25000000) %>% 
      filter(gross > 5.347390e+02) %>% 
      filter(gross < 1.350719e+08)
    lr = lm(dat$gross~dat$rating)
    if (lr$coefficients[1] > 0){
      str_c("Gross = ", 
            (lr$coefficients[2]  / 1000000) %>% round(4) %>% as.character(), 
            " * Rating + ", 
            lr$coefficients[1] %>% round(0) %>% as.character(), 
            ", R-squared = ",
            lr %>% summary() %>% .[8] %>% as.numeric() %>% round(4))
    }else{
      str_c("Gross = ", 
            (lr$coefficients[2]  / 1000000) %>% round(4) %>% as.character(), 
            " * Rating - ", 
            lr$coefficients[1] %>% abs() %>% round(0) %>% as.character(), 
            ", R-squared = ",
            lr %>% summary() %>% .[8] %>% as.numeric() %>% round(4))
    }
    
  })
  
  output$Box_vs_Rating_1 = renderPlot({
    dat = movie_by_genre_imdb() %>% 
      mutate(rating_level = rating %/% 1) %>% 
      filter(budget < 25000000) %>% 
      filter(gross > 5.347390e+02) %>% 
      filter(gross < 1.350719e+08) %>% 
      filter(!is.na(rating_level)) 
    ggplot(dat) +
      # geom_point(aes(x = general_rating_user, y = gross))
      geom_boxplot(aes(x = factor(rating_level), y = gross), varwidth = T, color = "blue", alpha = 0.8) +
      scale_x_discrete(
                       name = "Ratings on IMDB (Out of Ten)") +
      scale_y_continuous(breaks = c(0, 50000000, 100000000),
                         labels = c("0", "50", "100"),
                         name = "Gross (Millions)")+
      labs(title = "Gross vs Rating")+
    theme(title = element_text(size = 16)) +
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12)) +
      theme(axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15)) 
  })
  
  output$box_vs_rating_hb_regression_line = renderText({
    dat = movie_by_genre_imdb() %>% 
      filter(budget > 25000000) %>% 
      filter(gross > 35923) %>% 
      filter(gross < 517559275)
      lr = lm(dat$gross~dat$rating)
    if (lr$coefficients[1] > 0){
      str_c("Gross = ", 
            (lr$coefficients[2]  / 1000000) %>% round(4) %>% as.character(), 
            " * Rating + ", 
            lr$coefficients[1] %>% round(0) %>% as.character(), 
            ", R-squared = ",
            lr %>% summary() %>% .[8] %>% as.numeric() %>% round(4))
    }else{
      str_c("Gross = ", 
            (lr$coefficients[2] / 1000000) %>% round(4) %>% as.character(), 
            " * Rating - ", 
            lr$coefficients[1] %>% abs() %>% round(0) %>% as.character(), 
            ", R-squared = ",
            lr %>% summary() %>% .[8] %>% as.numeric() %>% round(4))
    }
    
  })
  
  output$Box_vs_Rating_2 = renderPlot({
    dat = movie_by_genre_imdb() %>% 
      mutate(rating_level = rating %/% 1) %>% 
      filter(budget > 25000000) %>% 
      filter(gross > 35923) %>% 
      filter(gross < 517559275) %>%
      filter(!is.na(rating_level)) 
    ggplot(dat) +
      # geom_point(aes(x = general_rating_user, y = gross))
      geom_boxplot(aes(x = factor(rating_level), y = gross), varwidth = T, color = "blue", alpha = 0.8) +
      scale_x_discrete(
        name = "Ratings on IMDB (Out of Ten)") +
      scale_y_continuous(
        # breaks = c(0, 50000000, 100000000),
                         labels = c("0", "100", "200", "300", "400"),
                         name = "Gross (Millions)")+
      labs(title = "Gross vs Rating")+
      theme(title = element_text(size = 16)) +
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12)) +
      theme(axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15)) 
  })
  
  
  output$box_vs_metaScore_lb_regression_line = renderText({
    dat = movie_by_genre_imdb() %>% 
      filter(budget < 25000000) %>% 
      filter(gross > 5.347390e+02) %>% 
      filter(gross < 1.350719e+08)
    lr = lm(dat$gross~dat$metaScore)
    if (lr$coefficients[1] > 0){
      str_c("Gross = ", 
            (lr$coefficients[2]  / 1000000) %>% round(4) %>% as.character(), 
            " * Rating + ", 
            lr$coefficients[1] %>% round(0) %>% as.character(), 
            ", R-squared = ",
            lr %>% summary() %>% .[8] %>% as.numeric() %>% round(4))
    }else{
      str_c("Gross = ", 
            (lr$coefficients[2]  / 1000000) %>% round(4) %>% as.character(), 
            " * Rating - ", 
            lr$coefficients[1] %>% abs() %>% round(0) %>% as.character(), 
            ", R-squared = ",
            lr %>% summary() %>% .[8] %>% as.numeric() %>% round(4))
    }
    
  })
  
  
  output$Box_vs_MetaScore_1 = renderPlot({
    dat = movie_by_genre_imdb() %>% 
      mutate(metaScoreLevel = metaScore %/% 15) %>% 
      filter(budget < 25000000) %>% 
      filter(gross > 5.347390e+02) %>% 
      filter(gross < 1.350719e+08) %>% 
      filter(!is.na(metaScoreLevel))
    ggplot(dat) +
      geom_boxplot(aes(x = factor(metaScoreLevel), y = gross), varwidth=TRUE, color = "blue", alpha = 0.8)+
      scale_x_discrete(name = "Meta Score", 
                         labels = c("0-15", "15-30", "30-45", "45-60", "60-75", "75-90", "90+"))+
      scale_y_continuous(name = "Gross (Million)",
                         breaks = c(0, 50000000, 100000000),
                         labels = c("0", "50", "100")
                         )+
      labs(title = "Gross vs Meta Score")+
      theme(title = element_text(size = 16)) +
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12)) +
      theme(axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15)) 
  })
  output$box_vs_metaScore_hb_regression_line = renderText({
    dat = movie_by_genre_imdb() %>% 
      filter(budget > 25000000) %>% 
      filter(gross > 35923) %>% 
      filter(gross < 517559275)
    lr = lm(dat$gross~dat$metaScore)
    if (lr$coefficients[1] > 0){
      str_c("Gross = ", 
            (lr$coefficients[2]  / 1000000) %>% round(4) %>% as.character(), 
            " * Rating + ", 
            lr$coefficients[1] %>% round(0) %>% as.character(), 
            ", R-squared = ",
            lr %>% summary() %>% .[8] %>% as.numeric() %>% round(4))
    }else{
      str_c("Gross = ", 
            (lr$coefficients[2]  / 1000000) %>% round(4) %>% as.character(), 
            " * Rating - ", 
            lr$coefficients[1] %>% abs() %>% round(0) %>% as.character(), 
            ", R-squared = ",
            lr %>% summary() %>% .[8] %>% as.numeric() %>% round(4))
    }
    
  })
  output$Box_vs_MetaScore_2 = renderPlot({
    dat = movie_by_genre_imdb() %>% 
      mutate(metaScoreLevel = metaScore %/% 15) %>%
      filter(gross > 35923) %>% 
      filter(gross < 517559275) %>%
      filter(!is.na(metaScoreLevel))
    ggplot(dat) +
      geom_boxplot(aes(x = factor(metaScoreLevel), y = gross), varwidth=TRUE, color = "blue", alpha =0.8)+
      scale_x_discrete(name = "Meta Score", 
                       labels = c("0-15", "15-30", "30-45", "45-60", "60-75", "75-90", "90+"))+
      scale_y_continuous(name = "Gross (Million)",
                         labels = c("0", "100", "200", "300", "400")
                         )+
    labs(title = "Gross vs Meta Score")+
      theme(title = element_text(size = 16)) +
      theme(axis.text.x = element_text(size=12),
            axis.text.y = element_text(size=12)) +
      theme(axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15)) 
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
      # mutate(portion = number / sum(number, na.rm = T)) %>% 
      mutate(alphabet = c("a", "b", "c", "d",
                          "e", "f", "g", "h",
                          "i", "j", "k", "l",
                          "m", "n", "o", "p",
                          "q", "r", "s", "t" )) %>% 
      ggplot() +


      geom_point(aes(x = genre, y = number , fill = alphabet), shape = 21, colour = "black", alpha = 0.8, size = 5) + 
      guides(fill=FALSE) +
      scale_y_continuous(name = "Number of movies",
                         labels = c("25", "50", "75", "100", "125", "150", "175", "200", "225", "250", "275", "300"),
                         breaks = c(25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300),
                         limits = c(0, 310))

  })
    
    # movies_by_director %>% 
    #   arrange(desc(mean_gross)) %>% 
    #   head(200) %>% 
    #   ggvis(~mean_budget, ~mean_gross, size = ~number_of_movies, stroke:= ~director, fill := "blue", fillOpacity:= 0.6) %>% 
    #   layer_points() %>% 
    #   add_tooltip(function(data) {paste0("Director: ", data$director, " Averge buget: ", 
    #                                      as.character(data$mean_budget), " Average gross: ", 
    #                                      as.character(data$mean_gross))}, "hover") %>% 
    #   bind_shiny("ggvis1")
    
    
    
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
                           breaks = c(30000000, 45000000, 50000000, 80000000, 90000000, 100000000, 135000000),
                           limits = c(0000000, 131622148)) +
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
      dat = average_profit_by_genre() 
        dat$all_genres <- factor(dat$all_genres, levels = dat$all_genres[order(dat$profit_rate)])
        ggplot(dat) +
        geom_point(aes(x = all_genres, y = profit_rate, size = profit_rate_sd)) +
        scale_x_discrete(
#           labels = c("Doc", "Music", "Bio", "Comedy", "Romance", "Horror",
#                                     "Family", "Drama", "Musical", "Ani", "Sport", "Mys", "Fant", "Adv",
#                                     "Thril", "Crime", "Sci-Fi", "His", "War", "Western")
          ) +
        scale_size_continuous(guide=FALSE, range=c(1,10)) +
        scale_x_discrete(name = "Genres")+
        scale_y_continuous(name = "Average Profit Rate")+
        labs(title = "Average Profit Rate vs Genres")+
        theme(title = element_text(size = 16)) +
        theme(axis.text.x = element_text(size=12),
              axis.text.y = element_text(size=12)) +
        theme(axis.title.x = element_text(size=15),
              axis.title.y = element_text(size=15)) 
      
    })
    
  
  movies_by_director %>% 
    arrange(desc(mean_gross)) %>% 
    head(200) %>% 
    ggvis(~mean_budget/1000000, ~mean_gross/1000000, size = ~number_of_movies, stroke:= ~director, key := ~profit_ratio, fill := "blue", fillOpacity:=0.6) %>% 
    layer_points() %>% 
    add_tooltip(function(data) {str_c("Director: ", data$director, "<br>", "Profit Ratio: ", 
                                        as.character(data$profit_ratio %>% round(2)), "<br>", "Averge budget(Millions): ", 
                                       as.character(data$mean_budget %>% round(2)),"<br>", "Average gross(Millions): ", 
                                       as.character(data$mean_gross %>% round(2)), "<br>", "Number of movies: ",
                                      as.character(data$number_of_movies))}, "hover") %>% 
    scale_numeric("size", range = c(30, 300)) %>%
    add_axis("x", title = "Average Budget(Millions of Dollars)", title_offset = 50) %>% 
    add_axis("y", title = "Average Gross(Millions of Dollars)", title_offset = 50) %>% 
    hide_legend("size") %>% 
    bind_shiny("Box_vs_Budget_of_Directors")
  
  
  
  actors_and_movies %>% 
    arrange(desc(gross)) %>% 
    head(300) %>% 
    ggvis(~budget/1000000, ~gross/1000000, size = ~num, stroke:= ~actor, fill := "blue", fillOpacity:=0.6) %>% 
    layer_points() %>% 
    add_tooltip(function(data) {str_c("Actor/Actress: ", data$actor, "<br>", "Profit Ratio: ", 
                                      as.character((data$budget / data$gross) %>% round(2)), "<br>", "Averge budget(Millions): ", 
                                      as.character(data$budget %>% round(2)),"<br>", "Average gross(Millions): ", 
                                      as.character(data$gross %>% round(2)) )}, "hover") %>% 
    scale_numeric("size", range = c(30, 300)) %>% 
    add_axis("x", title = "Average Budget(Millions of Dollars)", title_offset = 50) %>% 
    add_axis("y", title = "Average Gross(Millions of Dollars)", title_offset = 50) %>% 
    hide_legend("size") %>% 
    bind_shiny("Box_vs_Budget_of_Actors")
  
  
})

