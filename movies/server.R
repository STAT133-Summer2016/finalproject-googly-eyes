#http://shiny.rstudio.com/reference/shiny/latest/renderDataTable.html

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

imdb_dat = read.csv("movies_imdb.csv", stringsAsFactors = FALSE)
rt_dat = read.csv("movies_rotten.csv", stringsAsFactors = FALSE)
genre_dat = read.csv("genre_counting.csv")
average_box_per_month_dat = read.csv("average_box_per_month.csv")
movies_by_director = read.csv("movies_by_director.csv", stringsAsFactors = FALSE)
actors_and_movies = read_csv("actors_and_movies.csv")

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
      filter( year >= input$year_range[1] & 
                year <= input$year_range[2] & 
                str_detect(genres, input$genre) &
                str_detect(contentRatingLevel, str_c("^", input$contentRating, "$", sep=""))) %>% 
      select(name, rating, director, stars)
  })
  
movie_by_genre = reactive({
  imdb_dat %>% 
  filter( year >= input$year_range[1] & 
          year <= input$year_range[2] & 
          str_detect(genres, input$genre) &
          str_detect(contentRatingLevel, str_c("^", input$contentRating, "$", sep=""))) %>% 
  select(name, rating, director, stars, keywords, genres, year, budget, gross, general_rating_user) %>% 
  mutate(budget = budget %>% as.numeric(),
         gross = gross %>% as.numeric())
})

movie_by_genre_for_graphs = reactive({
  imdb_dat %>% 
    filter( year >= input$year_range[1] & 
              year <= input$year_range[2] & 
              str_detect(genres, input$genre) &
              input$contentRating == contentRatingLevel) %>% 
    select(budget, gross) %>% 
    mutate(budget = budget %>% as.numeric(),
           gross = gross %>% as.numeric())
})


 output$tbl <- renderDataTable({
   if(input$datasetSelection == "IMDB"){
     imdb_display() 
   }
   else if(input$datasetSelection == "RottenTomatoes"){
     rt_display()
   }
 })
 

  output$graph1 = renderPlot({
    movie_by_genre() %>% 
      ggplot() +
      geom_smooth(aes(x=budget, y=gross), color = "red")+ggtitle(input$genre) +
      geom_point(aes(x=budget, y=gross), color = "blue", alpha = 0.4) +
      scale_x_continuous(limits = c(0,300000000))+
      scale_y_continuous(limits = c(1, 500000000))
      
  },height = 1000, width = 600)
  
  output$graph2 = renderPlot({
    movie_by_genre() %>% 
      filter(year == input$year_for_graph2) %>% 
      ggplot()+
      geom_point(aes(x=general_rating_user, y = rating))
  }, height = 600, width = 600)
  
  output$graph3 = renderPlot({
    movie_by_genre() %>% 
      ggplot() +
      geom_point(aes(x = general_rating_user, y = gross))
  })
  
  output$graph_genre_trend = renderPlot({
    genre_dat %>% 
      filter(year == input$genre_year) %>% 
      mutate(alphabet = c("a", "b", "c", "d",
                          "e", "f", "g", "h",
                          "i", "j", "k", "l",
                          "m", "n", "o", "p",
                          "q", "r", "s", "t" )) %>% 
      ggplot() +
      geom_point(aes(x = genre, y = number, fill = alphabet), shape = 21, colour = "black", alpha = 0.8, size = 5) + 
      guides(fill=FALSE) +
      scale_y_continuous(name = "number of movies",
                         labels = c("25","50","150"),
                         breaks = c(25, 50, 150),
                         limits = c(0, 285))
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
    
    output$graph_box_month = renderPlot({
      average_box_per_month_dat %>% 
        mutate(alphabet = c("a", "b", "c", "d",
                            "e", "f", "g", "h",
                            "i", "j", "k", "l")) %>% 
        ggplot() +
        geom_point(aes(x = month, y = average, fill = alphabet), shape = 21, colour = "black", alpha = 0.8, size = 6) + 
        scale_fill_brewer(palette = "Spectral") +
        guides(fill=FALSE) +
        scale_y_continuous(name = "Average Opening Week Box",  labels = c("7000000","10000000","15000000", "20000000", "25000000"),
                           breaks = c(7000000, 10000000, 15000000, 20000000, 25000000),
                           limits = c(6000000, 28000000)) +
        scale_x_continuous(name = "Month",  labels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug",
                                                       "Sep", "Oct", "Nov", "Dec"),
                           breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
                           limits = c(1, 12))
    })
    

})

