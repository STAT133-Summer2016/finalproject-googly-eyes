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

imdb_dat = read.csv("movies_imdb.csv", stringsAsFactors = FALSE)
movies_by_director = read.csv("movies_by_director.csv", stringsAsFactors = FALSE)
actors_and_movies = read_csv("actors_and_movies.csv")

shinyServer(function(input, output) {

movie_by_genre = reactive({
  imdb_dat %>% 
  filter( year >= input$year_range[1] & 
          year <= input$year_range[2] & 
          str_detect(genres, input$genre) &
          str_detect(contentRatingLevel, str_c("^", input$contentRating, "$", sep=""))) %>% 
  select(name, rating, director, stars, keywords, genres, year, budget, gross) %>% 
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


 
 lucky <- reactive({
   subset(imdb_dat, 
          year >= input$year_range[1] & 
            year <= input$year_range[2] &
            all(input$genre %in% genres) &
            input$contentRating == contentRatingLevel) %>% 
     arrange(desc(rating)) %>% 
     select(trailer) %>% 
     head(1)
 })
   
 trailer <- reactive({
   subset(imdb_dat, 
          year >= input$year_range[1] & 
            year <= input$year_range[2] &
            all(input$genre %in% genres) &
            input$contentRating == contentRatingLevel) %>% 
     arrange(desc(rating)) %>% 
     select(trailer) %>% 
     head(1) %>% 
     .[[1]][[1]]
 })
 
  output$tbl <- renderDataTable({
    movie_by_genre()
  })
  
  output$trailer <- renderText({
    trailer()
  })
  
  output$lucky <- renderDataTable({
    lucky()
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
      ggplot() +
      geom_smooth(aes(x=gross, y=rating), color = "red")+ggtitle(input$genre) +
      geom_point(aes(x=gross, y=rating), color = "blue", alpha = 0.4) +
      scale_x_continuous(limits = c(0, 500000000))+
      scale_y_continuous(limits = c(0, 10))
  }, height = 600, width = 600)
    
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

})

