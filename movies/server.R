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

imdb_dat = read.csv("movies_imdb.csv")

shinyServer(function(input, output) {

  
 imdb <- reactive({
          subset(imdb_dat, 
                   year >= input$year_range[1] & 
                   year <= input$year_range[2] &
                   all(input$genre %in% genres) &
                  input$contentRating == contentRatingLevel) %>% 
     select(name, rating, director, stars, keywords)
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
    imdb()
  })
  
  output$trailer <- renderText({
    trailer()
  })
  
  output$lucky <- renderDataTable({
    lucky()
  })
})

