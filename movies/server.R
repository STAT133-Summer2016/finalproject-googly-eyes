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
movies_by_director = read.csv("movies_by_director.csv", stringsAsFactors = FALSE)

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
  
  output$graph1 = renderPlot({
    
    
#     movies_by_director = imdb_dat %>%
#       select(director, budget, gross) %>% 
#       mutate(director = as.character(director)) %>% 
#       mutate(temp = 1) %>% 
#       group_by(director) %>% 
#       summarise(mean_gross = mean(gross, na.rm = T),
#                 mean_budget = mean(budget, na.rm = T),
#                 number_of_movies = sum(temp))

    
    # movies_by_director %>% 
#       arrange(desc(mean_gross)) %>% 
#       head(200) %>% 
#       ggvis(~mean_budget, ~mean_gross, size = ~number_of_movies, stroke:= ~director) %>% 
#       layer_points() %>% 
#       add_tooltip(function(data) {str_c(data$director)}, "hover") %>% 
#       bind_shiny("ggvis")
    


#   df %>% 
#     ggvis(~log10(gdp), ~lifeexpectancy, 
#           fill = ~region, size = ~population, 
#           stroke:= ~country, fillOpacity := 0.6) %>% 
#       layer_points() %>% 
#       add_axis("x",
#                values = c(log10(500), log10(5000), log10(50000)),
#                title = "GDP Per Capita (Inflation-Adjusted)"
#       ) %>% 
#       add_axis("y", 
#                values = c(25, 50, 75),
#                title = "Life Expectancy at Birth") %>%
#       scale_numeric("x", domain = c(log10(500), log10(120000))) %>%
#       scale_numeric("y", domain = c(12, 80)) %>% 
#       scale_numeric("size", range = c(20,2000)) %>% 
#       hide_legend("size") %>% 
#       add_tooltip(function(data) {str_c(data$country)}, "hover") %>%
#       bind_shiny("ggvis", "ggvis_ui")
  })
})

