library(dplyr)
library(stringr)
library(xml2)
library(rvest)
library(readr)
library(tidyr)
library(plyr)
library(scales)
library(ggplot2)

imdb = read.csv("../movies_imdb.csv")

shinyServer(function(input, output) {
  output$demo_plot <- renderPlot({
    
    # imdb = imdb %>% 
    #   filter(year == input$year)
    
    # ggplot(cleaned_demographics) +
    #   geom_point(aes(x = gdp, y = lifeexpectancy,
    #                  label = country, size = population, 
    #                  fill = region), shape = 21, colour = "black", alpha = 0.7) +
    #   labs(title = input$year, fill = "Region") + 
    #   
    #   scale_y_continuous(name = "Life Expectancy at Birth",
    #                      labels = c("25 years","50 years","75 years"),
    #                      breaks = c(25, 50, 75),
    #                      limits = c(15, 80)
    #   ) +
    #   scale_x_log10(name = 'GDP Per Capita (Inflation-Adjusted)',
    #                 labels = c("$500", "$5000", "$50000"),
    #                 breaks = c(500, 5000, 50000),
    #                 limits = c(500, 110000)
    #   )+
    #   scale_fill_manual(values = c("green","red", "yellow", "purple", "black", "blue")) +
    #   
    #   scale_size_continuous(guide = FALSE, range = c(1, 21)) +
    #   
    #   theme(text = element_text(size=15))
    # 
  })
})