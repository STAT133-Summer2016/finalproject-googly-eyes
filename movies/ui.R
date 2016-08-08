# use Lab 21 and CreatingShiny as referece
library(ggvis)

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

shinyUI(fluidPage(
  theme = "bootstrap.css",
  titlePanel("Movies Query"),
  em("by Guanghongfu, Wenjun Zeng, Rongzhao Huang, Bian Yin"),
  br(),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", 
                  label = h3("Range of years:"), 
                  min = 2000, 
                  max = 2015, 
                  value = c(2000, 2015), 
                  format = "####"),
      hr(),
      
      radioButtons("genre", 
                         label = h3("Genre"), 
                         choices = list("All" = "[a-zA-Z]", 
                                        "Action" = "Action",
                                        "Animation" = "Animation", 
                                        "Comedy" = "Comedy",
                                        "Documentary" = "Documentary",
                                        "Family" = "Family",
                                        "Horror" = "Horror",
                                        "Musical" = "Musical",
                                        "Romance" = "Romance",
                                        "Sport" = "Sport",
                                        "War" = "War",
                                        "Adventure" = "Adventure",
                                        "Biography" = "Biography",
                                        "Crime" = "Crime",
                                        "Drama" = "Drama",
                                        "Fantasy" = "Fantasy",
                                        "History" = "History",
                                        "Music" = "Music",
                                        "Mystery" = "Mystery",
                                        "Sci-Fi" = "Sci-Fi",
                                        "Thriller" = "Thriller",
                                        "Western" = "Western"
                         ),
                         selected = "All"),
      hr(),
      
      radioButtons("contentRating", label = h3("Content Rating"),
                   choices = list("All" = ".*",
                                  "G" = "G",
                                  "PG" = "PG",
                                  "PG-13" = "PG-13",
                                  "NC-17" = "NC-17",
                                  "R" = "R"),
                   selected = "All"),
      
      hr(),
      
      img(src = "./ucb.png", height = "100", style = "margin-left:30%"),
      
      hr(),
    
      wellPanel(
        helpText( a("Look it up on Github!", href="https://github.com/STAT133-Summer2016/finalproject-googly-eyes", taget = "blank")
        )
      )
    

    ),
    mainPanel(
      
      tabsetPanel(
  
        tabPanel("Movies Ranking", 
                 radioButtons("datasetSelection", 
                              label = h3("Select Dataset"),
                              choices = list("IMDB" = "IMDB",
                                             "Rotten Tomatoes" = "RottenTomatoes"),
                              selected = "IMDB"),
                 dataTableOutput("tbl")),
        tabPanel("Box vs Budget by genre", plotOutput("Box_vs_Budget_by_genre")),
        tabPanel("Box vs Budget of Directors", ggvisOutput("Box_vs_Budget_of_Directors")),
        tabPanel("Box vs Budget of Actors", ggvisOutput("Box_vs_Budget_of_Actors")),
        tabPanel("Rating vs Users by Genre", 
                 radioButtons("datasetSelection_rug", 
                              label = h3("Select Dataset"),
                              choices = list("IMDB" = "IMDB",
                                             "Rotten Tomatoes" = "RottenTomatoes"),
                              selected = "IMDB"),
                 sliderInput("year_for_graph2", 
                             label = h3(""), 
                             min = 2000, 
                             max = 2015, 
                             value = 2000, 
                             format = "####",
                             animate = TRUE),
                  plotOutput("Rating_vs_Users_by_Genre")),
        
        tabPanel("Box vs Rating Users", plotOutput("Box_vs_Rating_Users")),
        
        tabPanel("Genre Trend",
                 sliderInput("genre_year", 
                                              label = h3(""), 
                                              min = 2000, 
                                              max = 2015, 
                                              value = 2000, 
                                              format = "####",
                                              animate = TRUE),
                 plotOutput("graph_genre_trend")),
        
        tabPanel("Average Opening Week Box for each month", plotOutput("graph_box_month"))
        
        
        )
      )
    )
  )
)