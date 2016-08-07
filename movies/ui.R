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
  # theme = "bootstrap.css",
  titlePanel("Movies Query"),
  em("by Rongzhao Huang"),
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
        helpText( a("Look it up on Github!", href="https://github.com/hrzlvn", taget = "blank")
        )
      )
    

    ),
    mainPanel(
      
      tabsetPanel(
  
        tabPanel("Movie List", dataTableOutput("tbl")),
        tabPanel("Genres and Box", plotOutput("graph1")),
        tabPanel("Directors and Box", ggvisOutput("ggvis1")),
        tabPanel("Actors and Box", ggvisOutput("ggvis2")),
        tabPanel("Genres, rating and Box", 
                 sliderInput("year_for_graph2", 
                             label = h3(""), 
                             min = 2000, 
                             max = 2015, 
                             value = 2000, 
                             format = "####",
                             animate = TRUE),
                  plotOutput("graph2")),
        tabPanel("rating user and box", plotOutput("graph3"))
        
        
        )
      )
    )
  )
)