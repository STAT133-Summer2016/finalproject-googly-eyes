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
  titlePanel("How to produce the most profitable movie"),
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
        
        tabPanel("Box vs Budget by genre", 
                 textOutput("movie_by_genre_regression_line_helper"),
                 textOutput("movie_by_genre_regression_line"), 
                 textOutput("movie_by_genre_regression_line_r"),
                 plotOutput("Box_vs_Budget_by_genre")),
        
        tabPanel("Most frequnt keywords", plotOutput("Most_Frequent_Key_Words")),
        
        tabPanel("Box vs Budget of Directors", ggvisOutput("Box_vs_Budget_of_Directors")),
        
        tabPanel("Box vs Budget of Actors", ggvisOutput("Box_vs_Budget_of_Actors")),

        tabPanel("Box vs Rating Numbers Low Budget Movies", textOutput("box_vs_ratingNum_lb_regression_line"), plotOutput("Box_vs_Rating_Users_1")),
        
        tabPanel("Box vs Rating Numbers", textOutput("box_vs_ratingNum_hb_regression_line"), plotOutput("Box_vs_Rating_Users_2")),
        
        tabPanel("Box vs IMDB Rating Low Budget", textOutput("box_vs_rating_lb_regression_line"), plotOutput("Box_vs_Rating_1")),

        tabPanel("Box vs IMDB Rating", textOutput("box_vs_rating_hb_regression_line"), plotOutput("Box_vs_Rating_2")),
        
        tabPanel("Box vs Meta Score Low Budget", textOutput("box_vs_metaScore_lb_regression_line"), plotOutput("Box_vs_MetaScore_1")),
        
        tabPanel("Box vs Meta Score", textOutput("box_vs_metaScore_hb_regression_line"), plotOutput("Box_vs_MetaScore_2")),
        
        tabPanel("Box_vs_Review_Users_metacritiics", plotOutput("Box_vs_Review_Users")),
        
        tabPanel("Box_vs_Review_Critics", plotOutput("Box_vs_Review_Critics")),
        
        tabPanel("Profit vs Genre", plotOutput("Profit_vs_genre")),
        
        tabPanel("Genre Trend Number",
                 sliderInput("genre_year", 
                                label = h3(""), 
                                min = 2000, 
                                max = 2015, 
                                value = 2000, 
                                format = "####",
                                animate = TRUE),
                 plotOutput("genre_trend_number")),
        
        tabPanel("Average Gross for Each Month", plotOutput("graph_gross_month")),
        
        tabPanel("Average Gross for Each Weekday", plotOutput("graph_gross_weekday"))
        
        )
      )
    )
  )
)