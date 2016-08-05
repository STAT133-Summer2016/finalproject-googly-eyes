# use Lab 21 and CreatingShiny as referece
shinyUI(fluidPage(
  theme = "bootstrap.css",
  titlePanel("Movies Query"),
  em("by Rongzhao Huang"),
  br(),
  br(),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("year_range", label = h3("Range of years:"), min = 2000, max = 2015, value = c(2000, 2015), 
                  format = "####"),
      hr(),
      
      checkboxGroupInput("genre", 
                         label = h3("Genre"), 
                         choices = list("Action" = "Action",
                                        "Animation" = "Animation", 
                                        "Comedy" = "Comedy",
                                        "Documentary" = "Comedy",
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
                         selected = "Action"),
      hr(),
      
      radioButtons("contentRating", label = h3("Content Rating"),
                   choices = list("G" = "G",
                                  "PG" = "PG",
                                  "PG-13" = "PG-13",
                                  "NC-17" = "NC-17",
                                  "R" = "R"),
                   selected = "R"),
      
      hr(),
      
      img(src = "./ucb.png", height = "100", style = "margin-left:30%"),
      
      hr(),
    
      wellPanel(
        helpText(   a("Look it up on Github!", href="https://github.com/hrzlvn", taget = "blank")
        )
      )
    

    ),
    mainPanel(
      
      tabsetPanel(
        
        tabPanel("Movie List", dataTableOutput("tbl")),
      
        tabPanel("I am feeling lucky!")
        )
      )
    )
  )
)