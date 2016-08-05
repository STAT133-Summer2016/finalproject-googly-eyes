shinyUI(fluidPage(
  titlePanel("Movies Query"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("checkGroupYear", 
                         label = h3("Year"), 
                         choices = list("2000" = 2000, 
                                        "2001" = 2001, 
                                        "2002" = 2002,
                                        "2003" = 2003, 
                                        "2004" = 2004, 
                                        "2005" = 2005,
                                        "2006" = 2006, 
                                        "2007" = 2007, 
                                        "2008" = 2008,
                                        "2009" = 2009, 
                                        "2010" = 2010, 
                                        "2011" = 2011,
                                        "2012" = 2012, 
                                        "2013" = 2013, 
                                        "2014" = 2014,
                                        "2015" = 2015),
                         selected = 1),
      
      checkboxGroupInput("checkGroupGenre", 
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
                         selected = 1),
      
      radioButtons("contentRating", label = h3("Content Rating"),
                   choices = list("R" = "R"  ,
                                  "PG" = "PG",
                                  "PG-13" = "PG-13",
                                  "NC-17" = "NC-17"),
                   selected = 1),
      
      
      radioButtons("arrange", label = h3("Arrange By"),
                   choices = list("Rating" = rating,
                                  "Meta Score" = metaScore,
                                  "Gross" = gross,
                                  "Popularity" = popularity),
                   selected = 1)
      
    ),
    mainPanel(
      plotOutput("demo_plot")
    )
  )
))