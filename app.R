library(shiny)
library(ggvis)
library(dplyr)
library(ggplot2)




musicdata <- read.csv("music_genre.csv")
head(musicdata)

axis_vars <- c(
  "popularity" = "popularity", 
  "liveness" = "liveness", 
  "danceability" ="danceability",
  "energy" = "energy"
)

axis_vars

# Define UI for miles per gallon app ----
ui <- fluidPage(
                tabsetPanel(
                  tabPanel(title = "Home",
                           tags$img(height=800,
                                    width=1450,
                                    src="DS501.png")),
                  tabPanel(title = "About",
                           tags$h2("About this Shiny App"),
                           tags$h3(""),
                           tags$p("This App contains a dataset from Kaggle user Gaoyuan (",
                                   tags$a(href="https://www.kaggle.com/vicsuperman/prediction-of-music-genre","Prediction of music genre
Classify music into genres"),
                                   ") which explores Music Genres in comparison to song popularity, acousticness, danceability, energy, liveness tempo and other features. The dataset contains 10 music genres: 'Electronic', 'Anime', 'Jazz', 'Alternative', 'Country', 'Rap', 'Blues', 'Rock', 'Classical', 'Hip-Hop’ with 50,0005 songs explored. The use of genres alows for a Classification as a suitable algorithm in understanding audio parameters per genre and how popular the song is. Navigate to the ‘Visualization’ tab to explore how changing the songs audio production changes the popularity of the song."),
                           tags$h3(""),
                           tags$p("The Data Set will use Cluster analysis, or clustering, is an unsupervised machine learning task. In Visualization Tab, you will be able to change the parameters (input data) to view the groupings created in the graph. It involves automatically discovering natural grouping (clusters) in data."),
                           tags$h5("Dataset Table"),
                           fluidRow(
                             column(12,
                                    dataTableOutput('abouttable')
                             ))
                           ),
                  tabPanel(title = "Visualization",
                           titlePanel("Prediction of Music Genre"),
                           sidebarPanel(
                             helpText("Create Visualizations with selected data"),
                             fluidRow(
                               selectInput("var", 
                                           label = "Choose a Music Genre to Display",
                                           choices = c("Electronic", 
                                                       "Anime", 
                                                       "Jazz",
                                                       "Country", 
                                                       "Rap", 
                                                       "Blues",
                                                       "Rock", 
                                                       "Classical", 
                                                       "Hip-Hop",
                                                       "Alternative"),
                                           selected = "Hip-Hop"),
                               
                               sliderInput("popularity", 
                                           label = "Range of popularity:",
                                           min = 0, max = 100, value = c(0, 100)),
                               
                               sliderInput("liveness", 
                                           label = "Range of liveness:",
                                           min = 0, max = 1, value = c(0, 1)),
                               sliderInput("danceability", 
                                           label = "Range of danceability:",
                                           min = 0, max = 1, value = c(0, 1)),
                               sliderInput("energy", 
                                           label = "Range of energy:",
                                           min = 0, max = 1, value = c(0, 1)),
                               selectInput("xvar", 
                                           label = "X-axis variable",
                                           choices = axis_vars,
                                           selected = "danceability"),
                               
                               selectInput("yvar", 
                                           label = "Y-axis variable",
                                           choices = axis_vars,
                                           selected = "popularity"),
                               
                               tags$small(paste0(
                                 "Note: The visulation will only display data for 1 genre at a time.
            Please refer to tab 'ALL' in order to view all data"))
                             )
                           ),
                           
                           mainPanel(
                             column(9,
                                    ggvisOutput("plot1")
                           ),
                             dataTableOutput("sumartist")
                           )),
                  tabPanel(title = "Contact",
                           tags$h3("Author"),
                           tags$h3(""),
                           tags$h3("Valentina Vacarez"),
                           tags$h3(""),
                           tags$h4("vvacarez@wpi.edu"))
                ))

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  output$selected_var <- renderText({
    paste("You have selected", input$var)
  })
  
  output$abouttable <- renderDataTable({(musicdata)
  })
  
  output$popularity_min_max <- renderText({ 
    paste("You have chosen a popularity range that goes from",
          input$popularity[1], "to", input$popularity[2])
  })
  output$liveness_min_max <- renderText({ 
    paste("You have chosen a liveness range that goes from",
          input$liveness[1], "to", input$liveness[2])
  })
  
  output$danceability_min_max <- renderText({ 
    paste("You have chosen a danceability range that goes from",
          input$danceability[1], "to", input$danceability[2])
  })
  
  # Filter the songs returning a data frame
  
  dfmusic <- reactive({
    
    # Create values to filer
    genre <- input$var
    minpopularity <- input$popularity[1]
    maxpopularity <- input$popularity[2]
    minliveness <- input$liveness[1]
    maxliveness<- input$liveness[2]
    mindanceability <-input$danceability[1]
    maxdanceability<- input$danceability[2]
    minenergy<- input$energy[1]
    maxenergy <- input$energy[2]
    
    #Apply Filter
    m <- musicdata %>%
      filter(
        music_genre == genre,
        popularity >= minpopularity,
        popularity <= maxpopularity,
        liveness >= minliveness,
        liveness <= maxliveness,
        danceability >= mindanceability,
        danceability <= maxdanceability,
        energy >= minenergy,
        energy <= maxenergy,
      ) %>%
      arrange(artist_name)
    
    m <- as.data.frame(m)
    
    m$has_mode <- character(nrow(m))
    m$has_mode[m$mode == "Minor"] <- "Minor"
    m$has_mode[m$mode == "Major"] <- "Major"
    m
    
  })
 
  # A reactive expression with the ggvis plot
  
  vis <- reactive({
    # Lables for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    dfmusic %>%
      ggvis(x = xvar, y = yvar) %>%
      layer_points(size := 50, size.hover := 200,
                   fillOpacity := 0.2, fillOpacity.hover := 0.5,
                   stroke = ~has_mode) %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      add_legend("stroke", title = "Song Mode", values = c("Major", "Minor")) %>%
      scale_nominal("stroke", domain = c("Yes", "No"),
                    range = c("orange", "#aaa")) %>%
      set_options(width = 900, height = 650)
  })
  
  vis %>% bind_shiny("plot1")
  
}

# Run the app ----
shinyApp(ui = ui, server = server)