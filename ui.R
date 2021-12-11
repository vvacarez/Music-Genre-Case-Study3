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
             tags$h4("vvacarez@wpi.edu"))
  ))