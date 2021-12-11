library(shiny)
library(ggvis)
library(dplyr)
library(ggplot2)

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

