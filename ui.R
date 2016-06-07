### Recommenderlab Example with Shiny by Michael Hahsler
###
### This work is licensed under the Creative Commons Attribution 4.0
### International License (http://creativecommons.org/licenses/by/4.0/).
### For questions please contact Michael Hahsler at http://michael.hahsler.net.

library(shiny)
library(ShinyRatingInput)
library(DT)

shinyUI(
  fluidPage(
    tags$head(
      # add some custom CSS
      tags$style(HTML('
        /* @import url("//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css");*/
        .orange {
        color: #ff6600;
        }
        .yellow {
        color: #CCCC00;
                      }
        .red {
        color: red;
        }
        '))
      ),
    
    titlePanel("Movie Recommender Using MovieLense Data"),
    
    sidebarLayout(position = "left",
                  sidebarPanel(
                    selectInput("current_user", label = p("Log-in as"),
                                choices = list("USER1", "USER2"),
                                selected = "USER1"),
                    #p("Rate the following movies:"),
                    
                    ### create the sliders
#                     lapply(1:3, function(i) sliderInput(paste0("slider", i),
#                                                         label = textOutput(paste0("movie", i)),
#                                                         #min = -10, max = 10, value = 0)),
#                                                         min = 1, max = 5, value = 3)),
                    
#                     selectInput("select_algo", label = p("Select recommender algorithm"),
#                                 choices = list("POPULAR", "UBCF", "IBCF", "SVD", "RANDOM"),
#                                 selected = "UBCF"),
                    
                    numericInput("num_recoms",
                                 label = p("Number of recommended movies"),
                                 value = 5, min = 1, max =10, step = 1),
                    
                    actionButton("new_recoms", "Change movies to rate")
                  ),
                  
                  mainPanel(
                            h3("Rate the following movies:"),
                            tags$table(
                              tags$tr(
                                lapply(1:3, movieName <- function(i) {tags$td(h5(textOutput(paste0("movie", i)), style="width: 200px;"))})
                              ),
                              tags$tr(
                                lapply(1:3, movieName <- function(i) {tags$td(imageOutput(paste0("poster", i), width = "100px", height = "150px"))})
                              ),
                              tags$tr(
                                lapply(1:3, ratingSlider <- function(i) {tags$td(ratingInput(paste0("slider", i), 
                                                                          dataFilled="fa fa-star fa-1x yellow", 
                                                                          dataEmpty="fa fa-star-o fa-1x yellow",
                                                                          dataStart=0, 
                                                                          dataStop=5,
                                                                          label=""))})
                              )
                            ),
#                             lapply(1:3, ratingSlider <- function(i) {ratingInput(paste0("slider", i), 
#                                                                          dataFilled="fa fa-star fa-1x orange", 
#                                                                          dataEmpty="fa fa-star-o fa-1x orange", 
#                                                                          label=imageOutput(paste0("movie", i), width = "200px", height = "400px"));}),
#                             h3("Recommendations from selected algorithm:"),
#                             tableOutput("movie_recom"), 
                            h3("You will love these:"),
                            tableOutput("movie_hybridrecom")
                            #DT::dataTableOutput("movie_hybridrecom")
                  )
    )
  )
)
