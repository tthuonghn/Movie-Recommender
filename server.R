### Recommenderlab Example with Shiny by Michael Hahsler
###
### This work is licensed under the Creative Commons Attribution 4.0
### International License (http://creativecommons.org/licenses/by/4.0/).
### For questions please contact Michael Hahsler at http://michael.hahsler.net.

### How to:
# 1. Start/open a Shiny project in R Studio
# 2. Edit ui.R and server.R and test (using Run App button)
# 3. Deploy:
#     a) Create a shinyapps.io account (https://www.shinyapps.io/)
#     b) Go to account and create a token and set account using package rsconnect
#     c) deploy using: library(rsconnect); deployApp()

library(shiny)
library(recommenderlab)
library(data.table)
library(DT)
library(rjson)
library(httr)

## load data
#rawRatings <- read.table("../ml-100k/u.data", sep="\t", stringsAsFactors = F)
#colnames(rawRatings) <- c("userid", "itemid", "rating", "time")
#db <- as(rawRatings, "realRatingMatrix")

load("MovieLense2016.rda")
load("MovieLense2016Meta_new.rda")
rawRatings <- MovieLense2016
rawMovieInfo <- subset(movieInfo, year >= 2000)
rawMovieInfo$title <- as.character(rawMovieInfo$title)
rawRatings <- rawRatings[, colnames(rawRatings)  %in% rawMovieInfo$title]
width <- "100px"
height <- "150px"

ubrecom <- readRDS("movie_ubrecom2016.rds")
ibrecom <- readRDS("movie_ibrecom2016.rds")
svdrecom <- readRDS("movie_svdrecom2016.rds")
poprecom <- readRDS("movie_poprecom2016.rds")
randrecom <- readRDS("movie_randrecom2016.rds")

shinyServer(
  function(input, output, session) {
    ## pick random movies and display
    movies_to_rate <- reactive({
      ignore <- input$new_recoms  ### listen to button
      
      ratings <- NULL
      if(file.exists(paste0(input$current_user, ".rds"))){
        ratings <- readRDS(paste0(input$current_user, ".rds"))
      }
      
      rand_movies <- sample(nrow(rawMovieInfo), 3)
      
      output[[paste0("movie", 1)]] <- renderText(rawMovieInfo[rand_movies[1],]$title)
      output[[paste0("movie", 2)]] <- renderText(rawMovieInfo[rand_movies[2],]$title)
      output[[paste0("movie", 3)]] <- renderText(rawMovieInfo[rand_movies[3],]$title)
      #output[[paste0("movie", 4)]] <- renderText(rawMovieInfo[rand_movies[4],]$title)
      #output[[paste0("movie", 5)]] <- renderText(rawMovieInfo[rand_movies[5],]$title)
      
      for(j in 1:3){
        if(!file.exists(paste0("Posters//", rawMovieInfo[rand_movies[j],]$imdbID, ".jpg"))){
          if(rawMovieInfo[rand_movies[j],]$posterURL != "N/A"){
            download.file(rawMovieInfo[rand_movies[j],]$posterURL, paste0("Posters//", rawMovieInfo[rand_movies[j],]$imdbID, ".jpg"), mode = "wb")
          }else{
            file.copy(from = "Posters//noImage.jpg", to = paste0("Posters//", rawMovieInfo[rand_movies[j],]$imdbID, ".jpg"))
          }
        }
      }
      
#       output[[paste0("poster", 1)]] <- renderUI({
#                                               tags$img(src= paste0("Posters//", rawMovieInfo[rand_movies[1],]$imdbID, ".jpg"), width = "150", height = "225",
#                                                        alt= rawMovieInfo[rand_movies[1],]$title)
#                                           })
#       
#       output[[paste0("poster", 2)]] <- renderUI({
#                                               tags$img(src= paste0("Posters//", rawMovieInfo[rand_movies[2],]$imdbID, ".jpg"), width = "150", height = "225",
#                                                        alt= rawMovieInfo[rand_movies[2],]$title)
#                                             })
      
#       output[[paste0("poster", 3)]] <- renderUI({
#                                               tags$img(src= paste0("Posters//", rawMovieInfo[rand_movies[3],]$imdbID, ".jpg"), width = "150", height = "225",
#                                                        alt= rawMovieInfo[rand_movies[3],]$title)
#                                             })
      output[[paste0("poster", 1)]] <- renderImage({
                                                list(src = paste0("Posters//", rawMovieInfo[rand_movies[1],]$imdbID, ".jpg"),
                                                     width = width,
                                                     height = height,
                                                     alt = rawMovieInfo[rand_movies[1],]$title)
                                              }, deleteFile = FALSE)
      
      output[[paste0("poster", 2)]] <- renderImage({
                                                list(src = paste0("Posters//", rawMovieInfo[rand_movies[2],]$imdbID, ".jpg"),
                                                     width = width,
                                                     height = height,
                                                     alt = rawMovieInfo[rand_movies[2],]$title)
                                              }, deleteFile = FALSE)
      
      
      output[[paste0("poster", 3)]] <- renderImage({
                                              list(src = paste0("Posters//", rawMovieInfo[rand_movies[3],]$imdbID, ".jpg"),
                                                   width = width,
                                                   height = height,
                                                   alt = rawMovieInfo[rand_movies[3],]$title)
                                            }, deleteFile = FALSE)
      
      if(!is.null(ratings)){
        for(i in 1:3){
          rating <- ratings[[1, rand_movies[i]]]
          if(!is.na(rating)){
            updateSliderInput(session, paste0("slider", i), value = rating)
          }else{
            updateSliderInput(session, paste0("slider", i), value = 2)
          }
        }
      }
      
      rand_movies
    })
    
    ### create and change recommender
    recom <- reactive({
      if(input$select_algo == "POPULAR"){
        poprecom
      }else if(input$select_algo == "UBCF"){
        ubrecom
      }else if(input$select_algo == "IBCF"){
        ibrecom
      }else if(input$select_algo == "SVD"){
        svdrecom
      }else if(input$select_algo == "RANDOM"){
        randrecom
      }
        #Recommender(rawRatings, method = input$select_algo)
    })
    
    output$movie_hybridrecom <- renderTable({
      
      ### read ratings
      ratings <- matrix(NA, nrow = 1, ncol = ncol(rawRatings))
      colnames(ratings) <- colnames(rawRatings)
      if(file.exists(paste0(input$current_user, ".rds"))){
        ratings <- readRDS(paste0(input$current_user, ".rds"))
      }
      
      for(i in 1:3)
        ratings[1, movies_to_rate()[i]] <- as.numeric(input[[paste0("slider", i)]])
      
      saveRDS(ratings, file = paste0(input$current_user, ".rds"))
      
      ### create recommendations
      ubpred <- predict(ubrecom, as(ratings, "realRatingMatrix"),
                        n = input$num_recoms)
      ibpred <- predict(ibrecom, as(ratings, "realRatingMatrix"),
                        n = input$num_recoms)
      svdpred <- predict(svdrecom, as(ratings, "realRatingMatrix"),
                         n = input$num_recoms)
      poppred <- predict(poprecom, as(ratings, "realRatingMatrix"),
                         n = input$num_recoms)
      randpred <- predict(randrecom, as(ratings, "realRatingMatrix"),
                          n = input$num_recoms)
      
      print(rbind(
        cbind(movie = getList(ubpred)[[1]], rating = ubpred@ratings[[1]]),
        cbind(movie = getList(ibpred)[[1]], rating = ibpred@ratings[[1]]),
        cbind(movie = getList(svdpred)[[1]], rating = svdpred@ratings[[1]]),
        cbind(movie = getList(poppred)[[1]], rating = poppred@ratings[[1]]),
        cbind(movie = getList(randpred)[[1]], rating = randpred@ratings[[1]])
      ))
      
      pred_hybrid <- rbind(
        cbind(movie = getList(ubpred)[[1]], rating = ubpred@ratings[[1]]*0.35),
        cbind(movie = getList(ibpred)[[1]], rating = ibpred@ratings[[1]]*0.3),
        cbind(movie = getList(svdpred)[[1]], rating = svdpred@ratings[[1]]*0.2),
        cbind(movie = getList(poppred)[[1]], rating = poppred@ratings[[1]]*0.1),
        cbind(movie = getList(randpred)[[1]], rating = randpred@ratings[[1]]*0.05)
      )
      pred_hybrid <- data.table(pred_hybrid)
      pred_hybrid$rating <- as.numeric(pred_hybrid$rating)
      pred_hybrid <- pred_hybrid[, .(rating = sum(rating)), by = .(movie)][order(-rating)]
      
      print(pred_hybrid)
      
#       cbind('Recommended Movie' = pred_hybrid[1:input$num_recoms,]$movie,
#             'Predicted Rating' = pred_hybrid[1:input$num_recoms,]$rating)
      IMDBRatings <- c()
      
      for(j in 1:input$num_recoms){
        title <- gsub(", .*| \\(.*\\)", "", pred_hybrid[j,]$movie)
        jsonInfo <- fromJSON(rawToChar((GET(build_url(parse_url(paste0("http://www.omdbapi.com/?t=", title)))))$content))
        
        if(!is.null(jsonInfo$imdbRating)){
          IMDBRatings[j] <- paste0("<a href=\"http://www.imdb.com/title/", data.table(rawMovieInfo)[title == pred_hybrid[j,]$movie,]$imdbID, "\">",jsonInfo$imdbRating,"</a>")
        }else{
          IMDBRatings[j] <- "No rating available"
        }
        
      }
      
      table <- cbind(
        "Title" = pred_hybrid[1:input$num_recoms,]$movie,
        "IMDB Rating" = IMDBRatings
        )
      table
      #DT::datatable(recomResult, escape = FALSE)
    }, sanitize.text.function = function(x) x)
      
  }
)

