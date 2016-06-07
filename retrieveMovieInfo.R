library(rjson)
library(httr)

load("MovieLense2016.rda")
movieInfo <- MovieLense2016Meta
movieInfo$title <- as.character(movieInfo$title)
movieInfo$imdbID <- "imdbID"
movieInfo$posterURL <- "PosterURL"

for(i in 1:nrow(movieInfo)){
  print(i)
  title <- gsub(", .*| \\(.*\\)", "", movieInfo[i,]$title)
  
  jsonInfo <- fromJSON(rawToChar((GET(build_url(parse_url(paste0("http://www.omdbapi.com/?t=", title)))))$content))
  
  if(is.null(jsonInfo$imdbID)){
    print(paste("No ID", title))
  }
  
  if(is.null(jsonInfo$Poster)){
    print(paste("No Poster", title))
  }
  
  movieInfo[i,]$imdbID <- jsonInfo$imdbID
  movieInfo[i,]$posterURL <- jsonInfo$Poster
}

save(movieInfo, file = "MovieLense2016Meta_new.rda")