library(recommenderlab)

load("MovieLense2016.rda")
rawRatings <- MovieLense2016

latestMovies <- subset(MovieLense2016Meta, year >= 2000)
rawRatings <- rawRatings[, unique(colnames(rawRatings)) %in% latestMovies$title]

train <- rawRatings[1:floor(nrow(rawRatings)*0.7),]
test <- rawRatings[floor(nrow(rawRatings)*0.7):nrow(rawRatings),]

print("Train UBIF")
ubrecom <- Recommender(train, method = "UBCF")
print("Train IBIF")
ibrecom <- Recommender(train, method = "IBCF")
print("Train SVD")
svdrecom <- Recommender(train, method = "SVD")
print("Train POPULAR")
poprecom <- Recommender(train, method = "POPULAR")
print("Train RANDOM")
randrecom <- Recommender(train, method = "RANDOM")

saveRDS(ubrecom, file = "movie_ubrecom2016.rds")
saveRDS(ibrecom, file = "movie_ibrecom2016.rds")
saveRDS(svdrecom, file = "movie_svdrecom2016.rds")
saveRDS(poprecom, file = "movie_poprecom2016.rds")
saveRDS(randrecom, file = "movie_randrecom2016.rds")

es <- evaluationScheme(rawRatings, method="split", 
                 train=0.7, k=1, goodRating = 4, given=2)
algorithms <- list(
  RANDOM = list(name = "RANDOM", param = NULL),
  POPULAR = list(name = "POPULAR", param = NULL),
  IBCF = list(name = "IBCF", param = NULL),
  UBCF = list(name = "UBCF", param = NULL),
  SVD = list(name = "SVD", param = NULL)
)
ev <- evaluate(es, algorithms, type="topNList", n=c(1, 3, 5, 10))
plot(ev, "prec/rec", annotate=TRUE)
