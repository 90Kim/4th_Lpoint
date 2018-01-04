############################################
### http://rstatistics.tistory.com/31 ######
############################################


install.packages("recommenderlab")
library(recommenderlab)

data_packages <- data(package = "recommenderlab")
data_packages$results[, c("Item", "Title")]

data("Jester5k")
Jester5k

dim(Jester5k)
hist(getRatings(Jester5k), main = "Distribution of ratings")
as(Jester5k, "matrix")[1:10, 1:10]

set.seed(2017)
index <- sample(1:nrow(Jester5k), size = nrow(Jester5k) * 0.7)

train <- Jester5k[index, ]
test <- Jester5k[-index, ]

dim(train) 

recommender_models <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
recommender_models

recomm_model <- Recommender(data = train, method = "UBCF")
recomm_model

pred <- predict(recomm_model, newdata = test, n = 10)
str(pred)

pred_list <- sapply(pred@items, function(x) { colnames(Jester5k)[x] })

pred_list[1]
pred_list[1500]

table( unlist( lapply(pred_list, length) ) )
table(rowCounts(Jester5k))
mean(rowCounts(Jester5k))

data_modify <- Jester5k[rowCounts(Jester5k) <= 72]
dim(data_modify)

eval_sets <- evaluationScheme(data = data_modify,
                              method = "cross-validation",
                              train = 0.7,
                              k = 10,
                              goodRating = 3,
                              given = 30)

sapply(eval_sets@runsTrain, length)

getData(eval_sets, "train")

recomm_eval <- Recommender(data = getData(eval_sets, "train"),
                           method = "UBCF", 
                           parameter = NULL)
recomm_eval

pred_eval <- predict(recomm_eval, 
                     newdata = getData(eval_sets, "known"),
                     n = 10, type = "ratings")
pred_eval

accuracy_eval <- calcPredictionAccuracy(x = pred_eval,
                                        data = getData(eval_sets, "unknown"),
                                        byUser = TRUE)

head( accuracy_eval, 10 )
colMeans(accuracy_eval)

accuracy_eval2 <- evaluate(x = eval_sets, 
                           method = "UBCF")

head( getConfusionMatrix(accuracy_eval2) )



recomm_model2 <- Recommender(data = train, 
                             method = "IBCF",
                             parameter = list(k = 30))
recomm_model2
str( getModel(recomm_model2) )

pred2 <- predict(recomm_model2, newdata = test, n = 10)
pred_list2 <- sapply(pred2@items, function(x) { colnames(Jester5k)[x] })

pred_list2[1]
pred_list2[1500]

eval_sets2 <- evaluationScheme(data = data_modify,
                               method = "cross-validation",
                               train = 0.7,
                               k = 5,
                               goodRating = 3,
                               given = 15)
getData(eval_sets2, "train")

recomm_eval2 <- Recommender(data = getData(eval_sets2, "train"),
                            method = "IBCF", 
                            parameter = NULL)
recomm_eval2

pred_eval2 <- predict(recomm_eval2, 
                      newdata = getData(eval_sets2, "known"),
                      n = 10, type = "ratings")
pred_eval2

accuracy_eval2 <- calcPredictionAccuracy(x = pred_eval2,
                                         data = getData(eval_sets2, "unknown"),
                                         byUser = TRUE)

head( accuracy_eval, 10 )
colMeans(accuracy_eval)

accuracy_eval2 <- evaluate(x = eval_sets2, 
                           method = "IBCF", 
                           n = seq(10, 100, by = 10))
head( getConfusionMatrix(accuracy_eval2) )

plot(accuracy_eval2, annotate = TRUE, main = "ROC Curve")
plot(accuracy_eval2, "prec/rec", annotate = TRUE, main = "Precision-Recall")
