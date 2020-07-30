require(mlbench)
require(caret)
require(e1071)

control <- trainControl(method="repeatedcv", number=10, repeats=3, search = "grid")
tunegrid <- expand.grid(.mtry=c(1:15))
metric <- "Accuracy"


rf_gridsearch <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                       data = base_treinamento, method="rf", metric=metric,
                       tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)


modellist <- list()
for (ntree in seq(100,500, by = 50)) {
  fit <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare,
               data = base_treinamento, method="rf", metric=metric,
               tuneGrid=tunegrid, trControl=control, ntree = ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)



customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# train model
control <- trainControl(method="repeatedcv", number=10, repeats=3, search = "grid")
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(100, 150, 200, 250, 300, 350, 400, 450, 500))
custom <- train(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare,
                data = base_treinamento,
                method=customRF, metric=metric, 
                tuneGrid=tunegrid, trControl=control)
print(custom)
plot(custom)

attach(custom$results)
custom$results[order(-Accuracy),] 

