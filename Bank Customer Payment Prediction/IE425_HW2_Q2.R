## Caret package
library(caret)
library(caTools)


raw_data=read.csv("C:\\Users\\fatma\\Desktop\\IE425_HW2\\SeoulBikeData.csv", sep=",")
head(raw_data)

#2a. Train Test Split
set.seed(425)
split<-sample.split(raw_data$Rented.Bike.Count, SplitRatio = 0.8)
train<-subset(raw_data, split==TRUE)
test<-subset(raw_data, split==FALSE)

#2b. Best Random Forest with 10-Fold CV
library(randomForest)
library(Metrics)

#Create cadidates for mtry and ntree.
mtry_tuning_seq <- seq(3,6,1)
ntree_tuning_seq <- seq(100,400,100)

best_accuracy <- Inf
#Find the best parameters by trying the different values with for loops.
for (i in mtry_tuning_seq){
  for (j in ntree_tuning_seq){
    best_parameters <- list()
    set.seed(425)
    ctrl <- trainControl(method='repeatedcv', number=10, repeats=5)
    rf_parameters <- randomForest(Rented.Bike.Count~., data=train, mtry=i, ntree=j,
                                  trControl=ctrl,importance=TRUE)
    rf_perdictions <- predict(rf_parameters, newdata=test)
    rmse_tree <- rmse(actual=test$y, predicted=rf_perdictions)
    if (accuracy <= best_accuracy){
      best_rmse <- rmse_tree
      new_parameters <- append(best_parameters, c(i,j))
    }
  }
}

best_parameters_int = as.integer(new_parameters)
new_parameters

cat("The best mtry: ",new_parameters[[1]], "and the best ntree: ", new_parameters[[2]])

#2c. Attribute Importance
set.seed(425)
#Cross Validation
ctrl <- trainControl(method='repeatedcv', number=10, repeats=5)
#Setting the model.
rf_final_model <- randomForest(Rented.Bike.Count~., data=train, mtry=as.integer(new_parameters[[1]]),
                               ntree=as.integer(new_parameters[[2]]), trControl=ctrl,importance=TRUE)
#Making the predictions.
rf_perdictions_final <- predict(rf_final_model, newdata=test)
#Importance table:
round(importance(rf_final_model),2)
varImpPlot(rf_final_model)

#2d. RMSE & MAE
rmse(actual = test$Rented.Bike.Count,predicted = rf_perdictions_final)
mae(actual = test$Rented.Bike.Count,predicted = rf_perdictions_final)

#2e. Gradient Boosting Machine Parameter Tuning 
install.packages("gbm")
library(gbm)

gbmGrid=expand.grid(interaction.depth = c(3,4,5), 
                    n.trees = (5:10)*10, 
                    shrinkage = (1:3)*0.1,
                    n.minobsinnode = 20)
set.seed(425)
library(caret)
ctrl1 <- trainControl(method="cv",number=10)
gbm_model <- train(Rented.Bike.Count~., data=train, method="gbm", metric="RMSE",
                   verbose = FALSE, trControl = ctrl1,tuneGrid = gbmGrid)
#Finding the min rmse index.
min_rmse <- which.min(gbm_model$results$RMSE)
cat("The best boosting tree is: shrinkage: ", gbm_model$results$shrinkage[min_rmse], 
    " intearction.depth  :", gbm_model$results$interaction.depth[min_rmse],
    " n.minobsinnode: ", gbm_model$results$n.minobsinnode[min_rmse],
    " n.trees: ", gbm_model$results$n.trees[min_rmse])

#2f. Final Prediction
set.seed(425)
ctrl1 <- trainControl(method="cv",number=10)
gbm_model_final <- train(Rented.Bike.Count~., data=train, method="gbm", metric="RMSE",
                         verbose = FALSE, trControl = ctrl1,
                         tuneGrid=expand.grid(shrinkage=0.3,
                                              interaction.depth=5,
                                              n.minobsinnode=20,
                                              n.trees=100))

gbm_perdictions <- predict(gbm_model_final, newdata=test)

rmse(actual = test$Rented.Bike.Count,predicted = gbm_perdictions)
mae(actual = test$Rented.Bike.Count,predicted = gbm_perdictions)


