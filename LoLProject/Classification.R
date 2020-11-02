setwd("C:/Users/alkim/Desktop/LolProject")
              #League of Legends
          ##How the first 10min features effects our winning conditions
        #Try to predict that blue team win or loose game with considering first ten minute. 

######library#####
library(readr)
library(dplyr)
library(caTools)
library(tree)
library(ggplot2)
library(pROC)
library(caret)
library(car)
library(mfx)
library(ROCR)
#####
        
######Importing dataset
all_dataset <- read_csv("dataset.csv")

summary(all_dataset)
names(all_dataset)
#We have variables which are very similar to each other. It can be problem in our future progress. Because of that some of the variables should be eliminated. 
#Variables that I am working with explanation. 
    
      #BlueWins: 0 represent loose, 1 represent win for blueteam.
      #blueWardsPlaced: How many wards placed until the firs 10 minute. It is very important the protect theirselves. Wards can increase the vision at the map and shows unseen places to players.
      #blueWardsDestroyed: Number of enemy warding totems the blue team has destroyed.
      #blueFirstBlood: It is dummy variable and very important for the teams to get a early lead. For some champion it can be huge effects on winning conditions. Most of the teams have plan to get first blood. 
      #blueKills: Represent the number of kill. Kill can increase the gold and experience of players.
      #blueDeath: Number of death. Death can cause the loose the experience and golds with loosing minions. Also, it can increase the power of opponent. 
      #blueDragons: Dragons are monsters in map. When one team kill the dragon, they can have some specific increase in players' features like higher movements speed or increase in armors. Occurs at 05:00
      #blueHeralds: It is very important objective for teams to get a early lead. Rift Herald can give the huge damage the enemies towers. It is occurs at 8:00
      #blueTowersDestroyed: The number of towers destreyoed by blue team. Towers can increase gold and experience
      #blueGoldDifference The total difference btw two team at 10 min.  
      #blueExperienceDifference: Experience is very important to have higher skills. In early game, even small experience differences can have huge effects. 
      #blueTotalMinionsKilled: When player killed a minion, player gain a gold and experience.
      #TotalJungleMinionsDiff: Differences between two team junglers. It represent the which jungler earn better experience. Sometimes positive differences can have negative effects. 
      #redWardsPlaced: how many wards placed by opponent team.
      #redWardsDestroyed: Number of enemy warding totems the blue team has destroyed. It is important because timing and placement of wards are very important.
      #redDragons: They have or not.
      #redHerald: They have or not.
      #redTowersDestroyed: number of towers destroyed by red team at 10:00
      #redTotalMinionsKilled: When player killed a minion, player gain a gold and experience. Higher minions should have negative effect to blue team. 

##Choose the related variables 
dataset <- all_dataset %>% select(blueWins, blueWardsPlaced, blueWardsDestroyed, blueFirstBlood, blueKills, blueDeaths, blueAssists, blueDragons, blueHeralds, blueTowersDestroyed, blueTotalMinionsKilled, 
                                  blueTotalJungleMinionsKilled, redTotalJungleMinionsKilled, redWardsPlaced, redWardsDestroyed, redDragons, redHeralds, redTowersDestroyed, redTotalMinionsKilled)
#Create a TotalJungleMinionsDiff variable 
dataset$TotalJungleMinionsDiff <- dataset$blueTotalJungleMinionsKilled - dataset$redTotalJungleMinionsKilled
dataset <- dataset %>% select(-blueTotalJungleMinionsKilled, -redTotalJungleMinionsKilled)

##checking the structure of our variables. 
summary(dataset) ##It is imposible to placed 250 ward at 10 min.
par(c(2,2))

ggplot(dataset, aes(x = blueWardsPlaced)) + geom_boxplot()
ggplot(dataset, aes(x = blueWardsPlaced)) + geom_boxplot()
dataset %>% filter(blueWardsPlaced > 75 | redWardsPlaced > 75)
## I should check values higher than 50
 ##11 percent of our data can be included problem. 
problem <- dataset %>% filter(blueWardsPlaced > 75 | redWardsPlaced > 75)
##They should be eliminated. I check the lots of proffesional match, I never see this number of ward placement, even warding is significantly important for these matches. 
dataset <- dataset %>% filter(blueWardsPlaced < 75)
dataset <- dataset %>% filter(redWardsPlaced < 75) 

str(dataset)
dataset$blueWins <- as.factor(dataset$blueWins) 
dataset$blueDragons <- as.factor(dataset$blueDragons) ## Until the first 10 minute, there is only one dragon for teams. Only one team can have it or two team can choose to not take dragon. 
dataset$blueFirstBlood <- as.factor(dataset$blueFirstBlood) ##1 for blue team take, 0 for red team take. 
isTRUE(print(all_dataset$blueFirstBlood == all_dataset$redFirstBlood)) ##there is no match in our dataset both team cannot take firstblood until 10.00
dataset$blueHeralds <- as.factor(dataset$blueHeralds) ## Only one team can have it or two team can choose to not take herald until 10:00. It occurs in 8.00 
dataset$redDragons <- as.factor(dataset$redDragons)
dataset$redHeralds <- as.factor(dataset$redHeralds)

boxplot(dataset)

##Prepare for analysis
##Create train and test data 
set.seed(10)
data_split <- caTools::sample.split(dataset$blueWins, 0.75) 

train <- subset(dataset ,data_split==T)
test <- subset(dataset,data_split==F)
##control the frequency of resposne variable
train %>% count(blueWins) %>% mutate( prop = n/sum(n)) 
test %>% count(blueWins) %>%  mutate( prop = n/sum(n)) 



##standartization for numeric variables
nonstartd_train <- train
##train
train$blueKills = scale(train$blueKills, center = T, scale = T)[,1]
train$blueWardsPlaced = scale(train$blueWardsPlaced, center = T, scale = T)[,1]
train$blueWardsDestroyed = scale(train$blueWardsDestroyed, center = T, scale = T)[,1]
train$blueDeaths = scale(train$blueDeaths, center = T, scale = T)[,1]
train$blueAssists = scale(train$blueAssists, center = T, scale = T)[,1]
train$blueTotalMinionsKilled = scale(train$blueTotalMinionsKilled, center = T, scale = T)[,1]
train$TotalJungleMinionsDiff = scale(train$TotalJungleMinionsDiff, center = T, scale = T)[,1]
train$redWardsPlaced = scale(train$redWardsPlaced, center = T, scale = T)[,1]
train$redWardsDestroyed = scale(train$redWardsDestroyed, center = T, scale = T)[,1]
train$redTowersDestroyed = scale(train$redTowersDestroyed, center = T, scale = T)[,1]
train$redTotalMinionsKilled = scale(train$redTotalMinionsKilled, center = T, scale = T)[,1]

##test 
test$blueKills <- (test$blueKills - mean(nonstartd_train$blueKills)) / sd(nonstartd_train$blueKills)
test$blueWardsPlaced <- (test$blueWardsPlaced - mean(nonstartd_train$blueWardsPlaced)) / sd(nonstartd_train$blueWardsPlaced)
test$blueWardsDestroyed <- (test$blueWardsDestroyed - mean(nonstartd_train$blueWardsDestroyed)) / sd(nonstartd_train$blueWardsDestroyed)
test$blueDeaths <- (test$blueDeaths - mean(nonstartd_train$blueDeaths)) / sd(nonstartd_train$blueDeaths)
test$blueAssists <- (test$blueAssists - mean(nonstartd_train$blueAssists)) / sd(nonstartd_train$blueAssists)
test$blueTotalMinionsKilled <- (test$blueTotalMinionsKilled - mean(nonstartd_train$blueTotalMinionsKilled)) / sd(nonstartd_train$blueTotalMinionsKilled)
test$TotalJungleMinionsDiff <- (test$TotalJungleMinionsDiff - mean(nonstartd_train$TotalJungleMinionsDiff)) / sd(nonstartd_train$TotalJungleMinionsDiff)
test$redWardsPlaced <- (test$redWardsPlaced - mean(nonstartd_train$redWardsPlaced)) / sd(nonstartd_train$redWardsPlaced)
test$redTotalMinionsKilled <- (test$redTotalMinionsKilled - mean(nonstartd_train$redTotalMinionsKilled)) / sd(nonstartd_train$redTotalMinionsKilled)
test$redWardsDestroyed <- (test$redWardsDestroyed - mean(nonstartd_train$redWardsDestroyed)) / sd(nonstartd_train$redWardsDestroyed)
test$redTowersDestroyed <- (test$redTowersDestroyed - mean(nonstartd_train$redTowersDestroyed)) / sd(nonstartd_train$redTowersDestroyed)

GGally::ggcorr(train)
##logistic regression, choosing best model y = blueWins

model <- glm(blueWins~., data = train, family = "binomial")
summary(model)
##best model selection
MASS::stepAIC(model, direction = "both")
best_model <- glm(blueWins ~ blueFirstBlood + blueKills + blueDeaths + blueDragons + 
  blueHeralds + blueTowersDestroyed + blueTotalMinionsKilled + 
  redWardsDestroyed + redDragons + redTowersDestroyed + redTotalMinionsKilled + 
  TotalJungleMinionsDiff, family = "binomial", data = train)
summary(best_model)
mfx::logitor(formula = blueWins ~ blueFirstBlood + blueKills + blueDeaths + blueDragons + 
               blueHeralds + blueTowersDestroyed + blueTotalMinionsKilled + 
               redWardsDestroyed + redDragons + redTowersDestroyed + redTotalMinionsKilled + 
               TotalJungleMinionsDiff, data = train)

car::vif(best_model)
##test / train evulation
#train confusion matrix 
train$pred <- predict(best_model, train, type="response")
train$good_pred <- ifelse(train$pred > 0.5, 1, 0)
p_class <- factor(train$good_pred, levels = levels(train[["blueWins"]]))
caret::confusionMatrix(p_class, train[["blueWins"]])

library(ROCR)
test$pred <- predict(best_model, newdata=test, type="response")
test$good_pred <- ifelse(test$pred > 0.5, 1, 0)
p_class_test <- factor(test$good_pred, levels = levels(test[["blueWins"]])) ##test and train acc is very close to each other, even it is no too high. 
caret::confusionMatrix(p_class_test, test[["blueWins"]])
##ROCR for train
par(mfrow=c(1,2))
predict_train <- predict(best_model, newdata =train, type = "response") 
ROCRtrain <- prediction(predict_train, train$blueWins)
ROCtrainperf <- performance(ROCRtrain, 'tpr', 'fpr')
plot(ROCtrainperf) + title("Train")
##ROCR for test
predict_test <- predict(best_model, newdata = test, type = "response")
ROCRpred <- prediction(predict_test, test$blueWins)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf) + title("Test")

##auc for test
auc_glm = performance(ROCRpred, "auc")
gmb_auc_test = as.numeric(auc_glm@y.values)
gmb_auc_test
auc_glm = performance(ROCRtrain, "auc")
gmb_auc_train = as.numeric(auc_glm@y.values)
gmb_auc_train
##fix dataframes
train <- train[1:17]
test <- test[1:17]

    ###Algorithims 
  ##Decision Tree
par(mfrow = c(1,1))
tree_train <- tree(blueWins~., train)
summary(tree_train)
plot(tree_train)
text(tree_train, pretty = 0)

cv_train <- cv.tree(tree_train)
plot(cv_train$size, log(cv_train$dev), type = 'b') ##I do not need to prune proccess. 

predicted_test <- predict(tree_train, 
                          newdata = test)
table(predicted_test)

train_tree <-  predict(tree_train, data = train, type = "class")
test_tree <- predict(tree_train, newdata = test, type = "class")

caret::confusionMatrix(train_tree, reference = train$blueWins)   # Accuracy = 0.6756
#confusion matrix: test
caret::confusionMatrix(test_tree, reference = test$blueWins)   # Accuracy = 06723
##decision tree roc curve
decision_test = prediction(as.numeric(test_tree), test$blueWins)
decision_train = prediction(as.numeric(train_tree), train$blueWins)
DT_ROCtrain = performance(decision_train, "tpr", "fpr")
plot(DT_ROCtrain)
plot(DT_ROCtrain) + title("Train")

DT_ROC = performance(decision_test, "tpr", "fpr")
par(mfrow = c(1,2))
plot(DT_ROC) + title("Test")
plot(DT_ROCtrain) + title("Train")
plot(DT_ROC, legend = c("GBM"), col = c("green"), lty =1:2, cex = 0.6, title("Test"))
auc_dt = performance(decision_test, "auc")
auc_dtrain = performance(decision_train, "auc")
dt_auc_test = as.numeric(auc_dt@y.values)
dt_auc_test
dt_auc_train = as.numeric(auc_dtrain@y.values)
dt_auc_train
##random forest
set.seed(1) 
library(randomForest)
rf_lol = randomForest(blueWins~.,
                      data = train,
                      mtry = sqrt(16),
                      importance = T)
rf_lol ##the oob estimate of error is %28 for train 

lol_estimate = predict(rf_lol, 
                       newdata = test)
rf_lol3 = randomForest(blueWins~.,
                      data = train,
                      mtry = sqrt(4),
                      importance = T,
                      ntree = 1000)

lol2_estimate = predict(rf_lol2, newdata = test)

##similar to decision tree, kills and deaths are very important for our model. 
features <- setdiff(names(train), "blueWins")

tuned = tuneRF(train[features],
               train$blueWins,
               stepFactor = 0.5,
               plot = TRUE,
               ntreeTry = 1000,
               trace = TRUE,
               improve = 0.05) ##same eerror with random forest 2 
rf_lol3 = randomForest(blueWins~.,
                       data = train,
                       mtry = sqrt(4),
                       importance = T,
                       ntree = 1000)
lol3_estimate = predict(rf_lol3, newdata = test)
lol3_train = predict(rf_lol3, newdata = train)
train$prediction = lol3_train
test$prediction = lol3_estimate

varImpPlot(rf_lol3)
caret::confusionMatrix(data = test$blueWins, reference = test$prediction)  ##test confusion matrix for our random forest proccess. I have 0.71 Accuracy 
caret::confusionMatrix(data = train$blueWins, reference = train$prediction)  ##train confusion matrix for our random forest proccess. I have 0.95 Accuracy 


##roc curve
test$blueWins = as.factor(test$blueWins)
caret::confusionMatrix(lol2_estimate, reference = test$blueWins)
randomforest_test = prediction(as.numeric(lol3_estimate), test$blueWins)
randomforest_train = prediction(as.numeric(lol3_train), train$blueWins)

RF_ROC = performance(randomforest_test, "tpr", "fpr")
RF_ROCtrain = performance(randomforest_train, "tpr", "fpr")
par(mfrow=c(1,2))
plot(RF_ROC) + title("Test")
plot(RF_ROCtrain) + title("Train")
plot(RF_ROC, legend = c("GBM"), col = c("green"), lty =1:2, cex = 0.6)
auc_rf = performance(randomforest_test, "auc")
rf_auc_test = as.numeric(auc_rf@y.values)
rf_auc_test

auc_rf_train = performance(randomforest_train, "auc")
rf_auc_train = as.numeric(auc_rf_train@y.values)
rf_auc_train

######Boosting
library(gbm)
train$blueWins = as.character(train$blueWins)
lol_boost = gbm(blueWins~., 
                data = train,
                distribution = "bernoulli", 
                n.trees = 5000, 
                interaction.depth = 4,
                shrinkage = 0.01)
lol_boost
summary(lol_boost) 
lolEffects <- tibble::as_tibble(gbm::summary.gbm(lol_boost, 
                                                  plotit = FALSE))
lolEffects %>% utils::head() 

ggplot(lolEffects, aes(x = reorder(var, rel.inf), y = rel.inf)) + geom_col(aes(fill = rel.inf)) + coord_flip() + 
  ggtitle("Variables and Relative Influence - Model 1") + theme_minimal() + xlab("Features") + ylab("Relative Influence")
###finding best boosting model for my project
cvperformance = gbm.perf(lol_boost, method = "OOB")
print(cvperformance)
lol_boost = gbm(blueWins~., 
                data = train,
                distribution = "bernoulli", 
                n.trees = 478, 
                interaction.depth = 4,
                shrinkage = 0.01)
lolEffects2 <- tibble::as_tibble(gbm::summary.gbm(lol_boost, 
                                                 plotit = FALSE))
lolEffects2 %>% utils::head() 

ggplot(lolEffects2, aes(x = reorder(var, rel.inf), y = rel.inf)) + geom_col(aes(fill = rel.inf)) + coord_flip() + 
  ggtitle("Variables and Relative Influence - Model 2") + theme_minimal() + xlab("Features") + ylab("Relative Influence")

boost_estimate = predict(lol_boost, newdata = test, n.trees = 478, interaction.depth = 4, distribution = "bernouilli")
boost_estimatetrain = predict(lol_boost, newdata = train, n.trees = 478, interaction.depth = 4, distribution = "bernouilli")

##confusionMatrix for boossssssst
boostpredicts = as.factor(ifelse(boost_estimate> 0.7,1,0))
test$blueWins = as.factor(test$blueWins)
caret::confusionMatrix(boostpredicts, reference = test$blueWins)
prediction_test = ROCR::prediction(boost_estimate, test$blueWins)

boostpredictstrain = as.factor(ifelse(boost_estimatetrain> 0.7,1,0))
train$blueWins = as.factor(train$blueWins)
caret::confusionMatrix(boostpredictstrain, reference = train$blueWins)
prediction_train = ROCR::prediction(boost_estimatetrain, train$blueWins)

GBM_ROC = ROCR::performance(prediction_test, "tpr", "fpr") 
plot(GBM_ROC) + title("Test")
GBM_ROCtrain = ROCR::prediction(prediction_test, "tpr", "fpr") 
plot(GBM_ROC) + title("Train")

auc.tmp = performance(prediction_test, "auc")
gmb_auc_test = as.numeric(auc.tmp@y.values)
gmb_auc_test ##0.79
auc.tmpTrain = performance(prediction_train, "auc")
gmb_auc_train = as.numeric(auc.tmpTrain@y.values)
gmb_auc_train

##bagging
library(ipred)
bagg_lol <- bagging(formula = blueWins~., 
                    data = train,
                    nbagg = 100,
                    coob = TRUE)
bagg_lol
bagginglol_predict <- predict(bagg_lol, 
                              newdata = train)
bagginglol_predicttest = predict(bagg_lol, newdata = test)
bagginglol_predicttrain = predict(bagg_lol, newdata = train)
# Performance Measurement 
caret::confusionMatrix(bagginglol_predict, train$blueWins)
caret::confusionMatrix(bagginglol_predicttest, test$blueWins)
bagginglol_predict = as.factor(ifelse(bagginglol_predict> 0.7,1,0))
bagginglol_predicttest = as.factor(ifelse(bagginglol_predicttest> 0.7,1,0))
train$blueWins = as.factor(train$blueWins)
test$blueWins = as.factor(test$blueWins)

prediction_bagg = prediction(as.numeric(bagginglol_predicttest), as.numeric(test$blueWins))
BAGG_ROC = performance(prediction_bagg, "tpr", "fpr")
predictiontrain_bagg = prediction(as.numeric(bagginglol_predicttrain), as.numeric(train$blueWins))
BAGG_ROC_train = performance(predictiontrain_bagg, "tpr", "fpr")
plot(BAGG_ROC) + title("Test")
plot(BAGG_ROC_train) + title("Train")
plot(BAGG_ROC, legend = c("GBM"), col = c("green"), lty =1:2, cex = 0.6)
auc.tmp = performance(prediction_bagg, "auc")
gmb_auc_test = as.numeric(auc.tmp@y.values)
gmb_auc_test ##0.79


##svm 
test = test[,-18]
train$blueWins = as.factor(train$blueWins)
test$blueWins = as.factor(test$blueWins)

ggplot(data = train, aes(x = blueDeaths, y = blueKills, color = blueWins)) + geom_point()
library(e1071)
model = svm(blueWins~., data = train, kernel = "polynomial")
plot(model, data = train, formula = blueWins~.) ##does not working



pred = predict(model, train)
table = table(Prediction = pred, Actual = train$blueWins)
1-sum(diag(table))/sum(table) 
table

##tuning
tune.out = tune(svm ,blueWins~.,data=train ,kernel ="polynomial", 
              ranges =list(epsilon = seq(0,1,0.1), cost = 2^(2:5)))
summary(tune.out)
bestmodel_svm = tune.out$best.model
plot(tune.out)
win_pred = predict(model, test)
win_predTrain = predict(model, train)
testtable = table(predicted = win_pred, Actual = test$blueWins)
traintable = table(predicted = win_predTrain, Acutal = train$blueWins)
1-sum(diag(testtable))/sum(testtable) 
1-sum(diag(traintable))/sum(traintable)
traintable

caret::confusionMatrix(win_pred, test$blueWins)
caret::confusionMatrix(win_predTrain, train$blueWins)

##roc curve
prediction_svm = prediction(as.numeric(win_pred), as.numeric(test$blueWins))
SVM_ROCtest = performance(prediction_svm, "tpr", "fpr")
predictiontrain_svm = prediction(as.numeric(win_predTrain), as.numeric(train$blueWins))
SVM_ROC_train = performance(predictiontrain_svm, "tpr", "fpr")
plot(SVM_ROCtest) + title("Test")
plot(SVM_ROC_train) + title("Train")
plot(BAGG_ROC, legend = c("GBM"), col = c("green"), lty =1:2, cex = 0.6)
auc.tmp_SVM = performance(prediction_svm, "auc")
SVM_auc_test = as.numeric(auc.tmp_SVM@y.values)
SVM_auc_test 
auc.tmp_SVM = performance(predictiontrain_svm, "auc")
SVM_auc_test = as.numeric(auc.tmp_SVM@y.values)
SVM_auc_test 
