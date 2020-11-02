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
dataset <- all_dataset %>% dplyr::select(blueWins, blueWardsPlaced, blueWardsDestroyed, blueFirstBlood, blueKills, blueDeaths, blueAssists, blueDragons, blueHeralds, blueTowersDestroyed, blueTotalMinionsKilled, 
                                  blueTotalJungleMinionsKilled, redTotalJungleMinionsKilled, blueTotalExperience, blueTotalGold, redWardsPlaced, redWardsDestroyed, redDragons, redHeralds, redTowersDestroyed, redTotalMinionsKilled)
#Create a TotalJungleMinionsDiff variable 
dataset$TotalJungleMinionsDiff <- dataset$blueTotalJungleMinionsKilled - dataset$redTotalJungleMinionsKilled
dataset <- dataset %>% dplyr::select(-blueTotalJungleMinionsKilled, -redTotalJungleMinionsKilled)

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


set.seed(10)
data_split <- caTools::sample.split(dataset$blueWins, 0.75) 

train <- subset(dataset ,data_split==T)
test <- subset(dataset,data_split==F)
##control the frequency of resposne variable
train %>% count(blueWins) %>% mutate( prop = n/sum(n)) 
test %>% count(blueWins) %>%  mutate( prop = n/sum(n)) 
##standartization for numeric variables
nonstartd_train <- train

train$blueKills = scale(train$blueKills, center = T, scale = T)[,1]
train$blueWardsPlaced = scale(train$blueWardsPlaced, center = T, scale = T)[,1]
train$blueWardsDestroyed = scale(train$blueWardsDestroyed, center = T, scale = T)[,1]
train$blueDeaths = scale(train$blueDeaths, center = T, scale = T)[,1]
train$blueAssists = scale(train$blueAssists, center = T, scale = T)[,1]
train$blueTotalMinionsKilled = scale(train$blueTotalMinionsKilled, center = T, scale = T)[,1]
train$blueTotalExperience = scale(train$blueTotalExperience, center = T, scale = T)[,1]
train$blueTotalGold = scale(train$blueTotalGold, center = T, scale = T)[,1]
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
test$blueTotalExperience <- (test$blueTotalExperience - mean(nonstartd_train$blueTotalExperience)) / sd(nonstartd_train$blueTotalExperience)
test$blueTotalGold <- (test$blueTotalGold - mean(nonstartd_train$blueTotalGold)) / sd(nonstartd_train$blueTotalGold)

####model 

model = lm(blueTotalMinionsKilled~., data = train)
summary(model)
MASS::stepAIC(model)

best_model = lm(formula = blueTotalMinionsKilled ~ blueWardsPlaced + blueWardsDestroyed + 
                blueFirstBlood + blueKills + blueDeaths + blueAssists + blueDragons + 
                blueHeralds + blueTowersDestroyed + blueTotalExperience + 
                blueTotalGold + redWardsPlaced + redWardsDestroyed + redDragons + 
                redTowersDestroyed + redTotalMinionsKilled + TotalJungleMinionsDiff, data = train)


summary(best_model)

car::vif(best_model)
#blueTotalGold have higher vif value. In this case, when you kill a minion you directly earn a gold because of that, I don't want to use it. 


best_model2 = lm(formula = blueTotalMinionsKilled ~ blueWardsPlaced + blueWardsDestroyed + 
                   blueFirstBlood + blueKills + blueDeaths + blueAssists + blueDragons + 
                   blueHeralds + blueTowersDestroyed + blueTotalExperience + 
                   redWardsPlaced + redWardsDestroyed + redDragons + 
                   redTowersDestroyed + redTotalMinionsKilled + TotalJungleMinionsDiff, data = train)
car::vif(best_model2)
summary(best_model2)
best_model3 = lm(formula = blueTotalMinionsKilled ~ blueWardsPlaced + blueWardsDestroyed + 
                   blueFirstBlood + blueKills + blueDeaths + blueAssists +  
                   blueTowersDestroyed + blueTotalExperience + 
                   redWardsDestroyed + redDragons + 
                   redTotalMinionsKilled + TotalJungleMinionsDiff, data = train)
summary(best_model3)
car::vif(best_model3)
best_model4 = lm(formula = blueTotalMinionsKilled ~ blueWardsPlaced + blueWardsDestroyed + 
                   blueKills + blueDeaths +  blueTowersDestroyed + 
                   blueTotalExperience + redWardsDestroyed + 
                   redDragons +  redTotalMinionsKilled + 
                   TotalJungleMinionsDiff, data = train)
summary(best_model4)
##then we fixed the multicollineratiy problem. 
##check out normality  for our model.
###because that number of observation higher than 5000 i used ad teest.
nortest::ad.test(best_model2$residuals)
nortest::ad.test(best_model3$residuals)
nortest::ad.test(best_model4$residuals)
power.t = MASS::boxcox(best_model2) ##response should be positive. 

train$blueTotalMinionsKilled = train$blueTotalMinionsKilled + 5.82

power.t2 = MASS::boxcox(best_model2) 
  
lambda <- power.t2$x # lambda values
lik <- power.t2$y # log likelihood values for SSE
bc <- cbind(lambda, lik) # combine lambda and lik
sorted_bc <- bc[order(-lik),] # values are sorted to identify the lambda value for the maximum log likelihood for obtaining minimum SSE
head(sorted_bc, n = 10)
##normality and other tests for model2
best_model2 = lm(formula = blueTotalMinionsKilled^(1.232323) ~ blueWardsPlaced + blueWardsDestroyed + 
                   blueKills + blueDeaths + blueHeralds + blueTowersDestroyed + 
                   blueTotalExperience + redWardsPlaced + redWardsDestroyed + 
                   redDragons + redTowersDestroyed + redTotalMinionsKilled + 
                   TotalJungleMinionsDiff, data = train)
nortest::ad.test(best_model2$residuals)#fix to normality problem
lmtest::bptest(best_model2)
car::durbinWatsonTest(best_model2)
##normality and other test for model3
power.t3 = MASS::boxcox(best_model3)
lambda <- power.t3$x # lambda values
lik <- power.t3$y # log likelihood values for SSE
bc3 <- cbind(lambda, lik) # combine lambda and lik
sorted_bc3 <- bc3[order(-lik),] # values are sorted to identify the lambda value for the maximum log likelihood for obtaining minimum SSE
head(sorted_bc3, n = 10)
best_model3 = lm(formula = blueTotalMinionsKilled^(1.434343) ~ blueWardsPlaced + blueWardsDestroyed + 
                   blueFirstBlood + blueKills + blueDeaths + blueAssists +  
                   blueTowersDestroyed + blueTotalExperience + 
                   redWardsDestroyed + redDragons + 
                   redTotalMinionsKilled + TotalJungleMinionsDiff, data = train)
summary(best_model3)
car::vif(best_model3)
nortest::ad.test(best_model3$residuals)
car::durbinWatsonTest(best_model3) ##autocorrelation
lmtest::bptest(best_model3) ##hc problem

robusted_model = robustbase::lmrob(formula = blueTotalMinionsKilled^(1.434343) ~ blueWardsPlaced + blueWardsDestroyed + 
                    blueFirstBlood + blueKills + blueDeaths + blueAssists +  
                    blueTowersDestroyed + 
                    redWardsDestroyed + redDragons + 
                    redTotalMinionsKilled + TotalJungleMinionsDiff, data = train)
summary(robusted_model)
lmtest::bptest(robusted_model)##do not solve hc problem.


##solving hc 
###boostrap
#boosted_model = mboost::glmboost(blueTotalMinionsKilled^(1.434343) ~ blueWardsPlaced + blueWardsDestroyed + 
                # blueFirstBlood + blueKills + blueDeaths + blueAssists +  
                 #blueTowersDestroyed + blueTotalExperience + 
                 #redWardsDestroyed + redDragons + 
                 #redTotalMinionsKilled + TotalJungleMinionsDiff, data = train)



#resamples = 100
#boost_train = train %>% modelr::bootstrap(resamples)
#boost_train


#boot_linear_reg <- boost_train %>% 
  #mutate(regressions = 
           #map(strap, 
               #~lm(formula = blueTotalMinionsKilled^(1.434343) ~ blueWardsPlaced + blueWardsDestroyed + 
                     #blueFirstBlood + blueKills + blueDeaths + blueAssists +  
                     #blueTowersDestroyed + blueTotalExperience + 
                     #redWardsDestroyed + redDragons + 
                     #redTotalMinionsKilled + TotalJungleMinionsDiff, data = train))) 
#tidied <- boot_linear_reg %>% 
  #mutate(tidy_lm = 
           #map(regressions, broom::tidy))
#tidied$tidy_lm[[1]]
#tidied$regressions[[1]]
#list_mods <- tidied %>%  ##list of tidy_lm data frames 
  #pull(tidy_lm)
#mods_df <- map2_df(list_mods, 
                   #seq(1, resamples), 
                   #~mutate(.x, resample = .y))
#head(mods_df, 25)
#r.std.error <- mods_df %>% 
  #group_by(term) %>% 
  #summarise(r.std.error = sd(estimate))
#boosted = best_model3 %>% 
  #broom::tidy() %>% 
  #full_join(r.std.error) %>% 
  #select(term, estimate, std.error, r.std.error)
#lmtest::bptest(best_model3)
#summary(best_model3)

##tests
lm_linearity <- data.frame(residual = best_model3$residuals, fitted = best_model3$fitted.values)

ggplot(lm_linearity, aes(fitted, residual)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Linearity")



num_predictrain = predict(best_model3, train)
num_predictest = predict(best_model3, test, type = "response")

caret::postResample(num_predictrain, train$blueTotalMinionsKilled)
caret::postResample(num_predictest, test$blueTotalMinionsKilled)
ggplot() + geom_point(aes(x = train$blueTotalMinionsKilled, y = num_predictrain)) +
  geom_abline(color = "red") + xlab("Actual") + ylab("Predicted")

##decision treee
tree_numeric = tree::tree(blueTotalMinionsKilled~., train)
summary(tree_numeric)
par(mfrow = c(1,1))
plot(tree_numeric)
text(tree_numeric)

cv_numeric = tree::cv.tree(tree_numeric)
plot(cv_numeric)
##no prune

##predict values for train and test
#train 
predicted_train = predict(tree_numeric, newdata = train)
table(predicted_train)
library(ggplot2)
ggplot() + geom_point(aes(x = train$blueTotalMinionsKilled, y = predicted_train)) +
  geom_abline(color = "red") + xlab("Actual") + ylab("Predicted")

##test
predicted_test = predict(tree_numeric, newdata = test)
table(predicted_test)
caret::postResample(predicted_train, train$blueTotalMinionsKilled)
caret::postResample(predicted_test, test$blueTotalMinionsKilled)
###very low rsquared values 


###randomForest
library(randomForest)
rf_numeric = randomForest(blueTotalMinionsKilled~.,
                      data = train,
                      mtry = 18,
                      importance = T)
rf_numeric

features <- setdiff(names(train), "blueTotalMinionsKilled")

tuned = tuneRF(train[features],
               train$blueTotalMinionsKilled,
               stepFactor = 0.5,
               plot = TRUE,
               ntreeTry = 1000,
               trace = TRUE,
               improve = 0.05)
#mtry == 12 

rf_numeric2 = randomForest(blueTotalMinionsKilled~.,
                          data = train,
                          mtry = 12,
                          ntree = 1000,
                          importance = T)
rf_numeric_estimate = predict(rf_numeric2, newdata = train)
rf_numeric_estimatetest = predict(rf_numeric2, newdata = test)

plot(rf_numeric2)
varImpPlot(rf_numeric2)

ggplot() + geom_point(aes(x = train$blueTotalMinionsKilled, y = rf_numeric_estimate)) +
  geom_abline(color = "red") + xlab("Actual") + ylab("Predicted")

caret::postResample(rf_numeric_estimate, train$blueTotalMinionsKilled)
caret::postResample(rf_numeric_estimatetest, test$blueTotalMinionsKilled)

##boosting
library(gbm)

numeric_boost = gbm(blueTotalMinionsKilled~., 
                data = train,
                distribution = "gaussian", 
                n.trees = 5000, 
                interaction.depth = 4,
                shrinkage = 0.01)
numeric_boost
summary(numeric_boost)

lolBoost <- tibble::as_tibble(gbm::summary.gbm(numeric_boost, 
                                                  plotit = FALSE))
lolBoost %>% utils::head() 

ggplot(lolBoost, aes(x = reorder(var, rel.inf), y = rel.inf)) + geom_col(aes(fill = rel.inf)) + coord_flip() + 
  ggtitle("Variables and Relative Influence - Model 1") + theme_minimal() + xlab("Features") + ylab("Relative Influence")
###finding best boosting model for my project
cvperformance = gbm.perf(numeric_boost, method = "OOB")
print(cvperformance)
numeric_boost2 = gbm(blueTotalMinionsKilled~., 
                data = train,
                distribution = "gaussian", 
                n.trees = 1741,
                interaction.depth = 4,
                shrinkage = 0.01)
numeric_boost2
summary(numeric_boost2)
lolBoost2 <- tibble::as_tibble(gbm::summary.gbm(numeric_boost2, 
                                               plotit = FALSE))
lolBoost2 %>% utils::head() 

ggplot(lolBoost2, aes(x = reorder(var, rel.inf), y = rel.inf)) + geom_col(aes(fill = rel.inf)) + coord_flip() + 
  ggtitle("Variables and Relative Influence - Model 2") + theme_minimal() + xlab("Features") + ylab("Relative Influence")

boost_estimate = predict(numeric_boost2, newdata = train, n.trees = 1642, interaction.depth = 4, distribution = "gaussian")
boost_estimatetest = predict(numeric_boost2, newdata = test, n.trees = 1642, interaction.depth = 4, distribution = "gaussian")

ggplot() + geom_point(aes(x = train$blueTotalMinionsKilled, y = boost_estimate)) +
  geom_abline(color = "red") + xlab("Actual") + ylab("Predicted")

caret::postResample(boost_estimate, train$blueTotalMinionsKilled)
caret::postResample(boost_estimatetest, test$blueTotalMinionsKilled)


##BAGGING
library(ipred)
bagg_numeric <- bagging(formula = blueTotalMinionsKilled~., 
                    data = train,
                    nbagg = 100,
                    coob = TRUE)
bagg_numeric

bagginglol_predict <- predict(bagg_numeric, 
                              newdata = train)
bagginglol_predicttest = predict(bagg_numeric, newdata = test)
ggplot() + geom_point(aes(x = train$blueTotalMinionsKilled, y = bagginglol_predict)) +
  geom_abline(color = "red") + xlab("Actual") + ylab("Predicted")

# Performance Measurement 
caret::postResample(bagginglol_predict, train$blueTotalMinionsKilled)
caret::postResample(bagginglol_predicttest, test$blueTotalMinionsKilled)

mean((bagginglol_predict - train$blueTotalMinionsKilled)^2)
Metrics::rmse(actual=train$blueTotalMinionsKilled, predicted = bagginglol_predict)

bagg_numeric

