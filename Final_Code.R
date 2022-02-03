# MGSC 310 - Final Project 
# Kayelin, Christian, Oliver, Nina

# Data Exploration
library(cowplot)
library(ggplot2)

## Scatter Plots comparing Average Price and Bag Sizes

avocados <- read.csv("/Users/kksizzle/Desktop/MGSC 310/final project/avocado.csv")

### plot for small bags
Small <- ggplot(avocados,aes(x = AveragePrice, y = Small.Bags))+
  geom_point(aes(color=avocados$type), size = 0.9)+
  scale_color_manual(values = c("lightblue", "blue"))+
  theme(legend.position = "bottom")

### plot for large bags
Large <- ggplot(avocados,aes(x = AveragePrice, y = Large.Bags))+
  geom_point(aes(color=avocados$type), size = 0.9)+
  scale_color_manual(values = c("pink", "red"))+
  theme(legend.position = "bottom")

### plot for Xlarge bags
XL <- ggplot(avocados,aes(x = AveragePrice, y = XLarge.Bags))+
  geom_point(aes(color=avocados$type), size = 0.9)+
  scale_color_manual(values = c("#E69F00", "black"))+
  theme(legend.position = "bottom")

### plots 3 scatterplots at same time
plot_grid(Small, Large, XL)


# Linear Regression 

### import packages
library(caret)
library(rsq)
library(olsrr)


### import data
avocados <- read.csv("/Users/kksizzle/Desktop/MGSC 310/final project/avocado.csv")

### basic data cleaning / organizing
names(avocados)[5:7] <- c("Small.Size","Medium.Size","Large.Size")  #rename columns
avocados <- avocados[,-1]  #get rid of first column

### test / train data
set.seed(310)
index <- sample(1:nrow(avocados),size=0.75*nrow(avocados),replace=FALSE)
train <- avocados[index,]
test <- avocados[-index,]

### linear model
mod1_lm_train <- lm(AveragePrice ~ . -Date,
                    data = train)

### Summary
summary(mod1_lm_train)

### Predictions
#train
preds_train1 <- predict(mod1_lm_train)
preds_train_df1 <- data.frame(true = train$AveragePrice, pred = preds_train1, resid = mod1_lm_train$residuals)

#test
preds_test1 <-  predict(mod1_lm_train, newdata = test)
preds_test_df1 <- data.frame(true = test$AveragePrice, pred = preds_test1)

### Graphs
ggplot(avocados, aes(AveragePrice, Total.Volume,color = type)) + 
  geom_point()



### model accuracy: RMSE and R2
#There is not much of an overfitting issue since there is no big difference between the RSMEs.
#R2 is very low, the model didn't score too well. 

# train RMSE
RMSE(preds_train_df1$pred, preds_train_df1$true)

# test RMSE
RMSE(preds_test_df1$pred, preds_test_df1$true) 

# R2
rsq(mod1_lm_train)

### heteroskedasticity
ggplot(preds_train_df1, aes(pred, resid)) +
  geom_point(alpha = 0.3, aes(color = train$type)) +
  geom_smooth(method = "lm")
#There are signs of heteroskedasticity which may contribute to the low R2.

### collinearity
#VIF > 10 indicates problematic level of multicollinearity.
#There seems to be no collinearity issue.
ols_vif_tol(mod1_lm_train)


###--------- Lasso Regression ---------###
library(glmnet)
library(glmnetUtils)
library(caret)

# CV Lasso Model
set.seed(310)
lasso_mod_cv <- cv.glmnet(AveragePrice~.-Date, alpha = 1,data = train)

#plot
plot(lasso_mod_cv) #training MSE as a function of lambda

#lambdas
lasso_mod_cv$lambda.min 
lasso_mod_cv$lambda.1se 

# put coef in a matrix
coef(lasso_mod_cv, s = lasso_mod_cv$lambda.min) #removed 6 variables
coef(lasso_mod_cv, s = lasso_mod_cv$lambda.1se) #removed 15 variables 

#best lambda
best_lam<- lasso_mod_cv$lambda.1se

#Predictions using best lambda
#train
lasso_pred_train <- predict(lasso_mod_cv, s = best_lam, newdata = train)

#test
lasso_pred_test <- predict(lasso_mod_cv, s = best_lam, newdata = test)

# Model accuracy: RMSE and R2

# MSE train
RMSE(lasso_pred_train, train$AveragePrice) #0.266

# MSE train
RMSE(lasso_pred_test, test$AveragePrice)#0.274

# R2
residual <- test$AveragePrice - lasso_pred_test
sse <- sum((residual)^2) #sum of sqr distances bwt actual & predicted
tss <- sum((test$AveragePrice-mean(test$AveragePrice))^2) #sum of sqr distances bwt actual & their mean
r2 <- 1 - (sse/tss)
r2


###--------- K-Means Clustering ---------###
library(tidyverse)
library(forcats)
library(zoo)
library(factoextra)
library(cluster)
library(scales)
library(mondate)
library(RColorBrewer)
library(ggplot2)

# import data
avocados <- read.csv("/Users/kksizzle/Desktop/MGSC 310/final project/avocado.csv")

# basic data cleaning / organizing
names(avocados)[5:7] <- c("Small.Size","Medium.Size","Large.Size")  #rename columns
avocados <- avocados[,-1]  # drop first column
avocados <- avocados[,-12] #drop year column 

# subset data
set.seed(310)
avo_indx <-  sample(1:nrow(avocados), 0.06*nrow(avocados), replace=FALSE)
avocados_subset <- avocados[avo_indx,]

# create revenue column
avocados_subset$revenue <- avocados_subset$AveragePrice * avocados_subset$Total.Volume

# re order columns
avocados_subset <- avocados_subset[, c(2,3,6,4,9,5,7,8,13,10,11,12,1)]
dim(avocados_subset)

# perform K-means clustering with 3 clusters
kmeans3 <- kmeans(avocados_subset[,-c(11:13)],
                  centers = 3,
                  nstart = 25)

# print the average feature for each cluster
kmeans3$centers

# cluster plot
clusplot(avocados_subset[,-c(11:13)],
         kmeans3$cluster,
         color = TRUE,
         shade = FALSE)

# colors for graphs
mycolor <-  c("#1B7837","#A6DBA0","#C2A5CF") 

fviz_cluster(kmeans3, avocados_subset[,-c(11:13)], geom = "point", ellipse.type = "norm") +
  scale_color_manual(values = mycolor)+
  scale_fill_manual(values = mycolor)

#add cluster columns
FinalDF <- data.frame(avocados_subset, factor(kmeans3$cluster))
FinalDF <- transform(FinalDF, cluster_name = paste("Cluster",kmeans3$cluster))

#add quarter columns
FinalDF$avocados_Q <- as.yearqtr(FinalDF$Date, format = "%Y-%m-%d")
FinalDF$avocados_Q <- quarters(FinalDF$avocados_Q)

#lump regions
FinalDF$region_simple <- fct_lump_min(FinalDF$region, min = 26)

##--------- Graphs-----------##
options(scipen = 999)

# function to turn axis to millions
ks <- function (x) { number_format(accuracy = 1,
                                   scale = 1/1000000,
                                   suffix = "M",
                                   big.mark = ",")(x) }

# avg price vs revenue
ggplot(FinalDF, aes(AveragePrice, revenue, color = cluster_name)) +
  geom_point() +
  scale_color_manual(values = mycolor) + 
  labs(x = "Average Price", y = "Revenue", title = "Average Price vs Revenue", color = "Cluster") +
  scale_y_continuous(labels = ks)

# avg price vs volume
ggplot(FinalDF, aes(AveragePrice, Total.Volume, color = cluster_name)) +
  geom_point() +
  scale_color_manual(values = mycolor) + 
  labs(x = "Average Price", y = "Volume", title = "Average Price vs Total Volume", color = "Cluster") +
  scale_y_continuous(labels = ks)

# avg price vs volume by quarter and type
ggplot(FinalDF, aes(AveragePrice, Total.Volume, color = cluster_name)) +
  geom_point() +
  facet_wrap(~avocados_Q+type) +
  scale_color_manual(values = mycolor) + 
  labs(x = "Average Price", y = "Total Volume", title = "Average Price vs Total Volume by Quarter and Type", color = "Cluster") +
  scale_y_continuous(labels = ks)

# volume vs revenue by quarter and type
ggplot(FinalDF, aes(Total.Volume, revenue, color = cluster_name)) +
  geom_point() +
  facet_wrap(~avocados_Q+type) +
  scale_color_manual(values = mycolor) + 
  labs(x = "Total Volume", y = "Revenue", title = "Total Volume vs Revenue by Quarter and Type", color = "Cluster") +
  scale_x_continuous(labels = ks) +
  scale_y_continuous(labels = ks)

## Evaluating the model ##
fviz_nbclust(avocados_subset[,-c(11:13)],
             kmeans,
             method="silhouette")


###--------- Random Forest Model---------###

library(randomForest)
library(randomForestExplainer)

avocados_subset <- avocados_subset[!(avocados_subset$region == "West"),]
avocados_subset$region <- droplevels(avocados_subset$region, "West")

set.seed(310)
index2 <- sample(1:nrow(avocados_subset),size=0.75*nrow(avocados_subset),replace=FALSE)
mod4_rf_train <- avocados_subset[index2,]
mod4_rf_test <- avocados_subset[-index2,]


set.seed(310)
mod4_rf <- randomForest(AveragePrice ~ . - Date, 
                        data = mod4_rf_train,
                        mtry = 5,
                        ntree = 500,
                        importance = TRUE)

mod4_rf

#train
preds_rf_train <- predict(mod4_rf)
RMSE(preds_rf_train, mod4_rf_train$AveragePrice)

#test
preds_rf_test <- predict(mod4_rf, newdata = mod4_rf_test)
RMSE(preds_rf_test, mod4_rf_test$AveragePrice)

#R2 
residual2 <- mod4_rf_test$AveragePrice - preds_rf_test
sse2 <- sum((residual2)^2) #sum of sqr distances bwt actual & predicted
tss2 <- sum((mod4_rf_test$AveragePrice-mean(mod4_rf_test$AveragePrice))^2) #sum of sqr distances bwt actual & their mean
r22 <- 1 - (sse2/tss2)
r22


### Visualizing Random Forest

# importance variable chart
importance(mod4_rf)

# importance variable plot
varImpPlot(mod4_rf)

# plot min depth distribution
plot_min_depth_distribution(mod4_rf)

# plot variable two-way importance measure
plot_multi_way_importance(mod4_rf)


