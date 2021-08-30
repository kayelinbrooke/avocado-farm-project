# Final Project 

#clear environment
remove(list = ls())

#load packages
library(ggplot2)

#read in data
avocados <- read.csv("/Users/kksizzle/Desktop/MGSC 310/final project/avocado.csv")

sum(is.na(avocados))  #check for NAs

names(avocados)[5:7] <- c("Small.Size","Medium.Size","Large.Size")  #rename columns

avocados <- avocados[,-1]  #get rid of first column

avocados$year <- factor(avocados$year) # factor the year variable

summary(avocados)  #summary statistics 



### linear regression model
lm_mod <- lm(AveragePrice ~ . - Date, data = avocados)
summary(lm_mod)

### Plots for region and AveragePrice, Total.Bags, Total.Volume
TotalUS_regionsubset <- subset(avocados, region == "TotalUS")

ggplot(TotalUS_regionsubset, aes(x = Date, y = AveragePrice))+
  geom_point(aes(color = TotalUS_regionsubset$type))+
  theme(legend.position = "bottom")

ggplot(TotalUS_regionsubset, aes(x = Date, y = Total.Volume))+
  geom_point(aes(color = TotalUS_regionsubset$type))+
  theme(legend.position = "bottom")

ggplot(TotalUS_regionsubset, aes(x = Date, y = Total.Bags))+
  geom_point(aes(color = TotalUS_regionsubset$type))+
  theme(legend.position = "bottom")

### Creating a glm model and plotting a logarithmic function based on predictions
mod_glm <- glm(type ~ AveragePrice,
               data = TotalUS_regionsubset,
               family = binomial)

scores_mod1 <- predict(mod_glm, type = "response")

ggplot(mod_glm, aes(AveragePrice, type, color=type))+
  geom_point()+
  geom_line(aes(,scores_mod1+1))


### Scatter Plots comparing Average Price and Bag Sizes

# plot for small bags
Small <- ggplot(avocados,aes(x = AveragePrice, y = Small.Bags))+
  geom_point(color="lightblue")

# plot for large bags
Large <- ggplot(avocados,aes(x = AveragePrice, y = Large.Bags))+
  geom_point(color="pink")

# plot for Xlarge bags
XL <- ggplot(avocados,aes(x = AveragePrice, y = XLarge.Bags))+
  geom_point(color="#E69F00")

# plots 3 scatterplots at same time
# install.packages("cowplot")
library(cowplot)
plot_grid(Small, Large, XL)

#####################

###  Scatter Plot comparing Average price and Type

ggplot(avocados, aes(AveragePrice, type, color=type)) +
  stat_smooth(method="glm", family=binomial) +
  geom_point(position=position_jitter(height=1, width=1)) +
  xlab("Average Price") + ylab("Type of Avocado")


#Christian modified version for ggplots: (added factors but kept the main colors for the plots)
### Scatter Plots comparing Average Price and Bag Sizes

# plot for small bags
Small <- ggplot(avocados,aes(x = AveragePrice, y = Small.Bags))+
  geom_point(aes(color=avocados$type), size = 0.9)+
  scale_color_manual(values = c("lightblue", "blue"))+
  theme(legend.position = "bottom")

# plot for large bags
Large <- ggplot(avocados,aes(x = AveragePrice, y = Large.Bags))+
  geom_point(aes(color=avocados$type), size = 0.9)+
  scale_color_manual(values = c("pink", "red"))+
  theme(legend.position = "bottom")

# plot for Xlarge bags
XL <- ggplot(avocados,aes(x = AveragePrice, y = XLarge.Bags))+
  geom_point(aes(color=avocados$type), size = 0.9)+
  scale_color_manual(values = c("#E69F00", "black"))+
  theme(legend.position = "bottom")

# plots 3 scatterplots at same time
# install.packages("cowplot")
library(cowplot)
plot_grid(Small, Large, XL)


# Preface:
## We began our project by creating a subset from the data using the TotalUS data located in the region comumn. This helps us focus on the broad trends and aggregate data, rather than trying to sort through data from all 54 regions, which would be messy and time consuming. Eventually we can analyze individual regions and compare that data against this "benchmark."
#
# Motive for the following plots: 

## Variables: Date, AveragePrice, Total.Bags, Total Volume, Bag size (Small.Bags, Large.Bags, XLarge.bags), and Organic vs. Conventional. We chose these variables because we believed they would have the largest impact on the Average price for the TotalUS subset, after our analysis of the summary coefficients.  

### Date with AveragePrice: Find the average selling price throughout the year. How does it change?
### Date with Total.Volume and Total.Bags: What time of year do avocados sell more? Is it cyclical?
### Compare the plots for the variables above the see when the most avocados sell and at what price.
### Bag Size and Average Price: Find the average price for the different bag sizes. 

## We are also interested in exploring the impact of organic avocados on the average price, number of sales, etc. We will use the "type" variable (which tells us if the avocados are organic or conventional), as a primary color factor in the plots, so that we could easily determine the impact on the other variables listed above.

###########################################

# import data
avocados <- read.csv("/Users/kksizzle/Desktop/MGSC 310/final project/avocado.csv")

# basic data cleaning / organizing
names(avocados)[5:7] <- c("Small.Size","Medium.Size","Large.Size")  #rename columns
avocados <- avocados[,-1]  #get rid of first column
avocados$year <- factor(avocados$year) # factor the year variable

# test / train data
set.seed(310)
index <- sample(1:nrow(avocados),size=0.75*nrow(avocados),replace=FALSE)
train <- avocados[index,]
test <- avocados[-index,]

# linear regress (Average Price by Region)
# train.control <- trainControl(method = "cv", number = 10)
# mod1_lm_train <- train(AveragePrice ~ region, method = "lm",
#                    data = train, trControl = train.control)
# 
# mod1_lm_test <- train(AveragePrice ~ region, method = "lm",
#                      data = test, trControl = train.control)
print(mod1_lm_train)

mod1_lm_train <- lm(AveragePrice ~ region,
                       data = train)
mod1_lm_test <- lm(AveragePrice ~ region,
                      data = test)

# Predictions
#mod1 
preds_train1 <- predict(mod1_lm_train)

preds_train_df1 <- data.frame(true = train$AveragePrice, pred = preds_train1, resid = mod1_lm_train$residuals)

preds_test1 <-  predict(mod1_lm_train, newdata = test)

preds_test_df1 <- data.frame(true = test$AveragePrice, pred = preds_test1)

# Model accuracy 
# MSE for train and test
library(caret)
RMSE(preds_train_df1$pred, preds_train_df1$true) #0.365
RMSE(preds_test_df1$pred, preds_test_df1$true) #0.374

#install.packages("rsq")
library(rsq)
rsq(mod1_lm_train)

#there is not much of an overfitting issue since there is no big difference between the RSMEs

###########################################

# heteroskedasticity
library(ggplot2)

ggplot(preds_train_df, aes(pred, resid)) +
  geom_point() +
  geom_smooth(method='lm')

# revenues = avg price * total volume
avocados$revenue <- avocados$AveragePrice * avocados$Total.Volume

set.seed(310)
index <- sample(1:nrow(avocados),size=0.75*nrow(avocados),replace=FALSE)
train <- avocados[index,]
test <- avocados[-index,]

###########################################

# Model 3 - Kmeans
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
dim(avocados)
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
# #scaling continuous variables
# avocados_subset_scale <- scale(avocados_subset[,-c(11:13)])
# avo.11.13 <- avocados_subset[,c(11:13)]
# 
# #final subset
# avocados_subset <- merge(data.frame(avocados_subset_scale, row.names=NULL), data.frame(avo.11.13, row.names=NULL), by = 0, all = TRUE)[-1]

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

ggplot(FinalDF, aes(cluster_name, fill = type)) + 
  geom_bar() +
  labs(x = "Cluster", y = "Count", fill = "Type of Avocado") +
  scale_fill_manual(values = mycolor)

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

##--------- Graphs by Region-----------##
# avg price vs volume by region
ggplot(FinalDF, aes(AveragePrice,Total.Volume, color = cluster_name)) +
  geom_point() + 
  facet_wrap(~region_simple) +
  scale_color_manual(values = mycolor) + 
  labs(x = "Average Price", y = "Total Volume", title = "Average Price vs Total Volume by Region", color = "Cluster") +
  scale_y_continuous(labels = ks)

#avg price vs revenue by region
ggplot(FinalDF, aes(AveragePrice, revenue, color = cluster_name)) +
  geom_point() + 
  facet_wrap(~region_simple) +
  scale_color_manual(values = mycolor) + 
  labs(x = "Average Price", y = "Revenue", title = "Average Price vs Revenue by Region", color = "Cluster") +
  scale_y_continuous(labels = ks)

# revenue vs Volume by region
ggplot(FinalDF, aes(revenue,Total.Volume, color = cluster_name)) +
  geom_point() + 
  facet_wrap(~region_simple) +
  scale_color_manual(values = mycolor) + 
  labs(x = "Revenue", y = "Total Volume", title = "Revenue vs Total Volume by Region", color = "Cluster") +
  scale_x_continuous(labels = ks) +
  scale_y_continuous(labels = ks)

##--------- Graphs by Type-----------##
# clusters by type
ggplot(FinalDF, aes(cluster_name, fill=type)) + 
  geom_bar() +
  scale_fill_manual(values = mycolor) +
  labs(x = "Cluster", y = "Count", title = "Clusters by Type of Avocado", fill = "Type of Avocado")
  
# avg price vs volume by type
ggplot(FinalDF, aes(AveragePrice,Total.Volume, color = cluster_name)) +
  geom_point() + 
  facet_wrap(~type) +
  scale_color_manual(values = mycolor) + 
  labs(x = "Average Price", y = "Total Volume", title = "Average Price vs Total Volume by Type", color = "Cluster") +
  scale_y_continuous(labels = ks)

# avg price vs volume by type and region 
ggplot(FinalDF, aes(AveragePrice,Total.Volume, color = cluster_name)) +
  geom_point() + 
  facet_wrap(~type+region_simple) +
  scale_color_manual(values = mycolor) + 
  labs(x = "Average Price", y = "Total Volume", title = "Average Price vs Total Volume by Type and Region", color = "Cluster") +
  scale_y_continuous(labels = ks)

#avg price vs revenue by type
ggplot(FinalDF, aes(AveragePrice, revenue, color = cluster_name)) +
  geom_point() + 
  facet_wrap(~type) +
  scale_color_manual(values = mycolor) + 
  labs(x = "Average Price", y = "Revenue", title = "Average Price vs Total Revenue by Type", color = "Cluster") +
  scale_y_continuous(labels = ks)

# revenue vs volume by type
ggplot(FinalDF, aes(Total.Volume, revenue, color = cluster_name)) +
  geom_point() + 
  facet_wrap(~type) +
  scale_color_manual(values = mycolor) + 
  labs(y = "Revenue", x = "Total Volume", title = "Revenue vs Total Volume by Type", color = "Cluster") +
  scale_x_continuous(labels = ks) +
  scale_y_continuous(labels = ks)

##--------- Graphs by Quarter-----------##

# avg price vs revenue by quarter
ggplot(FinalDF, aes(AveragePrice, revenue, color = cluster_name)) +
  geom_point() +
  facet_wrap(~avocados_Q) +
  scale_color_manual(values = mycolor) + 
  labs(x = "Average Price", y = "Revenue", title = "Average Price vs Revenue by Quarter", color = "Cluster") +
  scale_y_continuous(labels = ks)

# avg price vs volume by quarter and type
ggplot(FinalDF, aes(AveragePrice, Total.Volume, color = cluster_name)) +
  geom_point() +
  facet_wrap(~avocados_Q+type) +
  scale_color_manual(values = mycolor) + 
  labs(x = "Average Price", y = "Total Volume", title = "Average Price vs Total Volume by Quarter and Type", color = "Cluster") +
  scale_y_continuous(labels = ks)

# revenue vs volume by quarter and type
ggplot(FinalDF, aes(Total.Volume, revenue, color = cluster_name)) +
  geom_point() +
  facet_wrap(~avocados_Q+type) +
  scale_color_manual(values = mycolor) + 
  labs(x = "Total Volume", y = "Revenue", title = "Total Volume vs Revenue by Quarter and Type", color = "Cluster") +
  scale_x_continuous(labels = ks) +
  scale_y_continuous(labels = ks)


# avg price vs total volume by quarter and type
ggplot(FinalDF, aes(AveragePrice, Total.Volume, color = cluster_name)) +
  geom_point() +
  facet_wrap(~avocados_Q+type) +
  scale_color_manual(values = mycolor) + 
  labs(x = "Average Price", y = "Total Volume", title = "Average Price vs Total Volume by Quarter and Type", color = "Cluster") +
  scale_y_continuous(labels = ks)


##--------- Evaluating the model-----------##

fviz_nbclust(avocados_subset[,-c(11:13)],
             kmeans,
             method="silhouette")

###--------- Linear Regression ---------###
# import data
avocados <- read.csv("/Users/kksizzle/Desktop/MGSC 310/final project/avocado.csv")

# basic data cleaning / organizing
names(avocados)[5:7] <- c("Small.Size","Medium.Size","Large.Size")  #rename columns
avocados <- avocados[,-1]  #get rid of first column

# test / train data
set.seed(310)
index <- sample(1:nrow(avocados),size=0.75*nrow(avocados),replace=FALSE)
train <- avocados[index,]
test <- avocados[-index,]

mod1_lm_train <- lm(AveragePrice ~ . -Date,
                    data = train)
# Predictions
preds_train1 <- predict(mod1_lm_train)
preds_train_df1 <- data.frame(true = train$AveragePrice, pred = preds_train1, resid = mod1_lm_train$residuals)

preds_test1 <-  predict(mod1_lm_train, newdata = test)
preds_test_df1 <- data.frame(true = test$AveragePrice, pred = preds_test1)

# Model accuracy 
# MSE for train and test
library(caret)
RMSE(preds_train_df1$pred, preds_train_df1$true) #0.266
RMSE(preds_test_df1$pred, preds_test_df1$true) #0.274
#there is not much of an overfitting issue since there is no big difference between the RSMEs

# R2
library(rsq)
rsq(mod1_lm_train) #0.56
#R2 are very low

ggplot(preds_train_df1, aes(pred, resid)) +
  geom_point() +
  geom_smooth()
#signs of heteroskedasticity, contributes to the reason why R2 is so low

# collinearity
library(olsrr)
ols_vif_tol(mod1_lm_train)
#VIF > 10 indicates problematic level of multicollinearity
#There seems to be no collinearity issue

###--------- Lasso Regression ---------###
library(glmnet)
library(glmnetUtils)
library(caret)

# import data
avocados <- read.csv("/Users/kksizzle/Desktop/MGSC 310/final project/avocado.csv")

# basic data cleaning / organizing
names(avocados)[5:7] <- c("Small.Size","Medium.Size","Large.Size")  #rename columns
avocados <- avocados[,-1]  #get rid of first column

# test / train data
set.seed(310)
index <- sample(1:nrow(avocados),size=0.75*nrow(avocados),replace=FALSE)
train <- avocados[index,]
test <- avocados[-index,]

# CV Lasso Model
set.seed(310)
lasso_mod <- cv.glmnet(AveragePrice~.-Date, alpha = 1,data = train)

#plot
plot(lasso_mod) #training MSE as a function of lambda

#lambdas
lasso_mod_cv$lambda.min
lasso_mod_cv$lambda.1se

# put coef in a matrix
coef(lasso_mod_cv, s = lasso_mod_cv$lambda.min)
coef(lasso_mod_cv, s = lasso_mod_cv$lambda.1se)

#best lambda
best_lam<- lasso_mod_cv$lambda.min

#Predictions using best lambda
#train
lasso_pred_train <- predict(lasso_mod_cv, s = best_lam, newdata = train)

#test
lasso_pred_test <- predict(lasso_mod_cv, s = best_lam, newdata = test)

# Model accuracy 
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






