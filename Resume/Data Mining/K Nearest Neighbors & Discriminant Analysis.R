####### WEATHER DATA
weather.df <- read.csv("weatherAUS.csv", stringsAsFactors = TRUE, header = TRUE)
str(weather.df)

####### extract Month from Date #######
weather.df$Date <- as.Date(weather.df$Date, format = "%m/%d/%Y")
library(lubridate)
# extract month from Date and make it a factor
weather.df$Month <- as.factor(month(weather.df$Date))

# drop Date, Location, Temp3pm, and Pressure3pm
t(t(names(weather.df)))
weather.df <- weather.df[ , -c(1:2, 17, 21)]


####### handling missing data #######
numcols <- colnames(Filter(is.numeric, weather.df))
for (i in numcols) {
  ifelse(sum(is.na(weather.df[[i]]))/nrow(weather.df) > 0.01,
         weather.df[[i]][is.na(weather.df[[i]])] <- median(weather.df[[i]], na.rm = TRUE),
         weather.df <- weather.df[!is.na(weather.df[[i]]), ])}

summary(weather.df)

# drop unused level for RainToday
summary(Filter(is.factor, weather.df))
weather.df$RainToday <- droplevels(weather.df$RainToday)
summary(weather.df$RainToday)
str(weather.df)

####### DISCRIMINANT ANALYSIS #######

####### create dummy variables ######
library(fastDummies)
weatherDA <- dummy_cols(weather.df, select_columns = c("WindGustDir", "WindDir9am", "WindDir3pm", 
                                                       "RainToday", "RainTomorrow", "Month"),
                        remove_first_dummy=TRUE,
                        remove_selected_columns = TRUE)
t(t(names(weatherDA)))

####### partition the data #######
set.seed(7)
## partitioning into training (15%) and validation (85%) 
train.rows.DA <- sample(rownames(weatherDA), nrow(weatherDA)*0.15)
train.data.DA <- weatherDA[train.rows.DA, ]
valid.rows.DA <- setdiff(rownames(weatherDA), train.rows.DA)
valid.data.DA <- weatherDA[valid.rows.DA, ]

## normalize data
# initialize normalized training and validation data to originals
weather.train.norm <- train.data.DA
weather.valid.norm <- valid.data.DA
weather.norm <- weatherDA

# normalize all predictors to a 0-1 scale
cols <- colnames(train.data.DA)
for (i in cols) {
  weather.valid.norm[[i]] <- 
    (weather.valid.norm[[i]] - min(train.data.DA[[i]])) / (max(train.data.DA[[i]]) - min(train.data.DA[[i]]))
  weather.train.norm[[i]] <- 
    (weather.train.norm[[i]] - min(train.data.DA[[i]])) / (max(train.data.DA[[i]]) - min(train.data.DA[[i]]))
  weather.norm[[i]] <- 
    (weather.norm[[i]] - min(train.data.DA[[i]])) / (max(train.data.DA[[i]]) - min(train.data.DA[[i]]))
}

summary(weather.train.norm)
summary(weather.valid.norm)
summary(weather.norm)
t(t(names(weather.train.norm)))


####### discriminant analysis of RainTomorrow_Yes as a function of all predictors #######
library(DiscriMiner)
weather.da.full <- linDA(weather.norm[, -64],
                         weather.norm$RainTomorrow_Yes,
                         validation = "learntest",
                         learn = as.numeric(train.rows.DA),
                         test = as.numeric(valid.rows.DA))
options(scipen = 999, digits = 5)

# classification function coefficients
weather.da.full$functions

# confusion matrix
library(caret)
library(gains)
confusionMatrix(weather.da.full$classification, as.factor(valid.data.DA$RainTomorrow_Yes), positive = "1")
gain <- gains(valid.data.DA$RainTomorrow_Yes,
              exp(weather.da.full$scores[, 2]) / (exp(weather.da.full$scores[, 1]) + exp(weather.da.full$scores[, 2])),
              groups = length(valid.data.DA))

# plot lift chart
plot(c(0, gain$cume.pct.of.total * sum(as.numeric(valid.data.DA$RainTomorrow_Yes))) ~ c(0, gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(as.numeric(valid.data.DA$RainTomorrow_Yes))) ~ c(0, nrow(valid.data.DA)), lty = 2)
abline(v = 10000, col = "skyblue")
abline(h = 2200, lty = 3)
abline(h = 7500, lty = 3)

# compute deciles and plot decile-wise lift chart
gain <- gains(as.numeric(valid.data.DA$RainTomorrow_Yes), 
              exp(weather.da.full$scores[, 2]) / (exp(weather.da.full$scores[, 1]) + exp(weather.da.full$scores[, 2])))
heights <- gain$mean.resp / mean(as.numeric(valid.data.DA$RainTomorrow_Yes))
dec.lift <- barplot(heights, names.arg = gain$depth, ylim = c(0, 4),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")

######## new dataset ########
new.df <- weatherDA[1, -64]

####### classify new data point #######
cols.new <- colnames(new.df)
# set all variables to 0
for (i in cols.new) {
  new.df[[i]] <- 0
}
new.df$MinTemp <- 21.8
new.df$MaxTemp <- 30.7
new.df$Evaporation <- 8
new.df$Sunshine <- 5.9
new.df$WindGustDir_WNW <- 1
new.df$WindGustSpeed <- 56
new.df$WindDir9am_N <- 1
new.df$WindDir3pm_N <- 1
new.df$WindSpeed9am <- 24
new.df$WindSpeed3pm <- 19
new.df$Humidity9am <- 71
new.df$Humidity3pm <- 63
new.df$Pressure9am <- 1008.6
new.df$Cloud9am <- 7
new.df$Cloud3pm <- 7
new.df$Temp9am <- 24.4
new.df

cols.new <- colnames(new.df)
for (i in cols.new) {
  new.df[[i]] <- (new.df[[i]] - min(train.data.DA[[i]])) / (max(train.data.DA[[i]]) - min(train.data.DA[[i]]))
}
new.df



####### K-NN CLASSIFICATION #######

####### create dummy variables ######
library(fastDummies)
weatherknn <- dummy_cols(weather.df, select_columns = c("WindGustDir", "WindDir9am", "WindDir3pm", 
                                                        "RainToday", "RainTomorrow", "Month"),
                         remove_selected_columns = TRUE)
t(t(names(weatherknn)))

####### partition the data #######
set.seed(7)
## partitioning into training (15%) and validation (85%) 
train.rows.knn <- sample(rownames(weatherknn), nrow(weatherknn)*0.15)
train.data.knn <- weatherknn[train.rows.knn, ]
valid.rows.knn <- setdiff(rownames(weatherknn), train.rows.knn)
valid.data.knn <- weatherknn[valid.rows.knn, ]

## normalize data
# initialize normalized training and validation data to originals
weather.train.norm.knn <- train.data.knn
weather.valid.norm.knn <- valid.data.knn

# normalize all predictors to a 0-1 scale
cols <- colnames(train.data.knn[, -69])
for (i in cols) {
  weather.valid.norm.knn[[i]] <- 
    (weather.valid.norm.knn[[i]] - min(train.data.knn[[i]])) / (max(train.data.knn[[i]]) - min(train.data.knn[[i]]))
  weather.train.norm.knn[[i]] <- 
    (weather.train.norm.knn[[i]] - min(train.data.knn[[i]])) / (max(train.data.knn[[i]]) - min(train.data.knn[[i]]))
}
summary(weather.train.norm.knn)
summary(weather.valid.norm.knn)
t(t(names(weather.train.norm.knn)))

####### k-NN with k=1 #######
# knn() is available in library FNN (provides a list of the nearest neighbors)
library(FNN)
weather.nn <- knn(train = weather.train.norm.knn[,-c(68:69)],        # the predictors in training data
                  test = weather.valid.norm.knn[, -c(68:69)],                      # predictors from the new data point
                  cl = weather.train.norm.knn$RainTomorrow_Yes,        # the categorical outcome variable in training
                  k = 1)                                  # the number of neighbors used to classify
library(caret)
confusionMatrix(weather.nn, as.factor(weather.valid.norm.knn$RainTomorrow_Yes), positive = "1")

####### accuracy table to compare k #######
# initialize a data frame with two columns: k and accuracy
accuracy.df <- data.frame(k = seq(1, 30, 1), accuracy = rep(0, 30))
# compute knn for different k on validation set
for (i in 1:30) {
  weather.knn.pred <-  knn(train = weather.train.norm.knn[,c(1,2,3,5:8,10,12:13,67,71:81)],
                           test = weather.valid.norm.knn[,c(1,2,3,5:8,10,12:13,67,71:81)], 
                           cl = weather.train.norm.knn$RainTomorrow_Yes,
                           k = i) 
  accuracy.df[i, 2] <- confusionMatrix(weather.knn.pred, as.factor(weather.valid.norm.knn$RainTomorrow_Yes),
                                       positive = "1")$overall[1]
}
accuracy.df
##### The best K is going to be equal 21, having accuracy at 0.92685 #####
weather.nn.21 <- knn(train = weather.train.norm.knn[,c(1,2,3,5:8,10,12:13,67,71:81)],
                     test = weather.valid.norm.knn[,c(1,2,3,5:8,10,12:13,67,71:81)], 
                     cl = weather.train.norm.knn$RainTomorrow_Yes,
                     k = 21)   
confusionMatrix(weather.nn.21, as.factor(weather.valid.norm.knn$RainTomorrow_Yes), positive = "1")








####### ABALONE DATA

####### read in abalone data ######
abalone.df <- read.csv("abalone.csv", header = TRUE, 
                       stringsAsFactors = TRUE)

dim(abalone.df[abalone.df$Height < 100,])
str(abalone.df)

####### summary statistics #######
summary(abalone.df)

####### investigating outlier for Height #######
boxplot(abalone.df$Height, main = "Distribution of Height")

####### correlation matrix for numeric variables #######
library(gplots)
colfunc <- colorRampPalette(c("green", "white", "red"))
heatmap.2(cor(Filter(is.numeric, abalone.df), use = "complete.obs"), Rowv = FALSE, Colv = FALSE,
          dendrogram = "none", lwid=c(0.1,4), lhei=c(0.1,4), col = colfunc(15),
          cellnote = round(cor(Filter(is.numeric, abalone.df), use = "complete.obs"),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

####### box plot #######
boxplot(abalone.df$Rings ~ abalone.df$Sex, 
        ylab = "Rings", xlab = "Sex")

# drop Whole and Length due to multicollinearity and the records w/ Height outliers
t(t(names(abalone.df)))
abalone.df <- abalone.df[abalone.df$Height < 100, -c(2, 5)]
summary(abalone.df)

t(t(names(abalone.df)))

####### dummy variables #######
levels(abalone.df$Sex)

library(fastDummies)
abalone.df <- dummy_cols(abalone.df,
                         select_columns = "Sex",
                         remove_selected_columns = TRUE)
t(t(names(abalone.df))) # returns 9 predictors
str(abalone.df)

####### partition the data #######
set.seed(7)
## partitioning into training (70%) and validation (30%) 
train.rows <- sample(rownames(abalone.df), nrow(abalone.df)*0.7)
train.data <- abalone.df[train.rows, ]
valid.rows <- setdiff(rownames(abalone.df), train.rows)
valid.data <- abalone.df[valid.rows, ]

## normalize data
# initialize normalized training and validation data to originals
abalone.train.norm <- train.data
abalone.valid.norm <- valid.data

# normalize all predictors to a 0-1 scale
cols <- colnames(train.data[, -6])
for (i in cols) {
  abalone.valid.norm[[i]] <- 
    (abalone.valid.norm[[i]] - min(train.data[[i]])) / (max(train.data[[i]]) - min(train.data[[i]]))
  abalone.train.norm[[i]] <- 
    (abalone.train.norm[[i]] - min(train.data[[i]])) / (max(train.data[[i]]) - min(train.data[[i]]))
}
summary(abalone.train.norm) # returns predictors on a 0-1 scale
summary(abalone.valid.norm) # returns predictors on a 0-1 scale
t(t(names(abalone.train.norm))) # returns 8 predictors w rings(age)

##### k-NN with all predictors using k=1 #####
abalone.knn.reg <- knn.reg(train = abalone.train.norm[, -6], 
                           test = abalone.valid.norm[, -6], 
                           y = abalone.train.norm$Rings, 
                           k = 1)
# compile the actual and predicted values and view the first 20 records
abalone.results <- data.frame(cbind(pred = abalone.knn.reg$pred,
                                    actual = abalone.valid.norm$Rings))
head(abalone.results, 20)

RMSE.abalone.knn.1 <- RMSE(abalone.valid.norm$Rings,
                           abalone.knn.reg$pred)
RMSE.abalone.knn.1





### creating RMSE table to find best k (optimal k is 21)

RMSE.df <- data.frame(k = seq(1,30,1), RMSE.k = rep(0,30))


for (i in 1:30) {
  knn.reg.pred <- knn.reg( train = abalone.train.norm[, -6], 
                           test = abalone.valid.norm[, -6],
                           y = abalone.train.norm$Rings,
                           k = i)
  RMSE.df[i,2] <- RMSE(knn.reg.pred$pred,
                       abalone.valid.norm$Rings)
}
RMSE.df



# Calculate RMSE for best k(k = 21)

abalone.nn.best <- knn.reg( train = abalone.train.norm[, -6], 
                            test = abalone.valid.norm[, -6],
                            y = abalone.train.norm$Rings,
                            k = 21)
RMSE(abalone.nn.best$pred ,abalone.valid.norm$Rings)


### Update Table with Stats for KNN K=1, K = 21

# K=1

accuracy(abalone.knn.reg$pred,
         abalone.valid.norm$Rings)

# K=21(Best K)

accuracy(abalone.nn.best$pred,
         abalone.valid.norm$Rings)


