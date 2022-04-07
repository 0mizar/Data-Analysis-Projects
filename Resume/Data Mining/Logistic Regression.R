weather <- read.csv("weatherAUS.csv", stringsAsFactors = TRUE)
weather$Date <- as.Date(weather$Date , "%m/%d/%Y")

library(lubridate)
weather$Month <- as.factor(month(weather$Date))
summary(weather)

####### summary statistics for numeric variables #####
str(weather)
summary(Filter(is.numeric, weather))

# correlation matrix
library(gplots)
colfunc <- colorRampPalette(c("green", "white", "red"))
heatmap.2(cor(Filter(is.numeric, weather), use = "complete.obs"), Rowv = FALSE, Colv = FALSE,
          dendrogram = "none", lwid=c(0.1,4), lhei=c(0.1,4), col = colfunc(15),
          cellnote = round(cor(Filter(is.numeric, weather), use = "complete.obs"),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

#### Two-Way Contingency Table
# The null hypothesis is that X and Y are independent (e.g. there is no relationship between RainToday and RainTomorrow).
table.weather <- table(weather$RainToday, weather$RainTomorrow)
table.weather

# Chi-Square Test
chisq.test(table.weather)
# p-value is less than 0.05. 
# reject the null hypothesis and conclude that there IS a relationship between RainToday and RainTomorrow 
# variables are not independent !!! dependent


####### handle missing numeric data #####
numcols <- colnames(Filter(is.numeric, weather))
for (i in numcols) {
  ifelse(sum(is.na(weather[[i]]))/nrow(weather) > 0.01,
         weather[[i]][is.na(weather[[i]])] <- median(weather[[i]], na.rm = TRUE),
         weather <- weather[!is.na(weather[[i]]), ])}

####### handling missing categorical data ######    # delete records with blank data
summary(Filter(is.factor, weather))
weather$RainToday <- droplevels(weather$RainToday)
summary(weather$RainToday)

##### new data set w no multicollinearity #####
### remove Temp9am,Temp3pm, Pressure9am predictors
weather2 = subset(weather, select = -c(Temp9am,Temp3pm, Pressure9am))

# new correlation matrix
library(gplots)
colfunc <- colorRampPalette(c("green", "white", "red"))
heatmap.2(cor(Filter(is.numeric, weather2), use = "complete.obs"), Rowv = FALSE, Colv = FALSE,
          dendrogram = "none", lwid=c(0.1,4), lhei=c(0.1,4), col = colfunc(15),
          cellnote = round(cor(Filter(is.numeric, weather2), use = "complete.obs"),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

####### create dummy variables for Month ######
library(fastDummies)
summary(weather$Month)
levels(weather$Month)
weather.eda <- dummy_cols(weather2,
                          select_columns = c("Location", "WindGustDir", "WindDir9am", "WindDir3pm", "RainToday", "Month"), 
                          remove_first_dummy = TRUE,
                          remove_selected_columns = TRUE)
t(t(names(weather.eda)))
summary(weather.eda$RainToday_Yes)

###### exclude Location from data ######
t(t(names(weather.eda)))
weather.eda2 <- weather.eda[-c(19:46)]
t(t(names(weather.eda2)))

#partition the data#
set.seed(1)
train.weather <- sample(rownames(weather.eda2), nrow(weather.eda2) * 0.15)
train.df <- weather.eda2[train.weather, ]
valid.weather <- setdiff(rownames(weather.eda2), train.weather)
valid.df <- weather.eda2[valid.weather, ]

##### Logistic Regression #####
# Converting RainTomorrow column values from `No`, `Yes` to `0` and `1`.
train.df$RainTomorrow<-ifelse(train.df$RainTomorrow=="Yes",1,0)
valid.df$RainTomorrow<-ifelse(valid.df$RainTomorrow=="Yes",1,0)

# Using `glm()` and `binomial`to build model on `train_set`
log_model_full <- glm(RainTomorrow ~ . , family = "binomial", data = train.df)
log_model_full
# Obtain significance levels using summary()
summary(log_model_full) # 41 predictors that are not statistical significant at 95% confidence

#predict valid based on the `log_model_full`
log_pred_full <- predict(log_model_full, newdata = valid.df, type = "response")
summary(log_pred_full)
data.frame(actual = valid.df$RainTomorrow[1:20],
           predicted = log_pred_full[1:20])

# Construct a confusion matrix
library(caret)
confusionMatrix(as.factor(ifelse(log_pred_full >= 0.5, "1", "0")),
                as.factor(valid.df$RainTomorrow),
                positive ="1")

# Construct lift chart and decile lift chart
library(gains)
pred_full_50_lift <- gains(valid.df$RainTomorrow, #actual class
                           log_pred_full,       # probability without cut-off
                           groups = nrow(valid.df)) # include 1 record per group
# plot predicted successes on y-axis and total cases on x-axis
plot(c(0,pred_full_50_lift$cume.pct.of.total * sum(valid.df$RainTomorrow)) ~ c(0,pred_full_50_lift$cume.obs),
     xlab = "# of cases", ylab = "cumulative", type ="l")
#plot line that would result from randomly guessing
lines(c(0, sum(valid.df$RainTomorrow)) ~ c(0, nrow(valid.df)), col ="gray", lty = 2)

# plot decile-wise lift chart
pred_full_50_decile <- gains(valid.df$RainTomorrow, # default of group is 10
                             log_pred_full)
barplot(height = pred_full_50_decile$mean.resp / mean(valid.df$RainTomorrow), # % of successes in group / % of successes overall
        names.arg = pred_full_50_decile$depth,
        xlab = "percentile", ylab = "Mean Respone", main = "Decile-wise Lift Chart", ylim = c(0,3))# % of successes in group / % of successes overall) 

# Calculate the odds for the model. And the meaning of the odds coefficients for `Humidity3pm` and `RainToday_Yes`
library(MASS)
exp(coef(log_model_full))
####  after reviewing a summary, filter out WindGustDir, and all of the WindDir9am and WindDir3pm variables with high p-values indicating that they don't contribute significantly to the prediction mode
train.df = subset(train.df, select = -c(19:63))
valid.df = subset(valid.df, select = -c(19:63))


####### logistic regression model #######
# create the full logistic regression model
RainTom.logit <- glm(RainTomorrow ~ ., data = train.df, family = "binomial")
RainTom.summary <- summary(RainTom.logit)

######### variable selection algorithms review #######
####### forward selection #######
RainTom.null <- glm(RainTomorrow ~ 1, data = train.df, family = "binomial")
RainTom.fwd <- step(RainTom.null, scope = list(RainTom.null, upper = RainTom.logit), direction = "forward")
summary(RainTom.fwd)

fwd.pred <- predict(RainTom.fwd, valid.df, type = "response")
confusionMatrix(factor(ifelse(fwd.pred >= 0.5, 1, 0)), factor(valid.df$RainTomorrow), positive = "1")
fwd.gain <- gains(valid.df$RainTomorrow, fwd.pred, groups = nrow(valid.df))
plot(c(0, fwd.gain$cume.pct.of.total * sum(valid.df$RainTomorrow)) ~ c(0, fwd.gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", type = "l")

fwd.gain <- gains(valid.df$RainTomorrow, fwd.pred)
heights <- fwd.gain$mean.resp / mean(valid.df$RainTomorrow)
dec.lift <- barplot(heights, names.arg = fwd.gain$depth, ylim = c(0, 3),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")

####### backward elimination #######
RainTom.back <- step(RainTom.logit, direction = "back")
summary(RainTom.back)

back.pred <- predict(RainTom.back, valid.df, type = "response")
confusionMatrix(factor(ifelse(back.pred >= 0.5, 1, 0)), factor(valid.df$RainTomorrow), positive = "1")
back.gain <- gains(valid.df$RainTomorrow, back.pred, groups = nrow(valid.df))
plot(c(0, back.gain$cume.pct.of.total * sum(valid.df$RainTomorrow)) ~ c(0, back.gain$cume.obs), 
     xlab = "# of cases", ylab = "Cumulative", type = "l")
lines(c(0, sum(valid.df$RainTomorrow)) ~ c(0, nrow(valid.df)), lty = 2)
back.gain <- gains(valid.df$RainTomorrow, back.pred)
heights <- back.gain$mean.resp / mean(valid.df$RainTomorrow)
dec.lift <- barplot(heights, names.arg = back.gain$depth, ylim = c(0, 3),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")

####### stepwise regression #######
RainTom.step <- step(RainTom.null, scope = list(RainTom.null, upper = RainTom.logit), direction = "both")
summary(RainTom.step)

step.pred <- predict(RainTom.step, valid.df, type = "response")
confusionMatrix(factor(ifelse(step.pred >= 0.5, 1, 0)), factor(valid.df$RainTomorrow), positive = "1")
step.gain <- gains(valid.df$RainTomorrow, step.pred, groups = nrow(valid.df))
plot(c(0, step.gain$cume.pct.of.total * sum(valid.df$RainTomorrow)) ~ c(0, step.gain$cume.obs), 
     xlab = "# of cases", ylab = "Cumulative", type = "l")
lines(c(0, sum(valid.df$RainTomorrow)) ~ c(0, nrow(valid.df)), lty = 2)
step.gain <- gains(valid.df$RainTomorrow, step.pred)
heights <- step.gain$mean.resp / mean(valid.df$RainTomorrow)
dec.lift <- barplot(heights, names.arg = step.gain$depth, ylim = c(0, 3),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")

