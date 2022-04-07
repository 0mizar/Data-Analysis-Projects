##########Library Used######
library(tidyverse)
library(GGally)
library(gplots)
library(fastDummies)
library(usdm)
library(forecast)
library(leaps)

########## read in data #######
abalone <- read.csv("abalone.csv", stringsAsFactors = TRUE)
names(abalone)

####### summary statistics #####
summary(abalone)

####### correlation matrix for numeric variables #######
library(gplots)
colfunc <- colorRampPalette(c("red", "white", "green"))
heatmap.2(cor(Filter(is.numeric, abalone), use = "complete.obs"), Rowv = FALSE, Colv = FALSE,
          dendrogram = "none", lwid=c(0.1,4), lhei=c(0.1,4), col = colfunc(15),
          cellnote = round(cor(Filter(is.numeric, abalone), use = "complete.obs"),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

####### box plots ######
boxplot(abalone$Rings ~ abalone$Sex, 
        ylab = "Rings", xlab = "Sex")

######Outlier Removal
par(mfrow=c(3,3))
boxplot(abalone$Rings,col = "steelblue",border="black",xlab=("Rings"),ylab=("Height in mm"))
boxplot(abalone$Shell,col = "steelblue",border="black",xlab=("Shell"),ylab=("Height in mm"))
boxplot(abalone$Viscera,col = "steelblue",border="black",xlab=("Viscera"),ylab=("Height in mm"))
boxplot(abalone$Shucked,col = "steelblue",border="black",xlab=("Shucked"),ylab=("Height in mm"))
boxplot(abalone$Whole,col = "steelblue",border="black",xlab=("Whole"),ylab=("Height in mm"))
boxplot(abalone$Height,col = "steelblue",border="black",xlab=("Height"),ylab=("Height in mm"))
boxplot(abalone$Diam,col = "steelblue",border="black",xlab=("Diam"),ylab=("Height in mm"))
boxplot(abalone$Length,col = "steelblue",border="black",xlab=("Length"),ylab=("Height in mm"))
abalone <- abalone[abalone$Height != 226.0, ]
abalone <- abalone[abalone$Height != 103.0, ]
summary(abalone)

####### histograms
par(mfrow = c(2,2))
hist(abalone$Length, xlab = "Length", main = "Histogram", 
     col = "blue")
hist(abalone$Diam, xlab = "Diam", main = "Histogram", 
     col = "blue")
hist(abalone$Height, xlab = "Height", main = "Histogram", 
     col = "blue")
hist(abalone$Whole, xlab = "Whole", main = "Histogram", 
     col = "blue")
hist(abalone$Shucked, xlab = "Shucked", main = "Histogram", 
     col = "blue")
hist(abalone$Viscera, xlab = "Viscera", main = "Histogram", 
     col = "blue")
hist(abalone$Shell, xlab = "Shell", main = "Histogram", 
     col = "blue")
hist(abalone$Rings, xlab = "Rings", main = "Histogram", 
     col = "blue")

##### new data set w no multicollinearity #####
### remove Whole and Diam predictors
abalonedf2 = subset(abalone, select = -c(Whole,Diam))

##### view correlation matrix again
colfunc <- colorRampPalette(c("red", "white", "green"))
heatmap.2(cor(Filter(is.numeric, abalonedf2), use = "complete.obs"), Rowv = FALSE, Colv = FALSE,
          dendrogram = "none", lwid=c(0.1,4), lhei=c(0.1,4), col = colfunc(15),
          cellnote = round(cor(Filter(is.numeric, abalonedf2), use = "complete.obs"),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))


##### dummy variables #####
install.packages("fastDummies", dependencies = TRUE)
library(fastDummies)

dummy_cols(abalonedf2,                        # data frame w/o multicollinearity
           select_columns = "Sex",        # list of categorical variables
           remove_first_dummy = TRUE,         # drops one dummy variable
           remove_selected_columns = TRUE)    # drops original factor

# redefine data frame using this data
abalonedf <- dummy_cols(abalonedf2, 
                        select_columns = "Sex",
                        remove_first_dummy = TRUE,
                        remove_selected_columns = TRUE)

head(abalonedf)
summary(abalonedf)

####### partitioning the data #######
# use set.seed() to get the same partitions when re-running the R code
set.seed(1)

## partitioning into training (70%) and validation (30%) 
train.rows <- sample(rownames(abalonedf),       # sampling from the row IDs
                     nrow(abalonedf)*0.7)       # using 70% for training data

# collect all the columns with training row ID into training set:
train.data <- abalonedf[train.rows, ]

# assign row IDs that are not already in the training set into validation
valid.rows <- setdiff(rownames(abalonedf), train.rows)
valid.data <- abalonedf[valid.rows, ]

head(train.data)
head(valid.data)

####Linear Regression####
abalonedf.lm <- lm(Rings ~ .,            # outcome ~ predictors
                   data = train.data)
options(scipen = 999)  # use options() to ensure numbers are not displayed in scientific notation
summary(abalonedf.lm)

library(forecast)
# use predict() to make predictions on a new set
valid.lm.pred <- predict(abalonedf.lm,     # use the linear model
                         valid.data)    # to predict outcomes in the validation data

options(scipen = 999, digits = 1)
# calculate the residuals
valid.resid <- valid.data$Rings - valid.lm.pred  # define residuals as difference b/w actual and predicted
# look at the first 20 residuals
data.frame("Predicted" = valid.lm.pred[1:20], "Actual" = valid.data$Rings[1:20],
           "Residual" = valid.resid[1:20])

#  after reviewing the summary, filter out the Viscera and Sex_M with high p-value
# indicating that they don't contribute significantly to the prediction mode;
train.data = subset(train.data, select = -c(Viscera, Sex_M))
valid.data = subset(valid.data, select = -c(Viscera, Sex_M))


# use accuracy() to compute common accuracy measures
options(digits = 6)
accuracy(valid.lm.pred,                # predicted outcomes
         valid.data$Rings)             # actual outcomes

# view the distribution of residuals
library(tidyverse)
ggplot(data = as.data.frame(valid.resid), aes(y = valid.resid)) + geom_boxplot()

####Exhaustive Search####
install.packages("leaps")
library(leaps)

abalonedf.search <- regsubsets(Rings ~ .,                   # full model formula
                               data = train.data,           # training dataset
                               nbest = 1,                   # number of subsets of each size
                               nvmax = ncol(train.data),    # maximum number of variables to consider
                               method = "exhaustive")       # specify exhaustive search

search.summary <- summary(abalonedf.search)      # define summary for easy reference
search.summary$which                          # show which variables are included in each best subset

# compare the models returned by the exhaustive search
options(digits = 8)
t(t(search.summary$rsq))          # shows r-squared values
t(t(search.summary$adjr2))        # shows adjusted r-squared values
t(t(search.summary$cp))           # shows Mallow's Cp values
t(t(search.summary$bic))          # shows BIC values

# forward selection
abalonedf <- lm(Length ~ ., data = train.data)         # full model with all predictors
abalonedf.null <- lm(Length ~ 1, data = train.data) 
# use step() to run forward selection
abalonedf.fwd <- step(abalonedf.null,                                     # initial model
                      scope = list(abalonedf.null, upper = abalonedf),    # range of models
                      direction = "forward")                              # forward selection
summary(abalonedf.fwd)
# Which variables were added?
## All except sex_f

#backward elimination
abalonedf.back <- step(abalonedf,                    # start with full model
                       direction = "backward")       # backward elimination
summary(abalonedf.back)

#stepwise regression
abalonedf.step <- step(abalonedf.null,                                    # initial model
                       scope = list(abalonedf.null, upper = abalonedf),   # range of models
                       direction = "both")                                # stepwise regression
summary(abalonedf.step)

##### Executing the Full Model #####
## Linear Regression ##
abalonedf.lm <- lm(Rings ~ .,            # outcome ~ predictors
                   data = train.data)
options(scipen = 999)  # use options() to ensure numbers are not displayed in scientific notation
summary(abalonedf.lm)

library(forecast) 
valid.lm.full.pred <- predict(abalonedf.lm,    
                              valid.data)   

options(scipen = 999, digits = 2)
valid.resid <- valid.data$Rings - valid.lm.full.pred  

full <- data.frame("Predicted.full" = valid.lm.full.pred, 
                   "Actual" = valid.data$Rings,
                   "Residual.full" = valid.resid)

# how well the model predict the outcome based on valid dataset
options(digits = 5)
accuracy(valid.lm.full.pred, valid.data$Rings)



##### Executing the best model from Exhaustive Search #####

# the exhaustive search indicate that the 5-variable with the highest adjusted R, lowest CP and BIC 
# is the best optimize model we can have at this stage.
# The exhaustive search table also show the FALSE for Viscera and Sex-M in the fifth model
# In addition with reviewing the linear regression model summary, filter out the Viscera and Sex_M with high p-value
# indicating that they don't contribute significantly to the prediction mode;

train.data.exs = subset(train.data, select = -c(Viscera, Sex_M))
valid.data.exs = subset(valid.data, select = -c(Viscera, Sex_M))

#### The reduced model after Exhaustive Search ####
abalonedf.exs.lm <- lm(Rings ~ .,         
                       data = train.data.exs)
options(scipen = 999) 
valid.lm.exs.pred <- predict(abalonedf.exs.lm,     
                             valid.data.exs)   
options(scipen = 999, digits = 2)
valid.resid <- valid.data$Rings - valid.lm.exs.pred  
rd1 <- data.frame("Predicted.rd1" = valid.lm.exs.pred, 
                  "Actual" = valid.data.exs$Rings,
                  "Residual.rd1" = valid.resid)

# how well the model predict the outcome based on valid dataset
options(digits = 5)
accuracy(valid.lm.exs.pred, valid.data.exs$Rings)


##### Executing the reduced models based on Forward Selection, Backward Elimination, and Stepwise Regression #####

train.data.rd = subset(train.data, select = -c(Viscera, Sex_M, Sex_I))
valid.data.rd = subset(valid.data, select = -c(Viscera, Sex_M, Sex_I))
#### The reduced model after Exhaustive Search ####
abalonedf.rd.lm <- lm(Rings ~ .,         
                      data = train.data.rd)
options(scipen = 999) 
valid.lm.rd.pred <- predict(abalonedf.rd.lm,     
                            valid.data.rd)   
options(scipen = 999, digits = 2)

valid.resid <- valid.data$Rings - valid.lm.rd.pred  
rd2 <- data.frame("Predicted.rd2" = valid.lm.exs.pred, 
                  "Actual" = valid.data.exs$Rings,
                  "Residual.rd2" = valid.resid)

# how well the model predict the outcome based on valid dataset
options(digits = 5)
accuracy(valid.lm.exs.pred, valid.data.exs$Rings)

###### write out xlsx file to create table in excel ######
openxlsx::write.xlsx(full, 
                     "~/Desktop/SPRING 2022 (DKY 11 THANG 10)/BANA 4080/dataset/full.xlsx")
openxlsx::write.xlsx(rd1, 
                     "~/Desktop/SPRING 2022 (DKY 11 THANG 10)/BANA 4080/dataset/rd1.xlsx")
openxlsx::write.xlsx(rd2, 
                     "~/Desktop/SPRING 2022 (DKY 11 THANG 10)/BANA 4080/dataset/rd2.xlsx")

