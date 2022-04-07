weather <- read.csv("weatherAUS.csv", header = T, stringsAsFactors = T)
dim(weather)
str(weather)
#Conversions 
weather$Date <-  as.Date(weather$Date, format="%m/%d/%Y")
weather$Location <- as.character(weather$Location)
#Get the names of our variables
names(weather)
t(names(weather))

summary(weather)
colSums(is.na(weather))
weather$MinTemp[is.na(weather$MinTemp)] <- median(weather$MinTemp, na.rm = TRUE)
weather$MaxTemp[is.na(weather$MaxTemp)] <- median(weather$MaxTemp, na.rm = TRUE)
weather$Rainfall[is.na(weather$Rainfall)] <- median(weather$Rainfall, na.rm = TRUE)
weather$Evaporation[is.na(weather$Evaporation)] <- median(weather$Evaporation, na.rm = TRUE)
weather$Sunshine[is.na(weather$Sunshine)] <- median(weather$Sunshine, na.rm = TRUE)
weather$WindGustSpeed[is.na(weather$WindGustSpeed)] <- median(weather$WindGustSpeed, na.rm = TRUE)
weather$WindSpeed9am[is.na(weather$WindSpeed9am)] <- median(weather$WindSpeed9am, na.rm = TRUE)
weather$WindSpeed3pm[is.na(weather$WindSpeed3pm)] <- median(weather$WindSpeed3pm, na.rm = TRUE)
weather$Humidity9am[is.na(weather$Humidity9am)] <- median(weather$Humidity9am, na.rm = TRUE)
weather$Humidity3pm[is.na(weather$Humidity3pm)] <- median(weather$Humidity3pm, na.rm = TRUE)
weather$Pressure9am[is.na(weather$Pressure9am)] <- median(weather$Pressure9am, na.rm = TRUE)
weather$Pressure3pm[is.na(weather$Pressure3pm)] <- median(weather$Pressure3pm, na.rm = TRUE)
weather$Cloud9am[is.na(weather$Cloud9am)] <- median(weather$Cloud9am, na.rm = TRUE)
weather$Cloud3pm[is.na(weather$Cloud3pm)] <- median(weather$Cloud3pm, na.rm = TRUE)
weather$Temp9am[is.na(weather$Temp9am)] <- median(weather$Temp9am, na.rm = TRUE)
weather$Temp3pm[is.na(weather$Temp3pm)] <- median(weather$Temp3pm, na.rm = TRUE)
summary(weather)

#Dummy Variables
library(fastDummies)
dummy1 <- dummy_cols(weather,
                     select_columns = "Location",
                     remove_first_dummy = TRUE,
                     remove_selected_columns = TRUE)
dummy1

#Correlation Matrix
crr <- round(cor(weather[,c(-1,-2,-8,-10,-11,-22,-23)]),
             2)
crr

#Heatmap
library(gplots)
install.packages("binovisualfields")
library(binovisualfields)
heatmap.2(cor(weather[,c(-1,-2,-8,-10.-11.-22.-23)]),
          RowV = FALSE, Colv = FALSE,
          dendrogram = 'none',
          col = cm.colors(36),
          cellnote = crr,
    notecol = 'black',
    key = FALSE,
    trace = 'none',
    margins = c(10,8))

#Visualizations
#Temperature Histograms at 9am and 3pm
weather$Temp9am[is.na(weather$Temp9am)] <- median(weather$Temp9am, na.rm = T)
weather$Temp3pm[is.na(weather$Temp3pm)] <- median(weather$Temp3pm, na.rm = T)

#slight positive skew
T9am <- hist(weather$Temp9am, # the numeric variable of interest
             xlim = c(-1.30, 40), ylim = c(0, 11000), # defines the x/y-axis range
             col = "red", 
             xlab = "Temperature (degress C)", ylab = "Number of Observations",
             main = "Temperature in Australia at 9am")
text(x = T9am$mids, y = T9am$counts, labels = T9am$counts, cex = 0.5, pos = 3)
#More positively skewed 
T3pm <- hist(weather$Temp3pm, # the numeric variable of interest
             xlim = c(3.70, 46.70), ylim = c(0, 10500), # defines the x/y-axis range
             col = "blue", 
             xlab = "Temperature (degress C)", ylab = "Number of Observations",
             main = "Temperature in Australia at 3pm")
text(x = T3pm$mids, y = T3pm$counts, labels = T9am$counts, cex = 0.5, pos = 3)
#Sunshine histogram
weather$Sunshine[is.na(weather$Sunshine)] <- median(weather$Sunshine, na.rm = T)
sun <- hist(weather$Sunshine, # the numeric variable of interest
            xlim = c(0, 15), ylim = c(0, 25000), # defines the x/y-axis range
            col = "yellow", 
            xlab = "Hours of Sunshine", ylab = "Number of Observations",
            main = "Hours of Bright Sunshine in Australia")
text(x = sun$mids, y = sun$counts, labels = sun$counts, cex = 0.5, pos = 3)

barplot(height = weather$Evaporation, # value for bar height
        names.arg = weather$Location,       # label the bars
        xlab = "Location", ylab = "Evaporation",  # axis titles
        main = "Amount of Evaporation by Location")
barplot(height = weather$MaxTemp, # value for bar height
        names.arg = weather$Location,       # label the bars
        xlab = "Location", ylab = "MaxTemp",  # axis titles
        main = "Max Temperature by Location")
#Location Vs. Rainfall
barplot(height = weather$Rainfall, # value for bar height
        names.arg = weather$Location, # label the bars
        xlab = "Location", ylab = "Rainfall",  # axis titles
        main = "Location Vs. Rainfall") 

plot(weather$Humidity9am ~ weather$Pressure9am, ylab = "Humidity at 9am", xlab = "Pressure at 9am")
#Scatter plot showing relationship between Humidity at 3pm and Rainfall
weather$Rainfall[is.na(weather$Rainfall)] <- median(weather$Rainfall, na.rm = T)
weather$Humidity3pm[is.na(weather$Humidity3pm)] <- median(weather$Humidity3pm, na.rm = T)
plot(weather$Rainfall ~ weather$Humidity3pm,      # yvariable ~ xvariable
     ylab = "Rainfall (mm)", xlab = "Humidity % At 3pm", 
     main = "Humidity at 3pm Vs. Rainfall")
abline(lm(weather$Rainfall ~ weather$Humidity3pm, data = weather), col = "skyblue")

boxplot(weather$WindSpeed9am ~ weather$RainTomorrow,
        xlab = "RainTomorrow", ylab = "WindSpeed9am",
        main = "Distribution of WindSpeed9am by RainTomorrow")
boxplot(weather$Sunshine ~ weather$RainTomorrow,
        xlab = "RainTomorrow", ylab = "Sunshine",
        main = "Distribution of Sunshine by RainTomorrow")
