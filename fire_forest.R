
# Importing library
library(ggplot2)
library(dplyr)
library(UsingR)
library(reshape2)
library(lattice)
library(kernlab)
library(caret)
library(corrplot)
library(MASS)
library(car)
library(perturb)

# Loading file
getwd()
setwd("C:/Users/DELL/Documents/Imarticus_data")
forest_fire <- read.csv("forestfires.csv")

# Analyzing File
head(forest_fire)
tail(forest_fire)

# Dimention
dim(forest_fire)

# Structure
str(forest_fire)

# Summary
summary(forest_fire)

# Finding NA values in tahe dataset
sum(is.na(forest_fire))

# Creating corrplot
numeric = select_if(forest_fire,is.numeric)
c = cor(numeric)
corrplot(c, method = "color")

# Barplot to understnad the month and temperature realation 
ggplot(data = forest_fire, aes(x=month, y=temp)) + geom_bar(stat = "identity")

ggplot(data = forest_fire, aes(x=month, y=wind)) + geom_bar(stat = "identity")

ggplot(data = forest_fire, aes(x=month, y=rain)) + geom_bar(stat = "identity")

ggplot(data = forest_fire, aes(x = area)) + geom_histogram()
range(forest_fire$area)

# Dividing into training and test
inTrain <- createDataPartition(forest_fire$area,p = .2,list = F)
Training <- forest_fire[inTrain,]
Testing <- forest_fire[-inTrain,]
dim(Training)

# We then inspect the distribution of each variable in boxplots

boxplot(Training$X,main="X")
boxplot(Training$Y,main ='Y')
boxplot(Training$FFMC, main='FFMC') #outliers
boxplot(Training$DMC, main ='DMC') # outliers
boxplot(Training$DC, main='DC') # some outliers
boxplot(Training$ISI,main='ISI') # outliers
boxplot(Training$temp, main='temp') 
boxplot(Training$RH,main="RH") # outliers
boxplot(Training$wind, main='wind') #
boxplot(Training$rain, main='rain')  # heavy outliers...high variability in data
boxplot(Training$area, main='area') # heavy outliers..high variability in data

# Let's see the prob density distribution curve of response variable area
dar <- data.frame(x=Training$area) 
ggplot(dar,aes(x=Training$area))+geom_density(fill='red')

# Creating linear model
mod1 <- lm(area~X+Y+month+day+FFMC+DMC+DC+ISI+temp+RH+wind+rain,data=Training)
summary(mod1)

par(mfrow=c(2, 2))
plot(mod1)

# Lets see residual plot with variables.
residualPlots(mod1)

# Cheacking RMSE value
RMSE(Testing$area,prediction)






#---------------------------------------------------------------------------------------------#
# SVM 

hist(forest_fire$area)
rug(forest_fire$area)

forest_fire <- mutate(forest_fire, y = log(area + 1))  # default is to the base e, y is lower case
hist(forest_fire$y)

normalise <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))  # subtract the min value in x and divide by the range of values in x.
}

forest_fire$temp <- normalise(forest_fire$temp)
forest_fire$rain <- normalise(forest_fire$rain)
forest_fire$RH <- normalise(forest_fire$RH)
forest_fire$wind <- normalise(forest_fire$wind)

sum(forest_fire$area < 5) 
sum(forest_fire$area >= 5)

forest_fire$size <- NULL
forest_fire$size <- factor(ifelse(forest_fire$area < 5, 1, 0),
                      labels = c("small", "large"))


train <- sample(x = nrow(forest_fire), size = 400, replace = FALSE)


m.poly <- ksvm(size ~ temp + RH + wind + rain,
               data = forest_fire[train, ],
               kernel = "polydot", C = 1)
m.poly

m.rad <- ksvm(size ~ temp + RH + wind + rain,
              data = forest_fire[train, ],
              kernel = "rbfdot", C = 1)
m.rad

m.tan <- ksvm(size ~ temp + RH + wind + rain,
              data = forest_fire[train, ],
              kernel = "tanhdot", C = 1)
m.tan

table(forest_fire$size)

# Test with training data
pred <- predict(m.rad, newdata = forest_fire[-train, ], type = "response")

table(pred, forest_fire[-train, "size"])  

range(forest_fire$area)

ggplot(data =forest_fire, aes(x = area)) +
  geom_histogram(binwidth = 100)

dim(forest_fire[forest_fire$area != 0, ])
dim(forest_fire)

#Confussion matrix
confusionMatrix(table(pred, forest_fire[-train, "size"]), positive = "small")  # from the caret package, also need e1071 package
