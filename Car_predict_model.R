#Loading required packages
if (!require("pacman")) install.packages("pacman","caret","e1071","lift")
pacman::p_load(pacman,rio,tidyverse,caret,e1071,lift)
library(corrplot)
library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 

# LOADING DATA

dF <- read.csv("CarPrice_Assignment.csv")

# SUMMARIZING DATA

# Dimensions of data
dim(dF)

# Columns in the dataset
names(dF)

# Checking class for all the columns
sapply(dF,class)

# Categorical Columns Plots

par(mfrow = c(2,2))
# 1. Symboling
count1 <- table(dF$symboling)
barplot(count1, ylab = "count", xlab = "symboling", col = coul)


# 2. fueltype
count2 <- table(dF$fueltype)
barplot(count2, ylab = "count", xlab = "fuelType", col = coul)

# 3. aspiration
count3 <- table(dF$aspiration)
barplot(count3, ylab = "count", xlab = "aspiration", col = coul)

# 4. doornumber
count4 <- table(dF$doornumber)
barplot(count4, ylab = "count", xlab = "doornumber", col = coul)

# 5. carbody
count5 <- table(dF$carbody)
barplot(count5, ylab = "count", xlab = "carbody", col = coul)

# 6. drivewheel
count6 <- table(dF$drivewheel)
barplot(count6, ylab = "count", xlab = "drivewheel", col = coul)

# 7. enginelocation
count7 <- table(dF$enginelocation)
barplot(count7, ylab = "count", xlab = "enginelocation", col = coul)

# 8. enginetype
count8 <- table(dF$enginetype)
barplot(count8, ylab = "count", xlab = "enginetype", col = coul)

# 9. cylindernumber
count9 <- table(dF$cylindernumber)
barplot(count9, ylab = "count", xlab = "cylindernumber", col = coul)

# 10. fuelSystem
count10 <- table(dF$fuelsystem)
barplot(count10, ylab = "count", xlab = "fuelSystem", col = coul)

par(mfrow = c(1,1))

# Numeric Columns

# Response Variable - Price
summary(dF$price)
boxplot(dF$price, col = coul, ylab = "price", main = "Boxplot of Price")

par(mfrow = c(2,2))

# 1. wheelbase
plot(price ~ wheelbase, data = dF, main = "Price vs Wheelbase", col = "blue", pch = 16)
abline(lm(price ~ wheelbase, data = dF))

# 2. carlength
plot(price ~ carlength, data = dF, main = "Price vs carlength", col = "blue", pch = 16)
abline(lm(price ~ carlength, data = dF))

# 3. carwidth
plot(price ~ carwidth, data = dF, main = "Price vs carwidth", col = "blue", pch = 16)
abline(lm(price ~ carwidth, data = dF))

# 4. carheight
plot(price ~ carheight, data = dF, main = "Price vs carheight", col = "blue", pch = 16)
abline(lm(price ~ carheight, data = dF))

# 5. curbweight
plot(price ~ curbweight, data = dF, main = "Price vs curbweight", col = "blue", pch = 16)
abline(lm(price ~ curbweight, data = dF))

# 6. enginesize
plot(price ~ enginesize, data = dF, main = "Price vs enginesize", col = "blue", pch = 16)
abline(lm(price ~ enginesize, data = dF))

# 7. boreratio
plot(price ~ boreratio, data = dF, main = "Price vs boreratio", col = "blue", pch = 16)
abline(lm(price ~ boreratio, data = dF))

# 8. stroke
plot(price ~ stroke, data = dF, main = "Price vs stroke", col = "blue", pch = 16)
abline(lm(price ~ stroke, data = dF))

# 9. compressionratio
plot(price ~ compressionratio, data = dF, main = "Price vs compressionratio", col = "blue", pch = 16)
abline(lm(price ~ compressionratio, data = dF))

# 10. horsepower
plot(price ~ horsepower, data = dF, main = "Price vs horsepower", col = "blue", pch = 16)
abline(lm(price ~ horsepower, data = dF))

# 11. peakrpm
plot(price ~ peakrpm, data = dF, main = "Price vs peakrpm", col = "blue", pch = 16)
abline(lm(price ~ peakrpm, data = dF))

# 12. citympg
plot(price ~ citympg, data = dF, main = "Price vs citympg", col = "blue", pch = 16)
abline(lm(price ~ citympg, data = dF))

# 13. highwaympg
plot(price ~ highwaympg, data = dF, main = "Price vs highwaympg", col = "blue", pch = 16)
abline(lm(price ~ highwaympg, data = dF))

par(mfrow = c(1,1))

# VISUALIZING DATA

#Some insights into data
# 1.
boxplot(dF$price ~ dF$enginelocation, col = coul, 
        ylab = "price", xlab = "enginelocation")

#2.
boxplot(dF$price ~ dF$enginetype, col = coul,
        ylab = "price", xlab = "engineType")

#3.
boxplot(dF$price ~ dF$fuelsystem, col = coul,
        ylab = "price", xlab = "fuelSystem")

par(mfrow = c(1,2))
#4.Enginesize vs citympg
plot(dF$enginesize ~ dF$citympg,
     main = "Enginesize vs Citympg",
     xlab = " citympg",
     ylab = "enginesize",
     pch = ifelse(dF$fueltype == "gas", 1,3),
     col = ifelse(dF$fueltype == "gas", "blue","green")
)
abline(lm(dF$enginesize ~ dF$citympg, subset = dF$fueltype == "gas"),
       col = "blue")
abline(lm(dF$enginesize ~ dF$citympg, subset = dF$fueltype == "diesel"),
       col = "green")
legend("topright",legend = c("gas","diesel"),
       pch = c(1,3),col = c("blue","green"))

plot(price ~ citympg, data = dF, main = " Price vs citympg", xlab = "citympg",
     ylab = " price", col = "blue", pch = 16)
abline(lm(price ~ citympg, data = dF))

#5. Enginesize vs highwaympg
plot(dF$enginesize ~ dF$highwaympg,
     main = "Enginesize vs Highwaympg",
     xlab = " highwaympg",
     ylab = "enginesize",
     pch = ifelse(dF$fueltype == "gas", 1,3),
     col = ifelse(dF$fueltype == "gas", "blue","green")
)
abline(lm(dF$enginesize ~ dF$highwaympg, subset = dF$fueltype == "gas"),
       col = "blue")
abline(lm(dF$enginesize ~ dF$highwaympg, subset = dF$fueltype == "diesel"),
       col = "green")
legend("topright",legend = c("gas","diesel"),
       pch = c(1,3),col = c("blue","green"))
plot(price ~ highwaympg, data = dF, main = " Price vs highwaympg", xlab = "highwaympg",
     ylab = " price", col = "blue", pch = 16)
abline(lm(price ~ highwaympg, data = dF))

par(mfrow = c(1,1))

#6. Identifying predictor columns with strong correlation
temp.dF1 <- dF[,c("wheelbase","carlength","carwidth", "carheight","curbweight","enginesize",
                  "boreratio","stroke","compressionratio", "horsepower","peakrpm",
                  "citympg", "highwaympg")]
Corr.matrix <- cor(temp.dF1)
corrplot(Corr.matrix, method = "number", type = "upper")

# ANALYZING DATA

# 1. Data Preparation

# Checking the count of null values
sum(is.na(dF)) # Data does not have any null values.

# Converting word to number for cylindernumber and doornumber
dF$cylindernumber[dF$cylindernumber == "two"] <- 2
dF$cylindernumber[dF$cylindernumber == "three"] <- 3
dF$cylindernumber[dF$cylindernumber == "four"] <- 4
dF$cylindernumber[dF$cylindernumber == "five"] <- 5
dF$cylindernumber[dF$cylindernumber == "six"] <- 6
dF$cylindernumber[dF$cylindernumber == "eight"] <- 8
dF$cylindernumber[dF$cylindernumber == "twelve"] <- 12
unique(dF$cylindernumber)
dF$doornumber[dF$doornumber == "two"] <- 2
dF$doornumber[dF$doornumber == "four"] <- 4
unique(dF$doornumber)

# Converting from character to numeric

dF$cylindernumber <- as.numeric(dF$cylindernumber)
unique(dF$cylindernumber)
class(dF$cylindernumber)
dF$doornumber <- as.numeric(dF$doornumber)
unique(dF$doornumber)
class(dF$doornumber)

# Creating factors
dF$symboling <- factor(dF$symboling, levels = c("-2","-1","0","1","2","3"))
dF$fueltype <- factor(dF$fueltype)
dF$aspiration <- factor(dF$aspiration)
dF$doornumber <- factor(dF$doornumber)
dF$carbody <- factor(dF$carbody)
dF$drivewheel <- factor(dF$drivewheel, levels = c("fwd","rwd","4wd"))
dF$enginelocation <- factor(dF$enginelocation)
dF$enginetype <- factor(dF$enginetype)
dF$cylindernumber <- factor(dF$cylindernumber)
dF$fuelsystem <- factor(dF$fuelsystem, 
                        levels = c("1bbl","2bbl","4bbl","idi","mpfi","mfi","spfi","spdi"))

# Converting car_id from numeric to character

typeof(dF$car_ID)
dF$car_ID <- as.character(dF$car_ID)
typeof(dF$car_ID)

# Checking the structure of data
str(dF)

# Dropping columns that are not useful
# 1. Drop CarName
dF <- dF[,-c(1,3)]

# Handling Outlier
cook <- lm(price ~., data = dF)
cooksd <- cooks.distance(cook)
plot(cooksd, pch = "*", cex =2, main = "Outliers by Cooks distance")
identify(cooksd)

## Drop columns - cylindernumber and fuelSystem to avoid NA as estimate
dF <- dF[,-c(14,16)]

# Partitioning into training and testing datasets
set.seed(1)
training.rows <- sample(1:nrow(dF), size = (.8*nrow(dF)))
dF.train <- dF[training.rows,]
dim(dF.train)
dF.test <- dF[-training.rows,]
dim(dF.test)

# 2. Applying Linear Regression on dataset

# Full Model 
full.model <- lm(price ~ ., data = dF.train)
summary(full.model) # R-squared:0.9245 and standard error: 2530

full.model.predictions.train <- predict(full.model, newdata = dF.train)
sqrt(mean((full.model.predictions.train - dF.train$price)^2)) # 2243.782
full.model.predictions.test <- predict(full.model, newdata = dF.test)
sqrt(mean((full.model.predictions.test - dF.test$price)^2)) # 2362.082

# Step model
step.model <- step(full.model)
summary(step.model) # R-squared:0.9207 and standard error: 2498

step.model.predictions.train <- predict(step.model, newdata = dF.train)
sqrt(mean((step.model.predictions.train - dF.train$price)^2)) # 2299.712
step.model.predictions.test <- predict(step.model, newdata = dF.test)
sqrt(mean((step.model.predictions.test - dF.test$price)^2)) # 2248.812

# Model without strongly correlated predictor columns

# 1. dropping citympg and keeping highwaympg
model1 <- lm(price ~ symboling + fueltype + aspiration + doornumber + 
               carbody + drivewheel + enginelocation + wheelbase       
             +carlength + carwidth + carheight + curbweight + 
               enginetype + enginesize + boreratio + stroke +
               compressionratio + horsepower + peakrpm + highwaympg, 
             data = dF.train)    
summary(model1) #R-squared:0.9245 and standard error: 2521

model1.predictions.train <- predict(model1, newdata = dF.train)
sqrt(mean((model1.predictions.train - dF.train$price)^2)) # 2244.6
model1.predictions.test <- predict(model1, newdata = dF.test)
sqrt(mean((model1.predictions.test - dF.test$price)^2)) # 2361.236

# 2. dropping highwaympg and keeping citympg
model2 <- lm(price ~ symboling + fueltype + aspiration + doornumber + 
               carbody + drivewheel + enginelocation + wheelbase       
             +carlength + carwidth + carheight + curbweight + 
               enginetype + enginesize + boreratio + stroke +
               compressionratio + horsepower + peakrpm + citympg, 
             data = dF.train)    
summary(model2) #R-squared:0.9234 and standard error: 2539

model2.predictions.train <- predict(model2, newdata = dF.train)
sqrt(mean((model2.predictions.train - dF.train$price)^2)) # 2260.468
model2.predictions.test <- predict(model2, newdata = dF.test)
sqrt(mean((model2.predictions.test - dF.test$price)^2)) # 2278.214

#### highwaympg should dropped.

# 3. dropping carlength and keeping carwidth and curbweight
model3 <- lm(price ~ symboling + fueltype + aspiration + doornumber + 
               carbody + drivewheel + enginelocation + wheelbase       
             + carwidth + carheight + curbweight + enginetype 
             + enginesize + boreratio + stroke + compressionratio 
             + horsepower + peakrpm + citympg, 
             data = dF.train)    
summary(model3) #R-squared:0.9232 and standard error:2533

model3.predictions.train <- predict(model3, newdata = dF.train)
sqrt(mean((model3.predictions.train - dF.train$price)^2)) # 2263.517
model3.predictions.test <- predict(model3, newdata = dF.test)
sqrt(mean((model3.predictions.test - dF.test$price)^2)) # 2302.439

# 4. dropping carlength and carwidth and keeping curbweight
model4 <- lm(price ~ symboling + fueltype + aspiration + doornumber + 
               carbody + drivewheel + enginelocation + wheelbase       
             + carheight + curbweight + enginetype + enginesize +
               boreratio + stroke + compressionratio + horsepower + 
               peakrpm + citympg, 
             data = dF.train)    
summary(model4) #R-squared:0.9193  and standard error:2585

model4.predictions.train <- predict(model4, newdata = dF.train)
sqrt(mean((model4.predictions.train - dF.train$price)^2)) # 2319.52
model4.predictions.test <- predict(model4, newdata = dF.test)
sqrt(mean((model4.predictions.test - dF.test$price)^2)) # 2424.491 

# 5. dropping carwidth and keeping carlength and curbweight
model5 <- lm(price ~ symboling + fueltype + aspiration + doornumber + 
               carbody + drivewheel + enginelocation + wheelbase       
             +carlength + carheight + curbweight + 
               enginetype + enginesize + boreratio + stroke +
               compressionratio + horsepower + peakrpm + citympg, 
             data = dF.train)    
summary(model5) #R-squared:0.9193 and standard error:2595

model5.predictions.train <- predict(model5, newdata = dF.train)
sqrt(mean((model5.predictions.train - dF.train$price)^2)) # 2319.52
model5.predictions.test <- predict(model5, newdata = dF.test)
sqrt(mean((model5.predictions.test - dF.test$price)^2)) # 2424.372

# 6. dropping curbweight and keeping carlength and carwidth
model6 <- lm(price ~ symboling + fueltype + aspiration + doornumber + 
               carbody + drivewheel + enginelocation + wheelbase       
             +carlength + carwidth + carheight  + 
               enginetype + enginesize + boreratio + stroke +
               compressionratio + horsepower + peakrpm + citympg, 
             data = dF.train)    
summary(model6) #R-squared:0.9203  and standard error:2580

model6.predictions.train <- predict(model6, newdata = dF.train)
sqrt(mean((model6.predictions.train - dF.train$price)^2)) # 2306.222
model6.predictions.test <- predict(model6, newdata = dF.test)
sqrt(mean((model6.predictions.test - dF.test$price)^2)) # 2157.988

#### carlength should be dropped.

# 7. Model without any non- significant columns
model7 <- lm(price ~ fueltype + aspiration +  
               carbody + drivewheel + enginelocation + wheelbase       
             + carwidth + curbweight + enginetype + enginesize 
             + boreratio + stroke + compressionratio + peakrpm, 
             data = dF.train)    
summary(model7) #R-squared:0.9112 and standard error:2634

model7.predictions.train <- predict(model7, newdata = dF.train)
sqrt(mean((model7.predictions.train - dF.train$price)^2)) # 2433.313
model7.predictions.test <- predict(model7, newdata = dF.test)
sqrt(mean((model7.predictions.test - dF.test$price)^2)) # 2003.325

# Full model without outliers
dF.without.outliers <- dF[-c(3,17,75,135),]

set.seed(1)
dF.without.outliers.training.rows <- sample(1:nrow(dF.without.outliers), 
                                            size = (.8*nrow(dF.without.outliers)))
dF.without.outliers.train <- dF.without.outliers[dF.without.outliers.training.rows,]
dim(dF.without.outliers.train)
dF.without.outliers.test <- dF.without.outliers[-dF.without.outliers.training.rows,]
dim(dF.without.outliers.test)

model8 <- lm(price ~ symboling + fueltype + aspiration + doornumber + 
               carbody + drivewheel + enginelocation + wheelbase       
             + carwidth + carheight + curbweight + 
               enginetype + enginesize + boreratio + stroke +
               compressionratio + horsepower + peakrpm + citympg, 
             data = dF.without.outliers.train)    
summary(model8) #R-squared:0.9416  and standard error:2148

model8.predictions.train <- predict(model8, newdata = dF.without.outliers.train)
sqrt(mean((model8.predictions.train - dF.without.outliers.train$price)^2)) # 1913.431
model8.predictions.test <- predict(model8, newdata = dF.without.outliers.test)
sqrt(mean((model8.predictions.test - dF.without.outliers.test$price)^2)) #  2438.531

# dropping all non-significant columns
model9 <- lm(price ~ symboling + fueltype + doornumber + 
               carbody + drivewheel + enginelocation + wheelbase + 
               carwidth + carheight + enginetype + enginesize + 
               boreratio + stroke + compressionratio +  peakrpm +
               citympg, 
             data = dF.without.outliers.train)    
summary(model9) #R-squared:0.9314  and standard error:2301

model9.predictions.train <- predict(model9, newdata = dF.without.outliers.train)
sqrt(mean((model9.predictions.train - dF.without.outliers.train$price)^2)) # 2073.727
model9.predictions.test <- predict(model9, newdata = dF.without.outliers.test)
sqrt(mean((model9.predictions.test - dF.without.outliers.test$price)^2)) # 2161.004

#### Excluding influential records improves R-squared and RMSE.
#### Dropping non-significant columns leads to better results.

# Model 9 will be used for analyzing data.

# Questions

# Q1.
plot(model9.predictions.train ~ dF.without.outliers.train$price,
     main = "Regression model on Training Data",
     xlab = "Actual Price",
     ylab = "Prediced Price",
     xlim = c(0, 50000),
     ylim = c(0, 50000)
)
abline(0,1)

plot(model9.predictions.test ~ dF.without.outliers.test$price,
     main = "Regression model on Test Data",
     xlab = "Actual Price",
     ylab = "Prediced Price",
     xlim = c(0, 50000),
     ylim = c(0, 50000))
abline(0,1)

# Q2.
dF[dF$price == max(dF$price),]

# Q3.
dF[dF$price == min(dF$price),]

# Q4.
mean(dF$price)

# Q5.
mean(dF$carheight[dF$carbody == "sedan"])
mean(dF$carwidth[dF$carbody == "sedan"])
mean(dF$carlength[dF$carbody == "sedan"])
