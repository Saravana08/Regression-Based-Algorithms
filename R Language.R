##################################Basics of R###############################

# Calculate 3 + 4
3 + 4

# Calculate 6 + 12
6+12

# An addition
5 + 5 

# A subtraction
5 - 5 

# A multiplication
3 * 5

# Assign the value 42 to x
x <- 42

# Print out the value of the variable x
x


# Assign the value 5 to the variable my_apples
my_apples<-5

# Print out the value of the variable my_apples
my_apples


# Assign a value to the variables my_apples and my_oranges
my_apples <- 5
my_oranges <-6

# Add these two variables together
print(my_oranges+my_apples)

# Create the variable my_fruit
my_fruit<-print(my_apples+my_oranges)


# Assign a value to the variable my_apples
my_apples <- 5 

# Fix the assignment of my_oranges
my_oranges <- 6 

# Create the variable my_fruit and print it out
my_fruit <- my_apples + my_oranges 
print(my_fruit)


# Change my_numeric to be 42
my_numeric <- 42

# Change my_character to be "universe"
my_character <- "universe"

# Change my_logical to be FALSE
my_logical <- FALSE

print(my_logical)


# Declare variables of different types
my_numeric <- 42
my_character <- "universe"
my_logical <- FALSE 

########################Check the class of variable#########################

# Check class of my_numeric
class(my_numeric)

# Check class of my_character
class(my_character)

# Check class of my_logical
class(my_logical)

#####################For loops in r #############################################



# Create a vector filled with random normal values
u1 <- rnorm(30)
print("This loop calculates the square of the first 10 elements of vector u1")

# Initialize `usq`
usq <- 0

for(i in 1:10) {
  # i-th element of `u1` squared into `i`-th position of `usq`
  usq[i] <- u1[i]*u1[i]
  print(usq[i])
}

print(i)



# Create a 30 x 30 matrix (of 30 rows and 30 columns)
mymat <- matrix(nrow=30, ncol=30)

# For each row and for each column, assign values based on position: product of two indexes
for(i in 1:dim(mymat)[1]) {
  for(j in 1:dim(mymat)[2]) {
    mymat[i,j] = i*j
  }
}

# Just show the upper left 10x10 chunk
mymat[1:10, 1:10]


# Create your three-dimensional array
my_array <- array(1:20, dim=c(20, 20, 20))

for (i in 1:dim(my_array)[1]) {
  for (j in 1:dim(my_array)[2]) {
    for (k in 1:dim(my_array)[3]) {
      my_array[i,j,k] = i*j*k
    }
  }
}

# Show a 10x10x15 chunk of your array
my_array[1:10, 1:10, 1:15]



###################Changing the type of variable #####################################

# Change the Type of Variable
my_int <- 4
class(my_int)

nr <- as.integer(my_int)
class(nr)


###########if else loops #########################

x <- -5
if (x < 0) {
  print("Negative number")
}else if (x > 0) {
  print("Positive number")
}else {
  print("Zero")}



################## Instaling Packages #########################
install.packages("dplyr")



################### Plot ###################################
x <- seq(-pi,pi,0.1)
plot(x, sin(x))



plot(x, sin(x),
     main="The Sine Function",
     ylab="sin(x)")


#"p" - points
#"l" - lines
#"b" - both points and lines
#"c" - empty points joined by lines
#"o" - overplotted points and lines
#"s" and "S" - stair steps
#"h" - histogram-like vertical lines
#"n" - does not produce any points or lines


plot(x, sin(x),
     main="The Sine Function",
     ylab="sin(x)",
     type="l",
     col="blue")


################### Data Visualizations ############################

data(airquality)
# airquality = read.csv('airquality.csv',header=TRUE, sep=",")

head(airquality, n=3)
tail(airquality, n=3)

data<-airquality

summary(data)

plot(airquality$Ozone)
plot(airquality$Ozone, airquality$Wind)
plot(airquality)
# points and lines 
plot(airquality$Ozone, type= "b")
# high density vertical lines.
plot(airquality$Ozone, type= "h")
plot(airquality$Ozone, xlab = 'ozone Concentration', ylab = 'No of Instances', main = 'Ozone levels in NY city', col = 'green')
hist(airquality$Solar.R)
boxplot(airquality$Solar.R)
boxplot(airquality[,0:4], main='Multiple Box plots')


################## Machine learning Model in R ###############################

data <- cars
head(data)
plot(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot
scatter.smooth(x=cars$speed, y=cars$dist, main="Dist ~ Speed")  # scatterplot
par(mfrow=c(1, 2))  # divide graph area in 2 columns

boxplot(cars$speed, main="Speed", sub=paste("Outlier rows: ", boxplot.stats(cars$speed)$out))  # box plot for 'speed'

boxplot(cars$dist, main="Distance", sub=paste("Outlier rows: ", boxplot.stats(cars$dist)$out))  # box plot for 'distance'

cor(cars$speed, cars$dist) 
linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model on full data
print(linearMod)
summary(linearMod) 


# capture model summary as an object
modelSummary <- summary(linearMod)  

# model coefficients
modelCoeffs <- modelSummary$coefficients  

# get beta estimate for speed
beta.estimate <- modelCoeffs["speed", "Estimate"]

# get std.error for speed  
std.error <- modelCoeffs["speed", "Std. Error"]  


# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(cars), 0.8*nrow(cars))  # row indices for training data
trainingData <- cars[trainingRowIndex, ]  # model training data
testData  <- cars[-trainingRowIndex, ]   # test data

# Build the model on training data
lmMod <- lm(dist ~ speed, data=trainingData)  # build the model
distPred <- predict(lmMod, testData)  # predict distance

summary (lmMod)  


actuals_preds <- data.frame(cbind(actuals=testData$dist, predicteds=distPred))
actuals_preds

# Min-Max Accuracy Calculation
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  

# MAPE Calculation
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  



######################### Logistic Regression #####################################


# install.packages("mlbench")
data(BreastCancer, package="mlbench")
data<-BreastCancer
head(data)
bc <- BreastCancer[complete.cases(BreastCancer), ]  # create copy
str(data)
str(bc)

# remove id column
bc <- bc[,-1]
head(bc)

# convert factors to numeric
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}


bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))
str(bc)

table(bc$Class)

library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.

# Prep Training and Test data.
set.seed(100)
trainDataIndex <- createDataPartition(bc$Class, p=0.7, list = F)  # 70% training data
trainData <- bc[trainDataIndex, ]
testData <- bc[-trainDataIndex, ]

# Build Logistic Model
logitmod <- glm(Class ~  Cell.size + Cell.shape, family = "binomial", data=trainData)
summary(logitmod)

pred <- predict(logitmod, newdata = testData, type = "response")

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class

mean(y_pred == y_act) 


# linearMod <- lm(dist ~ speed, data=cars)  # build linear regression model
# logitMod <- glm(ABOVE50K ~ RELATIONSHIP + AGE + CAPITALGAIN + OCCUPATION + EDUCATIONNUM, data=trainingData, family=binomial)
# decision_tree = tree(High~.-Sales, data=carseats)
# randomforest = randomForest(medv~., data = boston, subset = train)
# boosting = gbm(medv~., data = boston[train,], distribution = "gaussian", n.trees = 10000, shrinkage = 0.01, interaction.depth = 4)
# xgb <- xgboost(data = data.matrix(X[,-1]), label = y,eta = 0.1, max_depth = 15,nround=25,subsample = 0.5, colsample_bytree = 0.5, seed = 1, eval_metric = "merror",objective = "multi:softprob", num_class = 12,nthread = 3)
# Knnmodel <- knn(iris_train,iris_test,cl=iris_target_category,k=13)
