# Import the dataset
sales <- read.csv('Revenue.csv')
head(sales) #Displays the top 6 rows of a dataset
summary(sales) #Gives certain statistical information about the data. 

dim(sales) # Displays the dimensions of the dataset
plot(sales) # Plot the variables to see their trends

library(corrplot) # Library to finds the correlation between the variables
num.cols<-sapply(sales, is.numeric)
num.cols
cor.data<-cor(sales[,num.cols])
cor.data
corrplot(cor.data, method='color')


set.seed(2)

library(caTools) #caTools has the split function

split <- sample.split(sales, SplitRatio = 0.7) 
# Assigning it to a variable split sample.split is one of the functions we are 
#using. With the ration value of 0.7, it states that we will have 70% of
# the sales data for training and 30% for testing the model

split

train <- subset(sales, split = 'TRUE') #Creating a training set

test <- subset(sales, split = 'FALSE') #Creating a testing set by assigning FALSE

head(train)

head(test)

View(train)

View(test)

Model <- lm(Revenue ~., data = train) 
#Creates the model. Here, lm stands for the linear regression model. 
#Revenue is the target variable we want to track.
summary(Model)

Model <- lm(Revenue ~., data = train) 
#Creates the model. Here, lm stands for the linear regression model. 
#Revenue is the target variable we want to track.
summary(Model)
pred <- predict(Model, test) #The test data was kept for this purpose
pred 
res<-residuals(Model) # Find the residuals
res<-as.data.frame(res) # Convert the residual into a dataframe
res 
results<-cbind(pred,test$Revenue)
results
colnames(results)<-c('predicted','real')
results<-as.data.frame(results)
head(results)

plot(test$Revenue, type = 'l', lty = 1.8, col = "red")

lines(pred, type = "l", col = "blue") 
plot(pred, type = "l", lty = 1.8, col = "blue") 
rmse <- sqrt(mean(pred-sales$Revenue)^2) 
# Root Mean Square Error 
rmse
