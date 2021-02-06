#-----------------------------------------PREPROCESSING-----------------------------------------------
#Importing Dataset
dataset = read.csv("C:\\Users\\A_R COMPUTERS\\Downloads\\zomato_restaurants_in_India.csv")
#View Dataset
# View(dataset)
# #Pre- Processing
# colnames(dataset)
# sapply(dataset, class)
# head(dataset)
# summary(dataset)
# #Showing number of missing values
# sum(is.na(dataset))
# #showing column which contains missing values
# colSums(is.na(dataset))
# #Dealing With Missing Values
# dataset$opentable_support = ifelse(is.na(dataset$opentable_support),ave(dataset$opentable_support, FUN = function(x) mean(x, na.rm = 'TRUE')),dataset$opentable_support)
# #Showing no of missing values after removing the missing values
# sum(is.na(dataset))
# #---------------------------------------EXPLORATORY DATA ANALYSIS-------------------------------------
# #showing bar graph between cities name and number of Restraunt
# table(dataset$city)
# barplot(table(dataset$city),xlab="Name Of City",ylab="Number Of Restraunt",main='Bar Plot')
# #showing Histogram frequency of  cost of the two in agra city
# hist(dataset$average_cost_for_two[dataset$city=="Agra"],ylim=c(10,1700),las=1, xlab="Cost For Two",breaks=5,main='Histogram')
# #showing box plot for votes VS Cities
# boxplot(dataset$votes~dataset$city,xlab="City",ylab="Votes",ylim=c(200,1000),main='Box Plot')
#--------------------------APPLYING MODEL ON THE DATASET----------------------------------------------
#standarize votes and average_cost_for_two
#install.packages('caTools') #install once
library(caTools) # importing caTools library
set.seed(123)
split = sample.split(dataset, SplitRatio = 0.7)
#split
training_set = subset(dataset, split =" TRUE")
test_set = subset(dataset, split =" FALSE")
#training_set
#test_set
#create the model
model=lm(average_cost_for_two ~ votes,aggregate_rating,data=training_set)
#summary(model)
#Prediction
pred=predict(model,test_set)
#pred
#comparing predicted vs actual values
plot(test_set$average_cost_for_two,type="l",lty=1.8,col="red")
lines(pred,type="l",col="blue")
#plot(pred,type="l",lty=1.8,col="blue")
#finding accuracy
rmse=sqrt(mean(pred-dataset$average_cost_for_two)^2)
rmse