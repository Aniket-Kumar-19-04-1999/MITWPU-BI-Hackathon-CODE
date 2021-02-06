# packages needed ----
pkgs <- c("openxlsx", "data.table", "tibble", "stringr", "RColorBrewer", "wesanderson", "viridis", "Hmisc", "ggplot2", "corrplot", "leaflet")
rc <- sapply(pkgs, function(x){library(package = x, character.only = T, logical.return = T)}, simplify = F)

dataset <- data.table(read.csv("C:\\Users\\A_R COMPUTERS\\Downloads\\zomato_restaurants_in_india.csv", header = T, sep = ",", encoding = "UTF-8"))
# dimension of dataset ----
cat("\n dimension of dataset = ", dim(dataset))

cat("\n Total number  of DUPLICATED entries = ", nrow(dataset[duplicated(dataset)]))
# unique records only filtered
dataset <- unique(dataset)
cat("\n Total number  of UNIQUE entries = ", nrow(dataset))

cat("\n Number of records with missing value = ", nrow(dataset[!complete.cases(dataset)]))
cat("\n 'establishment' column values - ") 
table(dataset$establishment)  
cat("\n Dealing with [] values, that is, renaming [] to NA\n") 
dataset[establishment == "[]"]$establishment <- NA
cat("\n For the rest of the values in the column - splitting from ['<value>'] --> value\n")
dataset$establishment <- unlist(lapply(strsplit(dataset$establishment, split = "\\'"), "[", 2))
table(dataset$establishment, useNA = "if")
sum(is.na(dataset))
dataset<-dataset[complete.cases(dataset),]
sum(is.na(dataset))
dim(dataset)

# 1) No. of restaurants per city
res_city <- suppressMessages(dcast(dataset, city ~., value.var = ("res_id")))
names(res_city)[-1] <- "count_of_rest"
# order in decreasing order
res_city <- res_city[order(count_of_rest, decreasing = T)]
# find what percentage of restaurants of total the city has
res_city[, percent := round(res_city$count_of_rest/ sum(res_city$count_of_rest), digits = 2)]
res_city[, cum_percent := cumsum(percent)]
cat("\n plot for count of top 50% restaurants in all the cities")
d <- res_city[cum_percent <= 0.50]
barplot(height = d$count_of_rest, names.arg = d$city, col = c("blue"), main = "No. of restaurants for top 50% cities",xlab = "city", ylab = "frequency", las = 2)
#legend(x = 7, y = 2500, d$city, fill = rainbow(length(d$city)), cex = 0.7, ncol = 4, text.width = 4)
rm(d)

# 2) No. of restaurants per category
res_category <- dcast(dataset, establishment ~ ., value.var = ("res_id"))
names(res_category) <- c("category", "count_of_rest")
res_category <- res_category[order(count_of_rest, decreasing = T)]
# find what percentage of restaurants (with respect to category) of total the city has
res_category[, percent := round(res_category$count_of_rest/ sum(res_category$count_of_rest), digits = 2)]
res_category[, cum_percent := cumsum(percent)]
d <- res_category[cum_percent <= 0.90]
x <- d$count_of_rest
pieper <- round(100*x/sum(x),2)
pie(x,labels=pieper,main = "Category of Major proportion of Restaurants",col=rainbow(length(x)),radius=1)
legend("topright", d$category, cex = 0.7,fill = rainbow(length(x)))
rm(d)

# 3) No. of restaurants of major brands.
res_name <- suppressMessages(dcast(dataset, name ~ ., value.var = ("res_id")))
names(res_name) <- c("restaurant_name", "count_of_rest")
# order in decreasing order
res_name <- res_name[order(count_of_rest, decreasing = F)]
d <- res_name[count_of_rest>100]
barplot(d$count_of_rest,names.arg=d$restaurant_name, col = c("red"), main = "No. of restaurants of major brands",xlab = "Count", las = 1,horiz=T,cex.names=0.6)
#legend(x = 7, y = 2500, d$city, fill = rainbow(length(d$city)), cex = 0.7, ncol = 4, text.width = 4)
rm(d)

## getting correlation between variables
c <- cor(dataset[, c("average_cost_for_two", "price_range", "aggregate_rating", "votes", "photo_count")])
corrplot::corrplot(c)

#Applying ML model
dataset$scaled_photos<- scale(dataset$photo_count)
dataset$scaled_votes <- scale(dataset$votes)

set.seed(1)
row.number <- sample(1:nrow(dataset), 0.8*nrow(dataset))
trainset <- dataset[row.number,c("scaled_photos","scaled_votes")]
testset <- dataset[-row.number,c("scaled_photos","scaled_votes")]
trainset <- data.frame(train)
testset <- data.frame(test)
dim(trainset)
dim(testset)

model <- lm(scaled_votes ~ scaled_photos,data=trainset)
summary(model)
pred <- predict(model,testset)

rmse <- sqrt(mean(pred-testset$scaled_votes)^2)
rmse