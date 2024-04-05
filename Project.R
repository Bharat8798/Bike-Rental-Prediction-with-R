library(readxl)


data <- read_excel(file.choose())

View(data)

# To get the data type of each column

sapply(data,class)

# for data type conversion

data['mnth'] <- as.factor(data$mnth)

data['yr'] <- as.factor(data$yr)


# To find missing values

m1 <- sum(is.na(data)) # for total number of missing values
m1

m2 <- sapply(data, function (x) sum(is.na(x))) # count of missing values in each column
m2

# 1 Plot monthly distribution of the total number of bikes rented

library(ggplot2)
library(RColorBrewer)
library(dplyr)


count <- data %>% 
  group_by(mnth) %>% 
  summarise(Total_bikes = sum(cnt))

count


c1 <- ggplot(data, aes(x =mnth, y = cnt)) +
  geom_bar(stat = "identity", fill = 'black') +
  labs(title = "Monthly Distribution of Bike Rentals",
       x = "Month",
       y = "Total Number of Bikes Rented")+
  scale_y_continuous(labels = function(x) format(x, scientific = F))

c1


# 2 Plot yearly distribution of the total number of bikes rented

yr_count <- data %>% 
  group_by(yr) %>% 
  summarise(Total_bikes = sum(cnt))

yr_count

c2 <- ggplot(data, aes(x =yr, y = cnt)) +
  geom_bar(stat = "identity", fill = 'skyblue') +
  labs(title = "Yearly Distribution of Bike Rentals",
       x = "Year",
       y = "Total Number of Bikes Rented")+
  scale_y_continuous(labels = function(x) format(x, scientific = F))

c2


# 3 Plot boxplot for outliers' analysis

# Monthly Boxplot for outliers' analysis

c3 <- ggplot(data, aes(x = mnth, y = cnt)) +
  geom_boxplot(fill = 'red', color = 'black') +
  labs(title = "Monthly Boxplot for Outliers' Analysis",
       x = "Month",
       y = "Total Number of Bikes Rented")+
  theme_minimal()

c3


# Yearly Boxplot for outliers' analysis

c4 <-  ggplot(data, aes(x =yr, y = cnt))+
  geom_boxplot(fill = 'skyblue', color = 'darkblue')+
  labs(title = "Yearly Boxplot for Outliers' Analysis",
       x = "Year",
       y = "Total Number of Bikes Rented") +
  theme_minimal()

c4

# 3. Split the dataset into train and test dataset

data$cnt <- as.factor(data$cnt)

set.seed(1234)

train_ind <- sample(1:nrow(data), nrow(data)*0.75)

train_dt <- data[train_ind,]

test_dt <- data[-train_ind,]

View(train_dt)

View(test_dt)


# 4. Create a model using the random forest algorithm

sum(is.na(test_dt$cnt))

train_dt$cnt <- as.numeric(train_dt$cnt)

test_dt$cnt <- as.numeric(test_dt$cnt)

library(randomForest)

rf <- randomForest(cnt ~ ., 
                   data = train_dt, 
                   ntree = 1000, 
                   ntry = 3, 
                   nodesize = 10)

rf

plot(rf)

# 5. Predict the performance of the model on the test dataset

rf_predict <- predict(rf, test_dt)

rf_predict

