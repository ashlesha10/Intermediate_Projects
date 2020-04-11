library(rpart)
library(corrplot)
library(rpart.plot)
library(glmnet)
library(caret)

data <- read.csv('housing.csv')
nrow(data)

colSums(is.na(data)) #checking the null values

data <- na.omit(data) # removing null values
rownames(data) <- 1:nrow(data)
nrow(data)

head(data)

str(data) # structure of data

# removing the columns that are not useful for prediction 
data$longitude <- NULL
data$latitude <- NULL
data$ocean_proximity <- NULL

str(data)

summary(data)

scaled_data <- as.data.frame(scale(data))

#correlation plot 
numeric_var <- which(sapply(scaled_data, is.numeric))
num_data <- scaled_data[, numeric_var]
correlation <- cor(num_data)
options(repr.plot.width = 5, repr.plot.height = 5)
corrplot(correlation, method = 'number')


hist(data$median_house_value)

set.seed(42)

data_split <- sample(3, nrow(scaled_data), replace = T, prob = c(0.8,0.1,0.1))
train_data <- scaled_data[data_split == 1,]
val_data <- scaled_data[data_split == 2,]
test_data <- scaled_data[data_split == 3,]

nrow(train_data)

nrow(val_data)

nrow(test_data)

custom_parameter <- trainControl(method = 'repeatedcv',
                                number = 10,
                                repeats = 5,
                                verboseIter = T)

set.seed(42)
lasso_reg <- train(median_house_value ~ ., train_data, method = 'glmnet', 
                   tuneGrid = expand.grid(alpha = 1, lambda = seq(0,0.001,length = 10)), 
                   trControl = custom_parameter)

options(repr.plot.width = 5, repr.plot.height = 3)
plot(lasso_reg)

options(repr.plot.width = 7, repr.plot.height = 4)
plot(varImp(lasso_reg))

tree <- rpart(median_house_value ~ median_income + total_bedrooms + total_rooms + households + population, 
              train_data, control = rpart.control(minsplit = 200, maxdepth = 30, cp = 0.024))

options(repr.plot.width = 8, repr.plot.height = 5)
rpart.plot(tree,extra = 1)

summary(tree)

tree$variable.importance

plotcp(tree)

val_pred = predict(tree, val_data)
tree.sse = sum((val_pred - val_data$median_house_value)^2)
rmse = sqrt(tree.sse)
rmse

test_pred = predict(tree, test_data)
tree.sse1 = sum((test_pred - test_data$median_house_value)^2)
rmse1 = sqrt(tree.sse1)
rmse1


