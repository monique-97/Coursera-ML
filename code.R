library(caret)
library(randomForest)
library(kernlab)
library(rpart.plot)

train_data <- read.csv("pml-training.csv")
test_data <- read.csv("pml-testing.csv")
dim(train_data)
dim(test_data)

train_data <- train_data[, colSums(is.na(train_data)) == 0] 
test_data <- test_data[, colSums(is.na(test_data)) == 0] 

##########################


training <- train_data[,-c(1:7)]
testing <-test_data[,-c(1:7)]


classe <- train_data$classe
train_clean <- train_data[, sapply(train_data, is.numeric)]
train_clean$classe <- classe

test_clean <- test_data[, sapply(test_data, is.numeric)]

##########################



set.seed(199997)
training_part <- createDataPartition(train_clean$classe, p=0.65, list=FALSE)
train_values <- train_clean[training_part, ]
test_values <- train_clean[-training_part, ]

################
barplot(table(train_values$classe), col="blue", main="Frequency of each level", xlab=" ", ylab="frequency")

######################

controltr <- trainControl(method="cv", 5)
model_random <- train(classe ~ ., data=train_values, method="rf", trControl=controltr, ntree=100)
model_random

##################

prediction <- predict(model_random, test_values)
confusionMatrix(test_values$classe, prediction)


################3

acc <- postResample(prediction, test_values$classe)
acc
error <- 1 - as.numeric(confusionMatrix(test_values$classe, prediction)$overall[1])
error

#############
final_result <- predict(model_random, test_clean[, -length(names(test_clean))])
final_result







