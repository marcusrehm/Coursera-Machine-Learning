library('data.table')
library(caret)
# enable multi-core processing
library(doParallel)

training <- read.csv(file = 'pml-training.csv', header = TRUE, sep = ',', na.strings=c("NA","","#DIV/0!"))
testing <- read.csv(file = 'pml-testing.csv', header = TRUE, sep = ',', na.strings=c("NA","","#DIV/0!"))

features <- which(sapply(X = names(training), FUN = grepl, pattern = '^total_accel|gyros|classe'))
training <- training[, features]
testing <- testing[, features]

#levels(training$classe) <- sub('B|C|D|E', 'wrong', levels(training$classe))
#levels(training$classe) <- sub('A', 'right', levels(training$classe))
#training$classe <- relevel(training$classe, 'wrong')

index <- createDataPartition(y = training$classe, p = 0.7, list = FALSE)
validation <- training[-index,]
training <- training[index,]

cl <- makeCluster(detectCores())
registerDoParallel(cl)
set.seed(32343)
modelFit <- train(classe ~ ., data = training, method = 'rf')
stopCluster(cl)

validation$prediction <- predict(modelFit, validation)
confusionMatrix(validation$prediction, validation$classe)

#save(modelFit, file = 'modelFit.RData')
