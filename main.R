library(keras)
library(caret)

#Import clean data
fifa_data <- subset(read.csv('CleanData.csv'), select = -c(X.1))
outputs <- subset(read.csv('Outputs.csv'), select = -c(X))

#Normalize Data
preprocess_fifa <- preProcess(fifa_data, method=c("range"))
preprocess_output <- preProcess(outputs, method = c('range'))
fifa_data <- predict(preprocess_fifa, fifa_data)
outputs <- predict(preprocess_output, outputs)


#Separate into training and testing data
index <- 1:nrow(fifa_data)
ratio <- 0.2
set.seed(12)
index_test <- sample(index, floor(ratio*nrow(fifa_data)), replace = FALSE)
input_train <- as.matrix(fifa_data[-index_test,])
input_test <- as.matrix(fifa_data[index_test,])
output_train <- outputs[-index_test,1]
output_test <- outputs[index_test,1]

# Build Keras model
tensorflow::tf$random$set_seed(12)
fifamodel <- keras_model_sequential()

# Architecture
fifamodel %>%
  layer_dense(units = 37, activation = 'relu', input_shape = 56) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 24, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  #layer_dense(units = 16, activation = 'relu') %>%
  #layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = 'relu')

# Compilation Info
fifamodel %>% compile(
  loss = 'mse',
  optimizer = 'adam',
  metrics = c('mean_absolute_error')
)

# Training the model
fifa_history <- fifamodel %>% fit(
  input_train,
  output_train,
  epochs = 100,
  batch_size = 1800,
  validation_split = 0.2
)

training_scores <- fifamodel %>% evaluate(input_train, output_train, verbose = 0)
cat('\nTraining and Validation MAE: ', training_scores[2], '\n')
test_scores <- fifamodel %>% evaluate(input_test, output_test, verbose = 0)
cat('Testing MAE: ', test_scores[2], '\n')

