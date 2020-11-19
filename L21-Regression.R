library(keras)
# Import data
mydata <- read.csv("RegressionExample1.csv")
# Plot the data
m <- nrow(mydata)
index <- 1:m
plot(index, mydata$X1, col="orange", pch=20, cex=.9)
points(index, mydata$X2, col="blue", pch=20, cex=.9)
points(index, mydata$X3, col="green", pch=20, cex=.9)
points(index, mydata$Y, col="red", type = "l",lwd=2)
#
# Split input/output and training-validation/testing
r <- 0.2 
set.seed(125)
index_test <- sample(index, floor(r*m), replace=FALSE)
my_input <- as.matrix(mydata[,1:3])
my_output <- as.matrix(mydata[,4])
input_test <- my_input[index_test,]
output_test <- my_output[index_test]
input_train <- my_input[-index_test,]
output_train <- my_output[-index_test]
# Build Keras model
# Set seed for reproducibility
seed <- 52
tensorflow::tf$random$set_seed(seed)
# Set NN model
mymodel <- keras_model_sequential()
# Architecture
mymodel %>% 
  layer_dense(units=2, activation="relu", input_shape=3) %>% 
  layer_dense(units=3, activation = "relu") %>% 
  layer_dense(units=1, activation="linear")
# Compilation Info
mymodel %>% compile(
  loss = "mse",
  optimizer =  "adam", 
  metrics = c("mean_absolute_error")
)
# Training the model
mymodel_history <- mymodel %>% fit(
  input_train,
  output_train,
  epochs = 50,
  batch_size = 96,
  validation_split=0.2,
  verbose=1
)
# 
scores <- mymodel %>% evaluate(
 input_test, output_test, verbose = 0
)
# Output metrics
cat("Test mean absolute error:", scores[[2]], "\n")
#
# Apply the model on new inputs
new_data <- data.frame(X1=c(14.7, 17.5), X2=c(5.8, 10.8), X3=c(7.2, 20.1))
new_input <- as.matrix(new_data)
pred_new_output <- predict(mymodel, new_input)
cat("New outputs:\n",pred_new_output,"\n")
# Evaluate the model on testing data
pred_output_test <- predict(mymodel, input_test)
metric <- mean(abs(pred_output_test-output_test))
cat("Metric on testing data:",metric, '\n')
#
# Create Comparison DataFrame
compare <- data.frame(ActualOutput=output_test, 
                      ModelPredictedOutput=pred_output_test)
print(compare[1:25,])
                            


  

