# Prepare a model for strength of dataset data using Neural Networks

dataset <- read.csv(file.choose())
View(dataset)
str(dataset)
attach(dataset)
#normal_dataset<-scale(dataset)
## or 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
dataset_norm<-as.data.frame(lapply(dataset[,-9],FUN=normalize))
#summary(dataset_norm$strength)
#summary(normal_dataset)
summary(dataset$strength)

dataset_norm <- cbind(dataset_norm,dataset$strength)
colnames(dataset_norm)[9] <- "strength"
dataset_train<-dataset_norm[1:773,]
dataset_test<-dataset_norm[774:1030,]

# Using multilayered feed forward nueral network
# package nueralnet
#install.packages("neuralnet")
#install.packages("nnet")

library(neuralnet)  # regression
library(nnet) # classification 

# Building model
#formula_nn <- paste("strength",paste(colnames(dataset[-9]),collapse ="+"),sep="~")
dataset_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,
                           data = dataset_train)
#dataset_model <- neuralnet(formula = formula_nn,data = dataset_train)
str(dataset_model)
plot(dataset_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(dataset_model,dataset_test[1:8])
str(model_results)
predicted_strength <- model_results$net.result
# predicted_strength
# model_results$neurons
cor(predicted_strength,dataset_test$strength)
plot(predicted_strength,dataset_test$strength)


model_5<-neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age,
                   data= dataset_norm,hidden = 5)
plot(model_5)
model_5_res<-compute(model_5,dataset_test[1:8])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,dataset_test$strength)
plot(pred_strn_5,dataset_test$strength)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased

