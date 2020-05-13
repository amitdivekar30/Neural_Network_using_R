# Build a Neural Network model for 50_startups data to predict profit 

dataset <- read.csv(file.choose())
View(dataset)
str(dataset)
attach(dataset)

#Encoding Categorical Data
dataset$State<-factor(dataset$State,
                      levels = c('New York','California','Florida'),
                      labels = c(0, 1, 2))
dataset$State<-as.numeric(dataset$State)
View(dataset)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
dataset_norm<-as.data.frame(lapply(dataset[,c(1:5)],FUN=normalize))


#dataset_norm <- cbind(dataset_norm,dataset$Profit)
#colnames(dataset_norm)[4] <- "State"
#colnames(dataset_norm)[5] <- "Profit"


#spilitting dataset into train and test
dataset_train<-dataset_norm[1:35,]
dataset_test<-dataset_norm[35:50,]

attach(dataset_train)
# Using multilayered feed forward nueral network
# package nueralnet
#install.packages("neuralnet")
#install.packages("nnet")

library(neuralnet)  # regression
library(nnet) # classification 

# Building model
dataset_model <- neuralnet(Profit~R.D.Spend+Administration+Marketing.Spend+State,
                           data = dataset_train)
#dataset_model <- neuralnet(formula = formula_nn,data = dataset_train)

summary(dataset_model)
str(dataset_model)
plot(dataset_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(dataset_model,dataset_test[1:4])
str(model_results)
predicted_Profit <- model_results$net.result
# predicted_Profit
# model_results$neurons
cor(predicted_Profit,dataset_test$Profit)
plot(predicted_Profit,dataset_test$Profit)

#unnormalizing profit again
str_max <- max(dataset$Profit)
str_min <- min(dataset$Profit)

unnormalize <- function(x, min, max) { 
  return( (max - min)*x + min )
}

ActualProfit_pred <- unnormalize(predicted_Profit,str_min,str_max)
head(ActualProfit_pred)

model_2<-neuralnet(Profit~.,data= dataset_train,hidden = 2)
plot(model_2)
model_2_res<-compute(model_2,dataset_test[1:4])
pred_strn_2<-model_2_res$net.result
cor(pred_strn_2,dataset_test$Profit)
plot(pred_strn_2,dataset_test$Profit)

ActualProfit_pred_2 <- unnormalize(pred_strn_2,str_min,str_max)
head(ActualProfit_pred)
