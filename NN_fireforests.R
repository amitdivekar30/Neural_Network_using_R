#PREDICT THE BURNED AREA OF FOREST FIRES WITH NEURAL NETWORKS

dataset <- read.csv(file.choose())
View(dataset)
str(dataset)
summary(dataset)
attach(dataset)

dataset$month<-as.numeric(factor(dataset$month))
dataset$day<-as.numeric(factor(dataset$day))
dataset$size_category<-factor(dataset$size_category,levels = c('small','large'),
                              labels = c(0,1))

dataset_1<- dataset[,-c(12:30)]
View(dataset_1)
summary(dataset_1)

normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
dataset_norm<-as.data.frame(lapply(dataset_1[,c(1:11)],FUN=normalize))
dataset_norm<-as.data.frame(cbind(dataset_norm,dataset_1[12]))

colnames(dataset_norm)[12]<-"size"
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset_norm$size, SplitRatio = 0.8)
training_set = subset(dataset_norm, split == TRUE)
test_set = subset(dataset_norm, split == FALSE)

attach(dataset_norm)

library(nnet) # classification 
model_1<-nnet(training_set$size~.,data = training_set, size=1)
#plot(model_1)
prob_train<-predict(model_1, data=training_set[-12])
size_pred_train<-ifelse(prob_train>0.5,1,0)
confusion<-table(training_set$size,size_pred_train)
confusion

prob_test<-predict(model_1, newdata=test_set[-12])
summary(prob_test)
size_pred_test<-ifelse(prob_test>0.5,1,0)
confusion<-table(test_set$size,size_pred_test)
confusion
