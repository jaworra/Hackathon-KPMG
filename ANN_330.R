
#gtfs = read.csv("D:\\RealTime GTFS Filtered by Route Points 10m_Ordered_Cleaned_1.csv")

gtfs = read.csv("D:\\FilteredGTFS.csv")

#Data Investigation
str(gtfs)
summary(gtfs)

#test Set
indexes = sample(1:nrow(gtfs), size=0.2*nrow(gtfs))

# Train-test split
gtfs.test <- gtfs[indexes,]
gtfs.train<- gtfs[-indexes,]

#Install libraries
#install.packages('neuralnet')
#install.packages('compute')
library('neuralnet')

# Let's start by building the simplest possible multilayer feedforward network with only one single hidden node
gtfs.model = neuralnet(data=gtfs.train, Chainage ~ T.1 + T.2 + T.3)
plot(gtfs.model)# Let's see the coeffs:

save(gtfs.model, file = "D:\\jaworra\\my_model1.rda") #Save Model 
load( file = "D:\\jaworra\\my_model1.rda") #Load Model

#TEST
predict.full <- prediction(gtfs.model,gtfs.test)


#train and test on 50 points
gtfs.model.2 = neuralnet(data=gtfs.train, Chainage ~ T.1 + T.2 + T.3)
predict.full <- prediction(gtfs.model.2,gtfs.test ,  type = "class")
summary(predict.full)


#sample dataset
gtfs.train <- gtfs[1:800,]
gtfs.test <- gtfs[1:200,]

#Sample Run NN Run
library(nnet)
nn1 <- nnet(Chainage ~ T.1 + T.2 + T.3, data = gtfs.train, size = 40, maxit = 500)
nn1.pred <- predict(nn1, gtfs.test, type="raw")
summary(nn1.pred)
tb1 <- table(nn1.pred, gtfs.test$Chainage)
tb1
nn1.pred[120]
gtfs.test$Chainage[120]

summary(gtfs.test$Chainage)
summary(nn1.pred)
gtfs.test$estimate <- predict(nn1, gtfs.test)


#Sample linear Run
lm1 <- lm(Chainage ~ T.1 + T.2 + T.3, data =  gtfs.train)
lm1.pred <- predict(lm1, gtfs.test , type="response")
summary(lm1)
gtfs.test$lm1.estimate <- predict(lm1, gtfs.test , type="response")
tb.lm1 <- table(lm1.pred,gtfs.test$Chainage)
tb.lm1
lm1.pred[12]
gtfs.test$Chainage[12]


#Generalise Run
glm1 <- glm(formula = Chainage ~ T.1 + T.2 + T.3 ,data =  gtfs.train, family = binomial)


#Another NN
#Sample Run NN Run
library(car)
library(caret)
library(lattice)
Chain.fit <- train(Chainage ~ T.1 + T.2 + T.3 + TimePeriod ,data = gtfs.train,method = "nnet", maxit = 1000, trace = F, linout = 1)  
Chain.predict <- predict(Chain.fit, newdata = gtfs.test)

Chain.rmse <- sqrt(mean((Chain.predict - gtfs.test$Chainage)^2))
summary(Chain.predict)
gtfs.test$NN_Prediction <- Chain.predict

write.csv(gtfs.test, file = "G:\\Geospatial\\GIS\\JohnW\\Hackathon\\Results.csv")
