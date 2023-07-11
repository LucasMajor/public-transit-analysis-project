library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(caret)
library(ROCR)

transit = read.csv("Transit1.csv")

##Remove outliers
transit = transit[transit$Distance > 100 | transit$Score > 30,]
transit = transit2[transit2$Distance < 750 | transit2$Score < 30,]

##Plot relationship between Distance and actual score
ggplot(transit2,aes(x=Distance, y=Score)) + geom_point()

##Initial model
model1 = lm(transit$Score~Distance+Num+Bus+Lightrail+HighFreq+SALE_VALUE, data = transit)
summary(model1)

#Calculate normalized RMSE
x=predict(model1)
RMSE = sqrt(mean((transit$Score-x)^2))/(85-22)


##Model with 0 for SALE_VALUE filtered out, to see if it gains significance
Filter = read.csv("Transit1.csv")
Filter = Filter[Filter$SALE_VALUE!=0,]
model2 = lm(Score~Distance+Num+Bus+Lightrail+HighFreq+SALE_VALUE, data = Filter)
summary(model2)


##Final Linear model with insignificant independent variables removed
model3 = lm(Score~Distance+Num+HighFreq, data = transit)
summary(model3)

#Calculate normalized RMSE
x=predict(model3)
RMSE = sqrt(mean((transit$Score-x)^2))/(85-22)

#Plot predicted vs. actual score
xaxis=predict(model3,transit)
plot1 = ggplot(transit, aes(x=xaxis,y=Score)) + geom_point()
plot1 + ggtitle("Predicted vs. Actual Score")+xlab("Predicted")+ylab("Actual")
       
#################################################################################

##Polynomial Model
model4 = lm(Score ~ poly(Num,3 ,raw=TRUE)+poly(Distance, 1,raw=TRUE) + HighFreq + Lightrail, data=transit)
summary(model4)
x = predict(model4)
RMSE = sqrt(mean((transit$Score-x)^2)) / (85-22)
RMSE

xaxis=predict(model4,transit)
plot2 = ggplot(transit, aes(x=xaxis,y=Score)) + geom_point()
plot2 + ggtitle("Predicted vs. Actual Score")+xlab("Predicted")+ylab("Actual")


## Test to see if model4 R^2 is significantly better than model 3, the linear model
anova(model3, model4)




