# Marketing 653

# Set working directory and import data
setwd("C:\\Users\\Darrell\\Desktop\\Syracuse\\Spring_19\\MAR_653_Marketing_Analytics\\Project\\world-happiness-report")
happydata_15 <- read.csv("2015.csv", na.string=c(""))
happydata_17 <- read.csv("2017_Clean.csv", na.string=c(""))

catdata <- read.csv("mar_proj_data_2.csv", na.string=c(""))

require(neuralnet)

happy_NN <- neuralnet(Happiness.Score~ Economy..GDP.per.Capita. + Family + Health..Life.Expectancy. + Freedom + Trust..Government.Corruption. + Generosity, hidden = 4,
                      data = happydata_15, lifesign = "minimal", linear.output = FALSE, threshold = 0.1)
catNN <- neuralnet(Label~ Economy..GDP.per.Capita. + Family + Health..Life.Expectancy. + Freedom + Trust..Government.Corruption. + Generosity, hidden = 4,
                   data = catdata, lifesign = "minimal", linear.output = FALSE, threshold = 0.1)

plot(happy_NN, rep = "best")
predicted2 = predict(happy_NN, happydata_17)
(head(predicted2,n=10))

plot(catNN, rep = "best")
predicted3 = predict(catNN, catdata, type = "class")
(head(predicted3,n=10))
predicted3 <- data.frame(predicted3)
predicted3[4] <- catdata$Label
View(predicted3)



################################
## Test 1

require(devtools)

#import 'gar.fun' from beckmw's Github - this is Garson's algorithm
source_gist('6206737')

#use the function on the model created above
gar.fun('Happiness.Score',happy_NN)
gar.fun('Happiness.Score', catNN)



################################
## Test 2
require(caret)

# varimp
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model <- train(Happiness.Score~ Economy..GDP.per.Capita. + Family + Health..Life.Expectancy. + Freedom + Trust..Government.Corruption. + Generosity, hidden = 4,
               data = happydata_15, method="nnet",trControl=control)
imp<-varImp(model)
plot(imp)


# varimp
control <- trainControl(method="repeatedcv", number=10, repeats=3)
model_1 <- train(Label~ Economy..GDP.per.Capita. + Family + Health..Life.Expectancy. + Freedom + Trust..Government.Corruption. + Generosity, hidden = 4,
               data = catdata, method="nnet",trControl=control)
imp_1<-varImp(model_1)
grp <- data.frame(imp_1$importance)
plot(grp$Overall)

