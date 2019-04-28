---
title: 'Practical Machine Learning Project: Quantifying the good performance
  of physical activity'
author: "Alberto Vicens"
date: "27/4/2019"
output: html_document
---

```{r setup, incñude = FALSE}
knitr::opts_chunk$set(echo = TRUE, eval = TRUE, cache = TRUE, autodep = TRUE, cache.path = "cache/", fig.path = "figures/", warning = FALSE)
```

##Introduction
One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, I will predict if barbell lifts are done correctly and incorrectly. using data from several accelerometers in 6 participants.

```{r load_libraries, include=FALSE}
library(caret)
```

##Getting data
To built and test a predictive model, I will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. This data are freely accesible in the webpage of [Human activity recognition](http://groupware.les.inf.puc-rio.br/har). I download the training and test datasets there are provided.
```{r download_data}
urlTrain<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
urlTest<-"http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

if(!file.exists("pml-training.csv")){
  download.file(urTrain, destfile ="pml-training.csv")
}

if(!file.exists("pml-testing.csv")){
  download.file(urTest, destfile ="pml-testing.csv")
}
```

```{r load_datasets}
har<-read.csv("pml-training.csv", header = T)
quiz<-read.csv("pml-testing.csv", header = T)
```

##Cleaning data
I noticed that multiple columns contain a lot of missing data and these are informative. I thus filtered those columns containing > 95% of missing data.

```{r remove_NAcolumns}
#Getting the percentage of missing values per colum in training dataset
NApercTrain <- sapply(har, function(df) {sum(is.na(df)==TRUE)/length(df)})

#Removing columns with >95% missing values
har<-har[,NApercTrain < 0.95] 
quiz<-quiz[,NApercTrain < 0.95] 

sum(is.na(har)==TRUE)
```

I also removed columns with varianze near zero, as these variable provide little information for model building.
```{r remove_lowVar}
nzv<-nearZeroVar(har)
har<-har[,-nzv]
quiz<-quiz[,-nzv]
```

Lastly, I took off the "X", "Individual" and "timestamp" columns for being uninformative.
```{r remove_factors}
har<-har[,-(1:6)]
quiz<-quiz[,-(1:6)]
rbind(har=dim(har), quiz = dim(quiz))
```

```{r data_partition}
library(caret)
set.seed(360)
inTrain<-createDataPartition(y=har$classe,p = 0.6, list = FALSE)
training<-har[inTrain,]
testing<-har[-inTrain,]
rbind(Training=dim(training), Testing=dim(testing))
```

##Building predictive models
I built predictive models using *Random Forest* and *Boosting* methods. I decided to apply these methods because they are the most widely used methods in machine learning and because they fit relatively well, with very little tunning required.

###Random forest
```{r rf_model}
set.seed(1211)
#Train control by cross validation
controlRF<-trainControl(method = "cv", number = 3, verboseIter = FALSE)

set.seed(3232)
#Model building
rfModFit<-train(classe ~., data = training, method = "rf", trControl = controlRF, verbose = FALSE)

rfModFit$finalModel
```

I then evaluate the model with the test dataset to estimate the accuracy.
```{r outframe_error_rf}
rfTestPred<-predict(rfModFit, testing)

rfTestCM<-confusionMatrix(testing$classe,rfTestPred)
rfTestCM$overall
```
```{r rf_plot1}
plot(rfModFit, main ="Accuracy of Random forest by number of predictors")
```
```{r rf_plot2}
plot(rfTestCM$table, main = "Random Forest Accuracy by Categories")
```

###Generalized Boosting

```{r boosting}
set.seed(4343)
controlGBM<-trainControl(method="repeatedcv",number=5, repeats=1)
bstModFit<-train(classe ~ ., training, method = "gbm", verbose = FALSE,
                 trControl = controlGBM)
bstModFit$finalModel
```

```{r bst_plot}
plot(bstModFit)
```

```{r predict_bst}
bstTestPredict<-predict(bstModFit,testing)
bstTestCM<-confusionMatrix(bstTestPredict, testing$classe)
bstTestCM$overall
```

```{r bst_CM_plot}
plot(bstTestCM$table, main = "Accuracy of Generalized Boosting Model")
```

##Choosing best model and validation test
I choose the Random Forest model because is that with the best accurracy (accuracy RF: 0.991; accuracy GBM: 0.961). I then test this model with the vadilation dataset.

```{r valid_test}
bstValidPred<- predict(bstModFit, quiz)
quizOut<-cbind(Question=seq(1:20), Response = as.character(bstValidPred))
quizOut
```

**Note**: The dataset used in this project is a courtesy of “Ugulino, W.; Cardador, D.; Vega, K.; Velloso, E.; Milidiu, R.; Fuks, H. Wearable Computing: Accelerometers’ Data Classification of Body Postures and Movements”