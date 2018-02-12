# Exploring Machine Learning Models for HIVOCs
# Keaton Wilson
# 2018-02-08
# keatonwilson@me.com


#packages we'll need
library(caret)
library(tidyverse)

#Reading in the data created in another VOC script
#Shows chemical data from GC-MS work done on a bunch of different plants that were subjected to different herbivory treatments (High, Low and Control)

ML_voc = read.csv(file = "/Users/KeatonWilson/Documents/Projects/plantvoc/data/wide2.csv")
glimpse(ML_voc)

#Column names are messy, but whatever - let's clean this up. We don't want any information on plants except the VOC data and the treatment. We also 
# want to get rid of tetralin, as it's the standard

ML_voc = as.tibble(ML_voc)

ML_voc_filtered = ML_voc %>%
  select(-(X:SampleNumber), -(Humidity:MaxTemp))

#Ok, let's do the Aitchison Transformation first. 
#Normalizing across rows for all data
#Building the normalizing function
normalize <- function(x) { 
  x <- sweep(x, 1, apply(x, 1, min)) 
  sweep(x, 1, apply(x, 1, max), "/") 
} 
#Applying the normalize function
normwide = normalize(ML_voc_filtered[,-1])
normwide[normwide == "0"] = 0.00000001

#Building the Aitchison Function
aitchison1 = function(x){
  x = log(x/normwide$Tetralin)
}

aitchison_ML = as.tibble(apply(normwide, 2, aitchison1))
aitchison_ML$Treatment = ML_voc_filtered$Treatment

#Using Caret to split into training and test sets
set.seed(42)
index = createDataPartition(aitchison_ML$Treatment, p = 0.6, list = FALSE)
ML_voc_train = aitchison_ML[index,]
ML_voc_test = aitchison_ML[-index,]

#Building some models - Removed center and scale in preprocessing because data is already centered and scaled. 
fitControl = trainControl(method = "repeatedcv", number = 10, repeats = 10)

ripper1 = train(Treatment ~ ., data = ML_voc_train,
                 method = "JRip",
                 trControl = fitControl,
                 preProcess = c("zv"))

p_rip = predict(ripper1, ML_voc_test)

postResample(pred = p_rip, obs = ML_voc_test$Treatment)


#This is bad! #Let's try random forest

rf1 = train(Treatment ~ ., data = ML_voc_train,
                      method = "ranger",
                      trControl = fitControl,
                      preProcess = c("zv"))

p_rf1 = predict(rf1, ML_voc_test)

postResample(pred = p_rf1, obs = ML_voc_test$Treatment)
table(ML_voc_test$Treatment, p_rf1)
length(ML_voc_test$Treatment)

#Ok, still not great. It's putting most things in control... But it's doing things right about 51% of the time. 

#Let's try something gnarlier. Going for a big ol' ANN model
nnfit1 = train(Treatment ~ ., data = ML_voc_train,
               method = "nnet",
               trControl = fitControl,
               preProcess = c("nzv"),
               maxit = 100,
               linout = 1, 
               tuneLength = 10)

nnfit1
summary(nnfit1)


p_nnfit1 = predict(nnfit1, ML_voc_test)
postResample(pred = p_nnfit1, obs = ML_voc_test$Treatment)

ploplot(nnfit1)

#This does things poorly too. Let's try the linear discriminant stuff
ldafit = train(Treatment ~ ., data = ML_voc_train,
               method = "lda",
               trControl = fitControl,
               preProcess = c("zv"))

ldafit
summary(ldafit)

p_ldafit = predict(ldafit, ML_voc_test)
postResample(pred = p_ldafit, obs = ML_voc_test$Treatment)


#Naive Bayes
#installing package
#install.packages("klaR")
library(klaR)
nbfit = train(Treatment ~ ., data = ML_voc_train,
               method = "nb",
               trControl = fitControl,
               preProcess = c("zv"))
