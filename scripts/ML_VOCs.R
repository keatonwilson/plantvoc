# Exploring Machine Learning Models for HIVOCs
# Keaton Wilson
# 2018-02-08
# keatonwilson@me.com


#packages we'll need
library(caret)
library(tidyverse)

#Reading in the data created in another VOC script
#Shows chemical data from GC-MS work done on a bunch of different plants that were subjected to different herbivory treatments (High, Low and Control)

ML_voc = read.csv(file = "voc_wide2")
glimpse(ML_voc)

#Column names are messy, but whatever - let's clean this up. We don't want any information on plants except the VOC data and the treatment. We also 
# want to get rid of tetralin, as it's the standard

ML_voc = as.tibble(ML_voc)

ML_voc_filtered = ML_voc %>%
  select(-(X:SampleNumber), -(Humidity:MaxTemp), -Tetralin)

#Using Caret to split into training and test sets
set.seed(42)
index = createDataPartition(ML_voc_filtered$Treatment, p = 0.75, list = FALSE)
ML_voc_train = ML_voc_filtered[index,]
ML_voc_test = ML_voc_filtered[-index,]

#Building some models
fitControl = trainControl(method = "repeatedcv", number = 10, repeats = 10)

ripper1 = train(Treatment ~ ., data = ML_voc_train,
                 method = "JRip",
                 trControl = fitControl,
                 preProcess = c("scale", "center", "nzv"))


#This is bad! #Let's try random forest

rf1 = train(Treatment ~ ., data = ML_voc_train,
                      method = "ranger",
                      trControl = fitControl,
                      preProcess = c("scale", "center", "zv"))

p_rf1 = predict(rf1, ML_voc_test)

postResample(pred = p_rf1, obs = ML_voc_test$Treatment)
table(ML_voc_test$Treatment, p_rf1)
length(ML_voc_test$Treatment)

#Ok, still not great. It's putting most things in control...

#Let's try something gnarlier. Going for a big ol' ANN model
nnfit1 = train(Treatment ~ ., data = ML_voc_train,
               method = "nnet",
               trControl = fitControl,
               preProcess = c("center", "scale", "zv"),
               maxit = 100,
               linout = 1)

nnfit1
summary(nnfit1)


p_nnfit1 = predict(nnfit1, ML_voc_test)
postResample(pred = p_nnfit1, obs = ML_voc_test$Treatment)

plot(nnfit1)

#This does things poorly too. Let's try the linear discriminant stuff
ldafit = train(Treatment ~ ., data = ML_voc_train,
               method = "lda",
               trControl = fitControl,
               preProcess = c("center", "scale", "zv"))

ldafit
summary(ldafit)

p_ldafit = predict(ldafit, ML_voc_test)
postResample(pred = p_ldafit, obs = ML_voc_test$Treatment)
