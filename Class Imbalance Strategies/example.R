### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

source('modelPerfFunctions.R')

###
### THIS EXAMPLE WILL DEMONSTRATE THE FOLLOWING THINGS:
### 1. CLASSIFCATION MACHINE LEARNERS STRUGGLE WITH IMBALANCED DATASETS
### 2. TYPICAL UNDERSAMPLING METHODS PRODUCES UNCALIBRATED PROBABILITIES
### 3. POSSBILE SOLUTIONS AND HOW TO MEASURE EFFECTIVENESS
###


library(ranger)
library(caret)
library(ModelMetrics)
library(knitr)
library(lattice)

myDat= read.csv("UCI_Credit_Card.csv")

# Change response variable name
colnames(myDat)[which(names(myDat) == "Resp")] <- "resp"
myDat$resp= as.factor(myDat$resp)

# Review highly imbalanced dataset indicating fraud
myDat %>% 
  count(resp) %>% 
  mutate(proportion = round(n / sum(n), 3)) %>% 
  kable()

# Remove positive responses so there is only a 1% prevalence
posResponses= subset(myDat, myDat$resp== '1')
posResponsesSamp= sample_n(posResponses, 2500)

myDat= rbind(subset(myDat, myDat$resp== '0'),
             posResponsesSamp)

myDat %>% 
  count(resp) %>% 
  mutate(proportion = round(n / sum(n), 3)) %>% 
  kable()

# Divide data into train and test
index= createDataPartition(myDat$resp, p= 0.7, list = F)

trainDat= myDat[index,]
testDat= myDat[-index,]

# Fit RF on train data
rfFit <- ranger(resp~ .,   
                data= trainDat,
                probability = TRUE,
                num.trees= 125,
                mtry= floor(sqrt(ncol(trainDat))))

# Get prediction on test data
rfProbs= predict(rfFit, data = testDat)$predictions
# Get model performance on test set
roc(testDat$resp, rfProbs[,2],
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)

calibrationPlot(dataFrame= testDat,
                respColumn= 'resp',
                respLabel= '1', 
                predictions= rfProbs[,2], 
                percentilesNo= 10)

### ATTEMPT UNDERSAMPLING TO IMPROVE PREDICTIONS

# Count responses
table(trainDat$resp)

# Sample so that there are 4 majority records for every minority
noNegRespRequired= table(trainDat$resp)[2] * 4

negResponsesOrg= subset(trainDat, trainDat$resp== '0')
negResponsesUnder= sample_n(negResponsesOrg, noNegRespRequired)

posResponses= subset(trainDat, trainDat$resp== '1')

trainDatUnder= rbind(negResponsesUnder, posResponses)
table(trainDatUnder$resp)

# Fit RF on train data
rfFit <- ranger(resp~ .,   
                data= trainDatUnder,
                probability = TRUE,
                num.trees= 125,
                mtry= floor(sqrt(ncol(trainDat))))

# Get prediction on test data
rfProbs= predict(rfFit, data = testDat)$predictions
# Get model performance on test set
roc(testDat$resp, rfProbs[,2],
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)

calibrationPlot(dataFrame= testDat,
                respColumn= 'resp',
                respLabel= '1', 
                predictions= rfProbs[,2], 
                percentilesNo= 10)

# The calibration plot demonstrates that the posterior probabilities are too high

# Using equation 4 from Dal Pozzolo Calibrating Probability with Undersampling for Unbalanced Classification
#system2('open', args = 'dalpozzolo2015calibrating.pdf', wait = FALSE)

beta= nrow(negResponsesUnder)/ nrow(negResponsesOrg)
predsCal= beta * rfProbs[,2] / ((beta-1) * rfProbs[,2] + 1)

rfProbs= cbind(rfProbs, predsCal)

# Get model performance on test set
roc(testDat$resp, rfProbs[,3],
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)

calibrationPlot(dataFrame= testDat,
                respColumn= 'resp',
                respLabel= '1', 
                predictions= rfProbs[,3], 
                percentilesNo= 10)
