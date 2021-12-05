### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

# Library packages
library(ranger)
library(pROC)

# Create matrix of random numbers
features.no<- 10000
n<- 1000

# Set seed
set.seed(111)
vals= runif(features.no * n)
my.matrix<- matrix(vals, nrow = n, ncol = features.no)

# Randomise matrix with numbers between 1 and 1000
myDat<- as.data.frame(my.matrix)

# Assign random target variable
myDat$resp<- sample(c(1, 0), size= n, replace= TRUE, prob= c(0.5, 0.5))

# Review target variable
tbl<- prop.table(table(myDat$resp))
bpl1<- barplot(tbl, main= "Proportion of class labels")
text(bpl1, 0, tbl, cex= 1, pos= 3)

# Pick the best features based on the correlation to the response
topFeatures= 30
correlations<- apply( myDat[,-which(names(myDat) == "resp")] , 2 , cor , y = myDat$resp)
selectedFeatures<- order(correlations, decreasing = TRUE)[1:topFeatures]
featureNames<- names(correlations[selectedFeatures])

# Split data into train and test
set.seed(333)
x <- c(TRUE, FALSE)
splitID= sample(x, nrow(myDat) ,replace = TRUE, prob=c(.75, .25))
trainDat= myDat[splitID, c(featureNames, 'resp')]
testDat= myDat[!splitID, c(featureNames, 'resp')]

# Fit random forest 
rfFit <- ranger(as.factor(resp)~ .,   
                data= trainDat,
                probability = TRUE,
                num.trees= 125,
                mtry= floor(sqrt(ncol(trainDat))))

rfProbs= predict(rfFit, data = testDat)$predictions

# Get model performance on test set
roc(testDat$resp, rfProbs[,2],
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)

# Observe there is a good model fit!
# This is because feature selection occured before partitioning the data and data leakage has occured
# Repeat above process AFTER partioning

# Split data into train and test
trainDat= myDat[splitID, ]
testDat= myDat[!splitID, ]

# Pick the best features based on the correlation to the response
correlations<- apply(trainDat[,-which(names(trainDat) == "resp")] , 2 , cor , y = trainDat$resp)
selectedFeatures<- order(correlations, decreasing = TRUE)[1:topFeatures]
featureNames<- names(correlations[selectedFeatures])

# Train only on the top 30 features
trainDat= trainDat[, c(featureNames, 'resp')]
testDat= testDat[, c(featureNames, 'resp')]

# Fit random forest 
rfFit <- ranger(as.factor(resp)~ .,   
                data= trainDat,
                probability = TRUE,
                num.trees= 125,
                mtry= floor(sqrt(ncol(trainDat))))

rfProbs= predict(rfFit, data = testDat)$predictions

# Get model performance on test set
roc(testDat$resp, rfProbs[,2],
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)

# As expected the model AUC is around 0.5
