### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

# Library packages
library(caret)
library(pROC)
library(doParallel)	
library(pROC)
library(dplyr)

# Load data
### LOAD DATA ####

# Amend code to load data
mydat = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
colnames(mydat) = c("chk_acct", "duration", "credit_his", "purpose", 
                    "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                    "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                    "job", "n_people", "telephone", "foreign", "resp")

### PREPARE DATA ####
# Change variables to factors
mydat[sapply(mydat, is.character)] <- lapply(mydat[sapply(mydat, is.character)], 
                                             as.factor)

# 2 is bad credit
mydat$resp<- as.factor(ifelse(mydat$resp== 2, "Pos", "Neg"))
mydat$resp<- factor(mydat$resp, levels=c("Pos", "Neg"), ordered= TRUE)

# Set up to do parallel processing   
detectCores()
usecores= detectCores() - 2
registerDoParallel(usecores)		# Registrer a parallel backend for train
getDoParWorkers()

# Fit RF using caret
# Custom grid
tgrid<- expand.grid(
  .mtry = round(sqrt(ncol(mydat))),
  .splitrule = "gini",
  .min.node.size = 10
)

control<- trainControl(classProbs=TRUE, summaryFunction= twoClassSummary)

# Test tree size
split= 0.80
trainIndex <- createDataPartition(mydat$resp, p= split, list=FALSE)
train<- mydat[ trainIndex,]
test<- mydat[-trainIndex,]

trees.list= c(1, 2, 4, 6, 8, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 125, 150, 175, 200, 250, 500, 750, 1000)
test.ROC.results= vector()

for(treeno in trees.list)
{
  print(treeno)
  model<- train(resp ~., data = train, 
                method = "ranger",
                trControl = control,
                tuneGrid = tgrid,
                num.trees = treeno,
                metric="ROC")
  
  preds= predict(model, newdata= test, type= 'prob')
  test.ROC<- roc(predictor= preds$Pos,
                 response= test$resp)
  test.ROC.results= c(test.ROC.results, test.ROC$auc)
}

plot(test.ROC.results~ trees.list, type= "b",
     main= "AUC by tree no",
     xlab= 'Tree no',
     ylab= "AUC")

# Add line denoting maximum AUC across all trees
abline(h= max(test.ROC.results), col= "red")

# Demonstrate how RF calculate probabilities

# Fit RF with 100 trees

library(randomForest)

treeNo= 100
rf = randomForest(resp~., data = train, norm.votes = TRUE, proximity = TRUE,
                  ntree= treeNo)
predProb= predict(rf, test, type = "prob")

# Return number of trees that vote for Pos
predVotes= as.data.frame(predict(rf, test, type = "vote", norm.votes = F))

# Divide by total no of trees
predProbVote= predVotes$Pos/treeNo

# Compare preda
identical(as.vector(predProb[,1]), predProbVote)

