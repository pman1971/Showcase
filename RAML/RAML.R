### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

setwd("~/Documents/Showcase/RAML")
source("RAMLFunctions.R")

### LIBRARY PACKAGES ####
library.packages()

### LOAD DATA ####
mydat = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")
colnames(mydat) = c("chk_acct", "duration", "credit_his", "purpose", 
                            "amount", "saving_acct", "present_emp", "installment_rate", "sex", "other_debtor", 
                            "present_resid", "property", "age", "other_install", "housing", "n_credits", 
                            "job", "n_people", "telephone", "foreign", "resp")
### REVIEW DATA ####
dim(mydat)
str(mydat)
summary(mydat)

### PREPARE DATA ####
# Change variables to factors
mydat[sapply(mydat, is.character)] <- lapply(mydat[sapply(mydat, is.character)], 
                                       as.factor)

### DEFINE RESPONSE VARIABLE ####
summary(mydat$resp)

mydat %>% 
  count(resp) %>% 
  mutate(proportion = round(n / sum(n), 3)) %>% 
  kable()

# O is bad credit
depend.var<- as.factor(ifelse(mydat$resp== 2, "Pos", "Neg"))
levels(depend.var)
depend.var<- factor(depend.var, levels=c("Pos", "Neg"), ordered= TRUE)
levels(depend.var)
length(depend.var)

# Remove to avoid leakage
mydat$resp<- NULL

### DUMMIFY DATA FOR MODELS THAT NUMERIC MATRIX ####
dummify.data()

### PARTITION DATA ####
# Partition data into train, test and validation
partition.data(seed= 123, split= 0.5, resp= depend.var)

### REVIEW DATA SPLITS ####
review.splits()

### TRAIN MODELS ####
### Accuracy + Kappa ###
### ROC + Sens + Spec ###
### AUC (Precision-Recall AUC) + Precision + Recall + F ###
train.models(seed= 123,          
             model.hyp.par= 5,   
             folds= 10,          
             repeats= 3,                          
             classification.metric= "ROC")
end.trn-start.trn

### REVIEW MODELS ####
model.diagnostics()

# Models that have a low correlation are good candidates to be stacked in an ensemble model

# Select model to review
review.model<- model.gbm

# Review model hyper-parameters
print(review.model)
plot(review.model)
# Review model variable importance
#varImp(review.model)
summary(review.model)

### CHOOSE FINAL MODEL ####

# "RF"; "GBM"; "C5"; "DT"; "GLM"
test.models(model.type= "RF")
test.models(model.type= "GBM")
test.models(model.type= "C5")
test.models(model.type= "DT")
test.models(model.type= "GLMnet")

