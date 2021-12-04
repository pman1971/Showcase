### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

# Set seed
set.seed(111)

# Library packages
library(progress)
library(ranger)
library(pROC)

# Create matrix of random numbers
features.no<- 1000
n<- 10000
my.matrix<- matrix(0, n, features.no)

# Randomise matrix with numbers between 1 and 1000
my.matrix<- apply(my.matrix, c(1, 2), function(x) sample(c(1:1000), 1))
# Convert to dataframe
my.data<- as.data.frame(my.matrix)

# Assign random target variable
set.seed(222)
prob.wght<- runif(1, min= 0.45, max= 0.55)
my.data$resp<- as.factor(sample(c("Yes", "No"), size= n, replace= TRUE, 
                                 prob= c(prob.wght, 1-prob.wght)))
# Create ordered factors for class
my.data$resp<- factor(my.data$resp, 
                       levels=c("Yes","No"),
                       ordered=TRUE)

# Review target variable
tbl<- prop.table(table(my.data$resp))
bpl1<- barplot(tbl, main= "Proportion of class labels")
text(bpl1, 0, tbl, cex= 1, pos= 3)

# Initializes the progress bar
pb <- progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                       total = features.no,
                       complete = "=",   # Completion bar character
                       incomplete = "-", # Incomplete bar character
                       current = ">",    # Current bar character
                       clear = FALSE,    # If TRUE, clears the bar when finish
                       width = 100)      # Width of the progress bar

#################################################################
### FIT GLM ON EVERY FEATURE TO IDENTIFY SIGNIFICANT FEATURES ###
#################################################################

# Create empty vector to store results
signif.var<- rep(0, features.no)
for(i in 1:features.no)
{
  colNames= c(names(my.data)[i], 'resp')
  df<- my.data[, colNames]
  names(df)[1]<- "indep.var"
  fit<- glm(resp~ indep.var, data= df, family= "binomial")
  
  # Extract coeficient significance and store in vector
  p.val<- coef(summary(fit))[2,4]
  signif.var[i]= p.val
  
  # Sets the progress bar to the current state
  pb$tick()

}

bpl2= barplot(prop.table(table(signif.var< 0.05)),
              main= 'No of significant features')
text(bpl2, 0, table(signif.var< 0.05), cex= 1, pos= 3)

# Keep only significant features and response variab;e
myDatFinal= my.data[, c(signif.var< 0.05, features.no + 1)]
dim(myDatFinal)

# Split data into train and test
set.seed(333)
x <- c(TRUE, FALSE)
splitID= sample(x, nrow(myDatFinal) ,replace = TRUE, prob=c(.75, .25))
trainDat= myDatFinal[splitID,]
testDat= myDatFinal[!splitID,]

# Fit random forest 
rfFit <- ranger(resp~ .,   
                data= trainDat,
                probability = TRUE,
                num.trees= 125,
                mtry= floor(sqrt(ncol(trainDat))))

rfProbs= predict(rfFit, data = testDat)$predictions

# Get model performance on test set
roc(testDat$resp, rfProbs[,1],
    smoothed = TRUE,
    # arguments for ci
    ci=TRUE, ci.alpha=0.9, stratified=FALSE,
    # arguments for plot
    plot=TRUE, auc.polygon=TRUE, max.auc.polygon=TRUE, grid=TRUE,
    print.auc=TRUE, show.thres=TRUE)

