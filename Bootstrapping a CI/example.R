### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

# Library
library(dplyr)

# Create population
set.seed(123)
popSize= 10000
popVals= rnorm(n= popSize, mean= 100, sd= 10)

# Visualise distibution and mean
meanPop= mean(popVals)
hist(popVals, main= paste('Population distribution with mean', round(meanPop, 2)))

# Demonstrate confidence interval based on small sample

# Draw multiple samples od the same size
sampleSize= 30
sims= 100

testResults= vector(, length = sims)
lowerCIVector= vector(, length = sims)
upperCIVector= vector(, length = sims)

pb = txtProgressBar(min = 0, max = sims, initial = 0)

for(i in 1:sims)
{
  setTxtProgressBar(pb,i)
  
  set.seed(i)
  # Take random sample
  samplePop= sample(popVals, size= sampleSize, replace = F)
  
  # Get sample mean
  meanPE= mean(samplePop)
  
  # Get standard error which is based on standard deviation of the draw sample
  SE= sd(samplePop)/sqrt(sampleSize)
  
  # Test if pop mean between CI
  testResults[i]= between(meanPop, meanPE - 1.96 * SE, meanPE + 1.96 * SE)
  
  # Record result
  lowerCI= meanPE - 1.96 * SE
  upperCI= meanPE + 1.96 * SE
  lowerCIVector[i]= lowerCI
  upperCIVector[i]= upperCI
}


# Draw plot demonstrating how many samples CI capture the true population parameter
# Any CI that captures the true mean is coloured black
plot.new()
title(main = "Sample confidence intervals")
plot.window(xlim= c(min(lowerCIVector), max(upperCIVector)), ylim= c(0, sims * 3))
for(i in 1:sims)
{
  colCI= ifelse(testResults[i], "black", "red")
  # Draw CI
  segments(lowerCIVector[i], i*3, upperCIVector[i], i*3, col= colCI)
  # Draw true mean line
  abline(v = meanPop, col= "blue")
}

############################################################
### APPROXIMATES CI USING BOOTSRAPPING ON A SMALL SAMPLE ###
############################################################

# Take small sample

set.seed(222)
smallSampleSize= 10
smallSample= sample(popVals, size= smallSampleSize, replace = F)

# Get sample mean
meanPE= mean(smallSample)

# Get standard error which is based on standard deviation of the draw sample
SE= sd(smallSample)/sqrt(sampleSize)

# Bootstrapping is where you resample with replacement many times

# Bootstrap samples
bootStrapNo = 10000
# Note add replacement
bootStrapSamples= sample(smallSample, size= bootStrapNo * smallSampleSize, replace = T)

# Populate matrix
bootStrapMatrix= matrix(bootStrapSamples, nrow= bootStrapNo, byrow= TRUE)

bootStrapSamplesMeans= rowMeans(bootStrapMatrix)
bootStrapMean= mean(bootStrapSamplesMeans)
hist(bootStrapSamplesMeans, 
     main= paste('Bootstrapping distribution with mean', round(bootStrapMean, 2)))
bootStrapCI= quantile(bootStrapSamplesMeans, c(0.05, 0.95))

# Add CI based on 95 percentile of bootstrapping distribution
abline(v= bootStrapCI, col= 'red', lty= 3)

# Worth noting that this process tends to produce a too narrow confidence interval  
# The so-called bias-corrected and accelerated bootstrap interval (the BCa interval) required
# Good package demostrating thia

library(boot)

data <- data.frame(smallSample)
meanfun <- function(data, i){
  d <- data[i, ]
  return(mean(d))   
}
bo <- boot(data[, "smallSample", drop = FALSE], 
           statistic= meanfun, R= bootStrapNo)
bootStrapCI= boot.ci(bo, conf= 0.95, type= "bca")
bootStrapCIBCA= bootStrapCIBCA$bca[c(4,5)]

# Add BCA corrected line noting that it is wider
abline(v= bootStrapCIBCA, col= 'blue', lty= 3)

