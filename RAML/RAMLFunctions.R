library.packages<- function()
{
  library(RODBC)
  library(caret)
  library(knitr)
  library(pROC)
  library(MLmetrics)
  library(PerformanceAnalytics)
  library(doParallel)
}

# TODO: REFACTOR CODE TO RETURN OBJECTS INTO LISTS

dummify.data<- function()
{
  mydatDummy<- dummyVars("~.", data= mydat, fullRank= TRUE)
  mydat.num<<- as.data.frame(predict(mydatDummy, mydat))
  str(mydat)
}

partition.data<- function(seed= 123, split= 0.7, resp= depend.var)
{
  set.seed(seed)
  
  # Split data for train and test
  inTrain<- createDataPartition(y= depend.var, p= split, list= FALSE)
  training<<- mydat[inTrain,] # global assignment use with care
  testing<<- mydat[-inTrain,] # global assignment use with care
  training.num<<- mydat.num[inTrain,] # global assignment use with care
  testing.num<<- mydat.num[-inTrain,] # global assignment use with care
  
  # Split response for train and test
  y.train<<- depend.var[inTrain] # global assignment use with care
  y.test<<- depend.var[-inTrain] # global assignment use with care
}

review.splits<- function()
{
  # Review distibution of splits in partitions
  par(mfrow= c(1, 4))
  val<- table(depend.var)
  bp<- barplot(val, main= "Response split")
  text(x= bp, y= val/2, label= sprintf("%1.1f%%",prop.table(val)*100))
  
  val<- c(nrow(training), nrow(testing))
  val.m<- as.matrix(val)
  bp<- barplot(val.m, main= "Partition split")
  text(x= bp, y= c(val[1]/2, val[1]+val[2]/2, val[1]+val[2]+val[3]/2), 
       label= sprintf("%1.1f%%", prop.table(val)*100))
  
  val<- table(y.train)
  bp1<- barplot(val, main= "Training split")
  text(x= bp1, y= val/2, label= sprintf("%1.1f%%",prop.table(val)*100))
  
  val<- table(y.test)
  bp2<- barplot(val, main= "Testing split")
  text(x= bp2, y= val/2, label= sprintf("%1.1f%%",prop.table(val)*100))
}

seed= 123
model.hyp.par= 5
folds= 10
repeats= 5
classification.metric= "Kappa"

train.models<- function(seed= 123, 
                        model.hyp.par= 5, 
                        folds= 10, 
                        repeats= 5,
                        classification.metric= "Accuracy",
                        infold.sampling= "down")
{
  
  start.trn<<- Sys.time()
  set.seed(seed)
  
  # Preprocess data
  #preprocessParams<- preProcess(training, method= "nzv")
  #training.proc <- predict(preprocessParams, training)
  
  #Pre-Compute CV folds so we can use the same ones for all models
  CV_Folds<- createMultiFolds(y= y.train, k= folds, times= repeats)
  
  ## Determine operating system type
  osType <- switch(Sys.info()[['sysname']],
                   Windows= {print("I'm a Windows PC.")},
                   Linux  = {print("I'm a penguin.")},
                   Darwin = {print("I'm a Mac.")})
  
  
  # Setup model train parameters
  if(classification.metric== "ROC"|
     classification.metric== "Sens"|
     classification.metric== "Spec")
  {
    # Tune for AUC under ROC curve
    trainctrl<- trainControl(method='repeatedCV', 
                             index= CV_Folds,
                             classProbs= TRUE,
                             allowParallel= TRUE,
                             summaryFunction= twoClassSummary,
                             verboseIter= TRUE,
                             sampling= infold.sampling)
    model.thresh<<- "variable"
    
  } else 
    if(classification.metric== "AUC"|
       classification.metric== "Precision"|
       classification.metric== "Recall"|
       classification.metric== "F")
    {
      trainctrl<- trainControl(method='repeatedCV', 
                               index= CV_Folds,
                               classProbs= TRUE,
                               allowParallel= TRUE,
                               summaryFunction= prSummary,
                               verboseIter= TRUE,
                               sampling= infold.sampling)
      model.thresh<<- "variable"
    }
  
  else
    if(classification.metric== "Accuracy"|
       classification.metric== "Kappa")
    {
      trainctrl<- trainControl(method='repeatedCV', 
                               index= CV_Folds,
                               classProbs= TRUE,
                               allowParallel= TRUE,
                               verboseIter= TRUE,
                               sampling= infold.sampling)
      model.thresh<<- "fixed"
    }
  
  
  metric.opt<<- classification.metric
  
  #Fit a Random Forest
  rf.start<<- Sys.time()
  model.rf<<- train(x= training, y= y.train,
                    method= "ranger", 
                    preProcess= "medianImpute",
                    tuneLength= model.hyp.par,
                    trControl= trainctrl,
                    metric= classification.metric)
  rf.end<<- Sys.time()
  
  #Fit a Gradient Boosting Machine
  gbm.start<<- Sys.time()
  model.gbm<<- train(x= training, y= y.train,
                     method= "gbm", 
                     preProcess= "medianImpute",
                     tuneLength= model.hyp.par,
                     trControl= trainctrl,
                     metric= classification.metric)
  gbm.end<<- Sys.time()
  
  c50Grid<- expand.grid(.trials= 50, .model= c("tree", "rules"), .winnow= FALSE)
  
  # Fit a Boosted Tree
  c5.start<<- Sys.time()
  model.c5<<- train(x= training, y= y.train,
                    method= "C5.0", 
                    preProcess= "medianImpute",
                    #tuneLength= model.hyp.par,
                    tuneGrid = c50Grid,
                    trControl= trainctrl,
                    metric= classification.metric)
  c5.end<<- Sys.time()
  
  #Fit a tree
  dt.start<<- Sys.time()
  model.tree<<- train(x= training, y= y.train,
                      method= "rpart", 
                      preProcess= "medianImpute",
                      tuneLength= model.hyp.par,
                      trControl= trainctrl,
                      metric= classification.metric)
  dt.end<<- Sys.time()
  
  # Create numerix matrix
  # Fit a penalised regression
  # Preprocessing cheat sheet 
  # Start with median imputation>center and scale>Try PCA and spatial sign
  # Tree-based models don't need much preprocessing 
  glm.start<<- Sys.time()
  model.glm<<- train(x= training.num, y= y.train,
                     method= "glmnet", 
                     preProcess= c("zv", "nzv", "medianImpute", "center", "scale"), 
                     tuneLength= model.hyp.par,
                     trControl= trainctrl,
                     metric= classification.metric)
  glm.end<<- Sys.time()
  
  #Compare models
  #RF= model.rf
  resamps<<- resamples(list(RF= model.rf,
                            GBM= model.gbm,
                            C5= model.c5, 
                            DT= model.tree,
                            GLMnet= model.glm
  ))
  
  rf.diff<- (rf.end-rf.start)
  gbm.diff<- (gbm.end-gbm.start)
  c5.diff<- (c5.end-c5.start)
  dt.diff<- (dt.end-dt.start)
  glm.diff<- (glm.end-glm.start)
  
  model.rt<- c(rf.diff,
               gbm.diff,
               c5.diff,
               dt.diff,
               glm.diff
  )
  
  model.rt.min<- as.vector(model.rt)/60
  names(model.rt.min)<- c("RF", "GBM", "C5", "DT", "GLMnet")
  
  par(mfrow= c(1,1))
  barplot(model.rt.min, main= "Model run-time (mins)", 
          ylim= c(0, max(model.rt.min)*1.1))
  
  end.trn<<- Sys.time()
}


model.diagnostics<- function()
{
  if(model.thresh== "fixed")
  {
    # Return lattice plots
    plot1<- bwplot(resamps, metric= "Accuracy")
    plot2<- bwplot(resamps, metric= "Kappa")
    plot3<- densityplot(resamps, metric= "Accuracy", 
                        auto.key= list(corner= c(0.98, 0.98),
                                       cex= 0.5))
    plot4<- densityplot(resamps, metric= "Kappa", 
                        auto.key= list(corner= c(0.98, 0.98),
                                       cex= 0.5))
    # Plot prints
    print(plot1, split = c(1, 1, 2, 2), more = TRUE)
    print(plot2, split = c(2, 1, 2, 2), more = TRUE)
    print(plot3, split = c(1, 2, 2, 2), more = TRUE)
    print(plot4, split = c(2, 2, 2, 2), more = FALSE)  # more = FALSE is redundant
    mtext(metric.opt, outer= TRUE, cex = 1.5)
    
    # Interogate correlations between model predictions
    chart.Correlation(resamps)
  }
  
  else
  {
    plot1<- bwplot(resamps, metric= "ROC")
    plot2<- bwplot(resamps, metric= "Sens")
    plot3<- bwplot(resamps, metric= "Spec")
    plot4<- densityplot(resamps, metric= "ROC", 
                        auto.key= list(corner= c(0.98, 0.98),
                                       cex= 0.5))
    plot5<- densityplot(resamps, metric= "Sens", 
                        auto.key= list(corner= c(0.98, 0.98),
                                       cex= 0.5))
    plot6<- densityplot(resamps, metric= "Spec", 
                        auto.key= list(corner= c(0.98, 0.98),
                                       cex= 0.5))
    
    print(plot1, split = c(1, 1, 3, 2), more = TRUE)
    print(plot2, split = c(2, 1, 3, 2), more = TRUE)
    print(plot3, split = c(3, 1, 3, 2), more = TRUE)
    print(plot4, split = c(1, 2, 3, 2), more = TRUE)
    print(plot5, split = c(2, 2, 3, 2), more = TRUE)
    print(plot6, split = c(3, 2, 3, 2), more = FALSE)
    mtext(metric.opt, outer= TRUE, cex = 1.5)
    
    # Interogate correlations between model predictions
    chart.Correlation(resamps)
  }
}

test.models<- function(model.type= "RF")
{
  
  # Predict on test set
  #pred.xgb<- predict(model.xgb, newdata= testing.num)
  
  eval.data<- testing
  
  if(model.type== "RF")
  {
    final.model<- model.rf
    pred.rf<- predict(model.rf, newdata= testing)
  }
  else
    if(model.type== "GBM")
    {
      final.model<- model.gbm
      pred.gbm<- predict(model.gbm, newdata= testing)
    }
  else
    if(model.type== "C5")
    {
      final.model<- model.c5
      pred.c5<- predict(model.c5, newdata= testing)
    }
  else
    if(model.type== "DT")
    {
      final.model<- model.tree
      pred.tree<- predict(model.tree, newdata= testing)
    }
  else
    if(model.type== "GLMnet")
    {
      final.model<- model.glm
      eval.data<- testing.num
      pred.glm<- predict(model.glm, newdata= testing.num)
    }
  else
  {
    final.model<- model.xgb
    eval.data<- testing.num
  }
  
  # Predict on test holdout
  predict.class<<- predict(final.model, newdata= eval.data)
  # Predict on test holdout- raw probabilities
  predict.prob<<- predict(final.model, newdata= eval.data, type= "prob")[,2]
  
  CM<<- confusionMatrix(predict.class, y.test)
  
  # ROC curve
  auc<- roc(y.test, predict.prob)
  
  # Best threshold
  optimal.thresh<- coords(auc, "b", ret="t")
  
  # Plot test results
  par(mfrow= c(1,2))
  list(
    fourfoldplot(CM$table, color = c("#CC6666", "#99CC99"), conf.level= 0, margin= 1),
    plot(auc, ylim=c(0,1), print.thres=TRUE, main=paste('AUC:',round(auc$auc[[1]],2)))
  )
  mtext(model.type, outer= TRUE, cex = 1.5)
}  

### FIT GBM+GLMNet ####

train.2models<- function(seed= 123, 
                         model.hyp.par= 5, 
                         folds= 10, 
                         repeats= 5,
                         classification.metric= "Accuracy",
                         sub.sampling= "none")
{
  
  start.trn<<- Sys.time()
  set.seed(seed)
  
  # Preprocess data
  #preprocessParams<- preProcess(training, method= "nzv")
  #training.proc <- predict(preprocessParams, training)
  
  #Pre-Compute CV folds so we can use the same ones for all models
  CV_Folds<- createMultiFolds(y= y.train, k= folds, times= repeats)
  
  # Setup model train parameters
  if(classification.metric== "ROC"|
     classification.metric== "Sens"|
     classification.metric== "Spec")
  {
    # Tune for AUC under ROC curve
    trainctrl<- trainControl(method='repeatedCV', 
                             index= CV_Folds,
                             classProbs= TRUE,
                             allowParallel= TRUE,
                             summaryFunction= twoClassSummary,
                             verboseIter= TRUE,
                             sampling= "down")
    model.thresh<<- "variable"
    
  } else
  {
    trainctrl<- trainControl(method='repeatedCV', 
                             index= CV_Folds,
                             classProbs= TRUE,
                             allowParallel= TRUE,
                             verboseIter= TRUE,
                             sampling= "down")
    model.thresh<<- "fixed"
  }
  metric.opt<<- classification.metric
  
  #Fit a Gradient Boosting Machine
  gbm.dwn.start<<- Sys.time()
  model.gbm.dwn<<- train(x= training, y= y.train,
                         method= "gbm", 
                         tuneLength= model.hyp.par,
                         trControl= trainctrl,
                         metric= classification.metric)
  gbm.dwn.end<<- Sys.time()
  
  # Create numerix matrix
  # Fit a penalised regression
  # Preprocessing cheat sheet 
  # Start with median imputation>center and scale>Try PCA and spatial sign
  # Tree-based models don't need much preprocessing 
  glm.dwn.start<<- Sys.time()
  model.glm.dwn<<- train(x= training.num, y= y.train,
                         method= "glmnet", 
                         preProcess= c("zv", "nzv", "medianImpute", "center", "scale"), 
                         tuneLength= model.hyp.par,
                         trControl= trainctrl,
                         metric= classification.metric)
  glm.dwn.end<<- Sys.time()
  
  # NO SAMPLING
  # Setup model train parameters
  if(classification.metric== "ROC"|
     classification.metric== "Sens"|
     classification.metric== "Spec")
  {
    # Tune for AUC under ROC curve
    trainctrl<- trainControl(method='repeatedCV', 
                             index= CV_Folds,
                             classProbs= TRUE,
                             allowParallel= TRUE,
                             summaryFunction= twoClassSummary,
                             verboseIter= TRUE,
                             sampling = "smote")
    model.thresh<<- "variable"
    
  } else
  {
    trainctrl<- trainControl(method='repeatedCV', 
                             index= CV_Folds,
                             classProbs= TRUE,
                             allowParallel= TRUE,
                             verboseIter= TRUE,
                             sampling= "smote")
    model.thresh<<- "fixed"
  }
  metric.opt<<- classification.metric
  
  #Fit a Gradient Boosting Machine
  gbm.start<<- Sys.time()
  model.gbm<<- train(x= training, y= y.train,
                     method= "gbm", 
                     tuneLength= model.hyp.par,
                     trControl= trainctrl,
                     metric= classification.metric)
  gbm.end<<- Sys.time()
  
  # Create numerix matrix
  # Fit a penalised regression
  # Preprocessing cheat sheet 
  # Start with median imputation>center and scale>Try PCA and spatial sign
  # Tree-based models don't need much preprocessing 
  glm.start<<- Sys.time()
  model.glm<<- train(x= training.num, y= y.train,
                     method= "glmnet", 
                     preProcess= c("zv", "nzv", "medianImpute", "center", "scale"), 
                     tuneLength= model.hyp.par,
                     trControl= trainctrl,
                     metric= classification.metric)
  glm.end<<- Sys.time()
  
  #Compare models
  #RF= model.rf
  resamps<<- resamples(list(GBM.down= model.gbm.dwn,
                            GLMnet.down= model.glm.dwn,
                            GBM= model.gbm,
                            GLMnet= model.glm
  ))
  
  gbm.dwn.diff<- (gbm.dwn.end-gbm.dwn.start)
  glm.dwn.diff<- (glm.dwn.end-glm.dwn.start)
  gbm.diff<- (gbm.end-gbm.start)
  glm.diff<- (glm.end-glm.start)
  
  model.rt<- c(gbm.dwn.diff,
               glm.dwn.diff,
               gbm.diff,
               glm.diff
  )
  
  model.rt.min<- as.vector(model.rt)/60
  names(model.rt.min)<- c("GBM downsamp", 
                          "GLMnet downsamp",
                          "GBM", 
                          "GLMnet"
  )
  
  par(mfrow= c(1,1))
  barplot(model.rt.min, main= "Model run-time (mins)", 
          ylim= c(0, max(model.rt.min)*1.1))
  
  end.trn<<- Sys.time()
}