calibrationPlot= function(dataFrame= testDat, 
                          respColumn= 'resp',
                          respLabel, 
                          predictions, 
                          percentilesNo= 10)
{
  dataFrame= cbind(dataFrame, preds= predictions)

  decileDataFrame= 
    dataFrame %>%
    mutate(decile = ntile(preds, percentilesNo)) %>%
    mutate(decile= as.factor(decile))
  
  calibrationDeciles=
    decileDataFrame %>%
    group_by(decile) %>%
    summarise(totalRows= n(),
              posResp= sum(ifelse(resp== respLabel, 1, 0)),
              avgPred= mean(preds)) %>%
    mutate(actualRate= posResp/totalRows)
  
  xyplot(avgPred + actualRate~  decile, data= calibrationDeciles, type = "l", pch=20,
         as.table= TRUE,
         main = "Calibration plot", ylab = "%", xlab = "Decile",
         auto.key=list(space="top", columns= 2))
}


