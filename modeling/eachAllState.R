
library(rpart)
library(partykit)

source_dir <- "~/development/mining/allstate/lib/"
source(paste(source_dir, "set_type_shopping_pt_1_2.R", sep=""))
source(paste(source_dir, "modelingAtoG.R", sep=""))
source(paste(source_dir, "modelingByLocation.R", sep=""))
source(paste(source_dir, "score.R", sep=""))


setwd("~/statistics/data/kaggle/allstate")
originTrainCsv <- read.csv("shoppingPt1-2.csv")

trainCsv <- setTypeShoppingPt_1_2(originTrainCsv)
trainCsv$customer_ID = NULL
trainCsv$pt1_day = NULL
trainCsv$pt2_day = NULL


##### all states modeling

states <- unique(trainCsv[,"pt1_state"])
for(state in states) {
  print(state)
  result <- paste("model", state, sep="")
  modelingStr <- paste("modelingByLocation(trainCsv, '", state, "')", sep="")
  eval(parse(text=paste(result, "<-", modelingStr, sep=" ")))
}

##### other states modeling

trainOther <- trainCsv
trainOther$pt1_state = NULL
trainOther$pt2_state = NULL
  
modelOther <- modelingAtoG(trainOther)



#### train data prediction

trainPt1_2 <- trainCsv
vect <- c("A", "B", "C", "D", "E", "F", "G")
predTrain <- ""
nRow <- nrow(trainPt1_2)

for(v in vect) {
  print(v)
  eval(parse(text=paste("predTrain.", v, " = ''", sep="")))
  for(i in 1:nRow) {
    if(i %% 100 == 0) { print(i)}
    state <- trainPt1_2[i, "pt1_state"]
    if (!is.element(state, states)) {
      state <- "Other"
    }
    result <- paste("predTrain.", v, "[", i, "]", sep="")
    predStr <- paste("predict(model", state, "$", v, ", newdata=trainPt1_2[", i, ",], type='class')", sep="")
    eval(parse(text=paste(result, " <- ", predStr, sep="")))
  }
}

predictState <- paste(predTrain.A, predTrain.B, predTrain.C, predTrain.D, predTrain.E, predTrain.F, predTrain.G, sep = "")
trainDf <- data.frame(customer_ID=originTrainCsv$customer_ID, plan=predictState)

trainDf$result <- paste(trainCsv$rt1_A, trainCsv$rt1_B, trainCsv$rt1_C, trainCsv$rt1_D, trainCsv$rt1_E, trainCsv$rt1_F, trainCsv$rt1_G, sep="")
score(trainDf$plan, trainDf$result)



##### test data prediction
test_csv <- read.csv("test_shopping_pt1_2.csv")
testPt1_2 <- setTypeShoppingPt_1_2(test_csv)


vect <- c("A", "B", "C", "D", "E", "F", "G")
predTest <- ""
nRow <- nrow(testPt1_2)

for(v in vect) {
  print(v)
  eval(parse(text=paste("predTest.", v, " = ''", sep="")))
  for(i in 1:nRow) {
    state <- testPt1_2[i, "pt1_state"]
    if (!is.element(state, states)) {
      state <- "Other"
    }
    result <- paste("predTest.", v, "[", i, "]", sep="")
    predStr <- paste("predict(model", state, "$", v, ", newdata=testPt1_2[", i, ",], type='class')", sep="")
    eval(parse(text=paste(result, " <- ", predStr, sep="")))
  }
}

predictState <- paste(predTest.A, predTest.B, predTest.C, predTest.D, predTest.E, predTest.F, predTest.G, sep = "")
testDf <- data.frame(customer_ID=testPt1_2$customer_ID, plan=predictState)
write.csv(testDf,file="./20140518_predictAllState.csv",row.names=FALSE)

# 全然だめだった・・・