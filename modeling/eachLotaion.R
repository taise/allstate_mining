# modeling: training major state or other states

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


##### major states

modelNY <- modelingByLocation(trainCsv, "NY")
modelFL <- modelingByLocation(trainCsv, "FL")
modelPA <- modelingByLocation(trainCsv, "PA")
modelOH <- modelingByLocation(trainCsv, "OH")

##### minor states

otherStates <- subset(trainCsv,
                      pt1_state != "NY" & pt1_state != "FL" &
                      pt1_state != "PA" & pt1_state != "OH")

modelOther <- modelingAtoG(otherStates)


##### test data prediction
test_csv <- read.csv("test_shopping_pt1_2.csv")
testPt1_2 <- setTypeShoppingPt_1_2(test_csv)


vect <- c("A", "B", "C", "D", "E", "F", "G")
states <- c("NY", "FL", "PA", "OH")
predTest <- ""
nRow <- nrow(testPt1_2)

for(v in vect) {
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
write.csv(testDf,file="./20140518_predictMajorState.csv",row.names=FALSE)
