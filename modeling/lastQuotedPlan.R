# Last quoted plan

library(rpart)
library(partykit)

source_dir <- "~/development/mining/allstate/lib/"
source(paste(source_dir, "set_type_shopping_pt_1_2.R", sep=""))
source(paste(source_dir, "modelingAtoG.R", sep=""))
source(paste(source_dir, "modelingByLocation.R", sep=""))
source(paste(source_dir, "score.R", sep=""))

setwd("~/statistics/data/kaggle/allstate")
train_csv <- read.csv("train.csv")
#valid_csv <- read.csv("shopping_pt1-2_valid.csv")

tmpPt <- train_csv

recordType1 <- subset(tmpPt, record_type = 1)
recordType1$lastQuotedPlan <- paste(recordType1$A, recordType1$B,recordType1$C, recordType1$D, recordType1$E, recordType1$F, recordType1$G, sep="")

lastQuote <- data.frame()
nRow <- nrow(tmpPt)
for(i in 1:nRow) {
  if(i %% 1000 == 0) { print(i)}
  if(tmpPt[i, "customer_ID"] != tmpPt[(i + 1), "customer_ID"]) {
    lq <- tmpPt[(i),]

    lqNrow <- nrow(lastQuote)
    if(lqNrow == "") {
      lastQuote <- data.frame(lq$customer_ID, lq$A, lq$B, lq$C, lq$D, lq$E, lq$F, lq$G)
    } else {
      lastQuote<- rbind(lastQuote, c(lq$customer_ID, lq$A, lq$B, lq$C, lq$D, lq$E, lq$F, lq$G))
    }
  }
}

colnames(lastQuote) <- c("customer_ID", "A", "B", "C", "D", "E", "F", "G")
lastQuote$resultPlan <-  paste(lastQuote$A, lastQuote$B, lastQuote$C, lastQuote$D, lastQuote$E, lastQuote$F, lastQuote$G, sep="")
score(lastQuote$resultPlan, recordType1$lastQuotedPlan)


table(recordType1$state, recordType1$A)
table(recordType1$state, recordType1$B)
table(recordType1$state, recordType1$C)
table(recordType1$state, recordType1$D)
table(recordType1$state, recordType1$E)
table(recordType1$state, recordType1$F)
table(recordType1$state, recordType1$G)

##### test

test_csv <- read.csv("test_v2.csv")
testDf <- test_csv

tlastQuote <- data.frame()
nRow <- nrow(testDf)
for(i in 1:nRow) {
  if(i %% 1000 == 0) { print(i)}
  if(testDf[i, "customer_ID"] != testDf[(i + 1), "customer_ID"]) {
    lq <- testDf[(i),]
    
    lqNrow <- nrow(tlastQuote)
    tlastQuote<- rbind(tlastQuote, lq)
  }
}
write.csv(tlastQuote,file="/Users/taise/development/mining/allstate/lastquoted.csv",row.names=FALSE)
