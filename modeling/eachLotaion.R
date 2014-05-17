# modeling: training each location

library(rpart)
library(partykit)

source_dir <- "~/development/mining/allstate/lib/"
source(paste(source_dir, "set_type_shopping_pt_1_2.R", sep=""))
source(paste(source_dir, "modelingByLocation.R", sep=""))
source(paste(source_dir, "score.R", sep=""))


setwd("~/statistics/data/kaggle/allstate")
originTrainCsv <- read.csv("shoppingPt1-2.csv")

trainCsv <- setTypeShoppingPt_1_2(originTrainCsv)
trainCsv$customer_ID = NULL
trainCsv$pt1_day = NULL
trainCsv$pt2_day = NULL




modelFL <- modelingByLocation(trainCsv, "FL")
