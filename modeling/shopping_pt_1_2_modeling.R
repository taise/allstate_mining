library(rpart)
library(partykit)

source_dir <- "~/development/mining/allstate/lib/"
source(paste(source_dir, "set_type_shopping_pt_1_2.R", sep=""))
source(paste(source_dir, "score.R", sep=""))

setwd("~/statistics/data/kaggle/allstate")
train_csv <- read.csv("shoppingPt1-2.csv")
#valid_csv <- read.csv("shopping_pt1-2_valid.csv")

tmpPt <- setTypeShoppingPt_1_2(train_csv)
tmpPt$customer_ID = NULL
dfRow <- nrow(tmpPt)

trainPt1_2 <- tmpPt[1:round(dfRow*0.7)-1,]
validPt1_2 <- tmpPt[round(dfRow*0.7):dfRow,]

#names(trainPt1_2)
#summary(trainPt1_2)
#str(trainPt1_2)

# typeがちゃんと認識されていないので関数でセットする


#trainPt1_2.tree <- rpart(as.factor(rt1_A)~., data=trainPt1_2, maxdepth=3)
# 全変数だと返ってこない

# 変数を減らせないか検討する
# 州によって法律が違うため、A~Gのオプションもばらつきがあった
#  => 州ごとに予測してみる

#str(trainPt1_2$pt2_state)
# Factor w/ 36 levels "AL","AR","CO",..: 11 24 28 35 16 25 26 28 25 25

#plot(trainPt1_2$pt1_state, trainPt1_2$rt1_A)
#plot(trainPt1_2$pt1_state, trainPt1_2$rt1_B)
#plot(trainPt1_2$pt1_state, trainPt1_2$rt1_C)
#plot(trainPt1_2$pt1_state, trainPt1_2$rt1_D)
#plot(trainPt1_2$pt1_state, trainPt1_2$rt1_E)
#plot(trainPt1_2$pt1_state, trainPt1_2$rt1_F)
#plot(trainPt1_2$pt1_state, trainPt1_2$rt1_G)

#table(trainPt1_2$pt1_state)
# NY: 9577で一番多いのでNYでモデル化の後、評価してみる

trainNY <- subset(trainPt1_2, pt1_state == "NY")
trainNY$pt1_state <- NULL
table(trainNY$pt2_state)
trainNY$pt2_state <- NULL

trainNY$rt1_B = NULL
trainNY$rt1_C = NULL
trainNY$rt1_D = NULL
trainNY$rt1_E = NULL
trainNY$rt1_F = NULL
trainNY$rt1_G = NULL


trainNY.treeA <- rpart(rt1_A~., data=trainNY, maxdepth=4, cp=-1)
trainNY.predA <- predict(trainNY.treeA, newdata=trainNY, type="class")
summary(trainNY.treeA)
plot(as.party(trainNY.treeA))

# valid

validNY <- subset(validPt1_2, pt1_state == "NY")
validNY.predA <- predict(trainNY.treeA, newdata=validNY, type="class")

#resA <- table(validNY.predA == validNY$rt1_A)
#resA[2] / (resA[1] + resA[2])

score(trainNY.predA, trainNY$rt1_A)
score(validNY.predA, validNY$rt1_A)


#### B

trainNY <- subset(trainPt1_2, pt1_state == "NY")
trainNY$pt1_state <- NULL
trainNY$pt2_state <- NULL

trainNY$rt1_A = NULL
trainNY$rt1_C = NULL
trainNY$rt1_D = NULL
trainNY$rt1_E = NULL
trainNY$rt1_F = NULL
trainNY$rt1_G = NULL

trainNY.treeB <- rpart(rt1_B~., data=trainNY, maxdepth=4, cp=-1)
trainNY.predB <- predict(trainNY.treeB, newdata=trainNY, type="class")
summary(trainNY.treeB)
plot(as.party(trainNY.treeB))

# valid
validNY.predB <- predict(trainNY.treeB, newdata=validNY, type="class")

score(trainNY.predB, trainNY$rt1_B)
score(validNY.predB, validNY$rt1_B)


#### C

trainNY <- subset(trainPt1_2, pt1_state == "NY")
trainNY$pt1_state <- NULL
trainNY$pt2_state <- NULL

trainNY$rt1_A = NULL
trainNY$rt1_B = NULL
trainNY$rt1_D = NULL
trainNY$rt1_E = NULL
trainNY$rt1_F = NULL
trainNY$rt1_G = NULL

trainNY.treeC <- rpart(rt1_C~., data=trainNY, maxdepth=4, cp=-1)
trainNY.predC <- predict(trainNY.treeC, newdata=trainNY, type="class")
summary(trainNY.treeC)
plot(as.party(trainNY.treeC))

# valid
validNY.predC <- predict(trainNY.treeC, newdata=validNY, type="class")

score(trainNY.predC, trainNY$rt1_C)
score(validNY.predC, validNY$rt1_C)

#### D

trainNY <- subset(trainPt1_2, pt1_state == "NY")
trainNY$pt1_state <- NULL
trainNY$pt2_state <- NULL

trainNY$rt1_A = NULL
trainNY$rt1_B = NULL
trainNY$rt1_C = NULL
trainNY$rt1_E = NULL
trainNY$rt1_F = NULL
trainNY$rt1_G = NULL

trainNY.treeD <- rpart(rt1_D~., data=trainNY, maxdepth=4, cp=-1)
trainNY.predD <- predict(trainNY.treeD, newdata=trainNY, type="class")
summary(trainNY.treeD)
plot(as.party(trainNY.treeD))

# valid
validNY.predD <- predict(trainNY.treeD, newdata=validNY, type="class")

score(trainNY.predD, trainNY$rt1_D)
score(validNY.predD, validNY$rt1_D)


#### E

trainNY <- subset(trainPt1_2, pt1_state == "NY")
trainNY$pt1_state <- NULL
trainNY$pt2_state <- NULL

trainNY$rt1_A = NULL
trainNY$rt1_B = NULL
trainNY$rt1_C = NULL
trainNY$rt1_D = NULL
trainNY$rt1_F = NULL
trainNY$rt1_G = NULL

trainNY.treeE <- rpart(rt1_E~., data=trainNY, maxdepth=4, cp=-1)
trainNY.predE <- predict(trainNY.treeE, newdata=trainNY, type="class")
summary(trainNY.treeE)
plot(as.party(trainNY.treeE))

# valid
validNY.predE <- predict(trainNY.treeE, newdata=validNY, type="class")

score(trainNY.predE, trainNY$rt1_E)
score(validNY.predE, validNY$rt1_E)


#### F

trainNY <- subset(trainPt1_2, pt1_state == "NY")
trainNY$pt1_state <- NULL
trainNY$pt2_state <- NULL

trainNY$rt1_A = NULL
trainNY$rt1_B = NULL
trainNY$rt1_C = NULL
trainNY$rt1_D = NULL
trainNY$rt1_E = NULL
trainNY$rt1_G = NULL

trainNY.treeF <- rpart(rt1_F~., data=trainNY, maxdepth=4, cp=-1)
trainNY.predF <- predict(trainNY.treeF, newdata=trainNY, type="class")
summary(trainNY.treeF)
plot(as.party(trainNY.treeF))

# valid
validNY.predF <- predict(trainNY.treeF, newdata=validNY, type="class")

score(trainNY.predF, trainNY$rt1_F)
score(validNY.predF, validNY$rt1_F)


#### G

trainNY <- subset(trainPt1_2, pt1_state == "NY")
trainNY$pt1_state <- NULL
trainNY$pt2_state <- NULL

trainNY$rt1_A = NULL
trainNY$rt1_B = NULL
trainNY$rt1_C = NULL
trainNY$rt1_D = NULL
trainNY$rt1_E = NULL
trainNY$rt1_F = NULL

trainNY.treeG <- rpart(rt1_G~., data=trainNY, maxdepth=4, cp=-1)
trainNY.predG <- predict(trainNY.treeG, newdata=trainNY, type="class")
summary(trainNY.treeG)
plot(as.party(trainNY.treeG))

# valid
validNY.predG <- predict(trainNY.treeG, newdata=validNY, type="class")

score(trainNY.predG, trainNY$rt1_G)
score(validNY.predG, validNY$rt1_G)


####### test

test_csv <- read.csv("test_shopping_pt1_2.csv")
testPt1_2 <- setTypeShoppingPt_1_2(test_csv)


pred.A <- predict(trainNY.treeA, newdata=testPt1_2, type="class")
pred.B <- predict(trainNY.treeB, newdata=testPt1_2, type="class")
pred.C <- predict(trainNY.treeC, newdata=testPt1_2, type="class")
pred.D <- predict(trainNY.treeD, newdata=testPt1_2, type="class")
pred.E <- predict(trainNY.treeE, newdata=testPt1_2, type="class")
pred.F <- predict(trainNY.treeF, newdata=testPt1_2, type="class")
pred.G <- predict(trainNY.treeG, newdata=testPt1_2, type="class")

test_preds <- paste(pred.A, pred.B, pred.C, pred.D, pred.E, pred.F, pred.G, sep = "")
test_df <- data.frame(customer_ID=testPt1_2$customer_ID, plan=test_preds)
write.csv(test_df,file="./20140515_predict_by_NY.csv",row.names=FALSE)

# 0.25278
