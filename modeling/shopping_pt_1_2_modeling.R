library(rpart)
library(partykit)
source("~/development/R/kaggle/allstate/set_type_shopping_pt_1_2.R")
source("~/development/R/kaggle/allstate/score.R")

setwd("~/statistics/data//kaggle/allstate")
train <- read.csv("shopping_pt1-2_train.csv")
valid <- read.csv("shopping_pt1-2_valid.csv")

train <- setTypeShoppingPt_1_2(train)
valid <- setTypeShoppingPt_1_2(valid)
train$customer_ID = NULL
valid$customer_ID = NULL
#names(train)
#summary(train)
#str(train)

# typeがちゃんと認識されていないので関数でセットする


#train.tree <- rpart(as.factor(rt1_A)~., data=train, maxdepth=3)
# 全変数だと返ってこない

# 変数を減らせないか検討する
# 州によって法律が違うため、A~Gのオプションもばらつきがあった
#  => 州ごとに予測してみる

#str(train$pt2_state)
# Factor w/ 36 levels "AL","AR","CO",..: 11 24 28 35 16 25 26 28 25 25

#plot(train$pt1_state, train$rt1_A)
#plot(train$pt1_state, train$rt1_B)
#plot(train$pt1_state, train$rt1_C)
#plot(train$pt1_state, train$rt1_D)
#plot(train$pt1_state, train$rt1_E)
#plot(train$pt1_state, train$rt1_F)
#plot(train$pt1_state, train$rt1_G)

#table(train$pt1_state)
# NY: 9577で一番多いのでNYでモデル化の後、評価してみる

trainNY <- subset(train, pt1_state == "NY")
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

validNY <- subset(valid, pt1_state == "NY")
validNY.predA <- predict(trainNY.treeA, newdata=validNY, type="class")

#resA <- table(validNY.predA == validNY$rt1_A)
#resA[2] / (resA[1] + resA[2])

score(trainNY.predA, trainNY$rt1_A)
score(validNY.predA, validNY$rt1_A)


### 試しに州のクラスタリングを行ってみる => 試行中
# scoreはA~Gのセットの選択頻度
# k-meansは値でなければ扱えないので、カテゴリ変数を州ごとの割合にする
# 
# option_pattern <- paste(train$pt1_A, train$pt1_B, train$pt1_C,
#                         train$pt1_D, train$pt1_E, train$pt1_F,
#                         train$pt1_G, sep = "")
# states <- na.omit(data.frame(train$pt1_state, option_pattern))
# state_freq <-table(states$train.pt1_state, option_pattern)
# state_freq[1,]
# state_cluster <- kmeans(cluster_train[,1:2], 2,iter.max = 10, nstart = 1)
