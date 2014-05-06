library(rpart)
library(partykit)

source_dir <- "~/development/mining/allstate/lib/"
source(paste(source_dir, "set_type_shopping_pt_1_2.R", sep=""))
source(paste(source_dir, "score.R", sep=""))

setwd("~/statistics/data//kaggle/allstate")
train_csv <- read.csv("shopping_pt1-2_train.csv")
valid_csv <- read.csv("shopping_pt1-2_valid.csv")

trainPt1_2 <- setTypeShoppingPt_1_2(train_csv)
validPt1_2 <- setTypeShoppingPt_1_2(valid_csv)
trainPt1_2$customer_ID = NULL
validPt1_2$customer_ID = NULL
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


### 試しに州のクラスタリングを行ってみる => 試行中
# scoreはA~Gのセットの選択頻度
# k-meansは値でなければ扱えないので、カテゴリ変数を州ごとの割合にする
# 
# option_pattern <- paste(trainPt1_2$pt1_A, trainPt1_2$pt1_B, trainPt1_2$pt1_C,
#                         trainPt1_2$pt1_D, trainPt1_2$pt1_E, trainPt1_2$pt1_F,
#                         trainPt1_2$pt1_G, sep = "")
# states <- na.omit(data.frame(trainPt1_2$pt1_state, option_pattern))
# state_freq <-table(states$trainPt1_2.pt1_state, option_pattern)
# state_freq[1,]
# state_cluster <- kmeans(cluster_trainPt1_2[,1:2], 2,iter.max = 10, nstart = 1)
