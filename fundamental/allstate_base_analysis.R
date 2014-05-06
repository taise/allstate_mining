## train
# 1ユーザの見積もり履歴
# shopping_pt 1 ~ 12
# record_type == 0  #=> 見積り結果
# record_type == 1  #=> 選んだ見積り
# データは時系列と結果

## Submission
# 一部切り取られた時系列の見積もりtestデータから
# ユーザごとに自動車保険のオプション(A~G)を予測する

setwd("~/statistics/data//kaggle/allstate")

train <- read.csv("./train.csv")
#test <- read.csv("./test_v2.csv")

names(train)
# [1] "customer_ID"       "shopping_pt"       "record_type"       "day"              
# [5] "time"              "state"             "location"          "group_size"       
# [9] "homeowner"         "car_age"           "car_value"         "risk_factor"      
# [13] "age_oldest"        "age_youngest"      "married_couple"    "C_previous"       
# [17] "duration_previous" "A"                 "B"                 "C"                
# [21] "D"                 "E"                 "F"                 "G"                
# [25] "cost"       

train[1:100,]
summary(train)

table(train$shopping_pt, train$record_type == 1)
hist(train$shopping_pt)

record_type1 <- subset(train, record_type == 1)

table(record_type1$A)
table(record_type1$B)
table(record_type1$C)
table(record_type1$D)
table(record_type1$E)
table(record_type1$F)
table(record_type1$G)

res <- data.frame(
  record_type1$A,
  record_type1$B,
  record_type1$C,
  record_type1$D,
  record_type1$E,
  record_type1$F,
  record_type1$G
  )

names(res)
colnames(res) <- c("A", "B", "C", "D", "E", "F", "G")

cor(res)

library(corrplot)
M <- cor(res)
corrplot.mixed(M)

# cor C,D => 0.61
# cor A,F => 0.53

res.pattern <- paste(res$A, res$B, res$C, res$D, res$E, res$F, res$G, sep = "")
res.pattern[1:10]
(res.pattern_freq <- table(res.pattern))
str(res.pattern)
length(unique(res.pattern))
sort(unique(res.pattern))

# 1522パターンを予測するのは現実的ではない。
# A~Gまでをそれぞれ予測するモデルを構築する。

res.pattern_freq_under100 <- subset(res.pattern_freq, res.pattern_freq < 100) 
res.pattern_freq_over100 <- subset(res.pattern_freq, res.pattern_freq >= 100) 

plot(res.pattern_freq_under100)
plot(res.pattern_freq_over100)

df_res_pattern <- data.frame(table(res.pattern))
plot(df_res_pattern)
# A~Gの組み合わせパターンに偏りがあることがわかった


# 分析方針
#
#  last_quoted_plan(最後の見積り案)が全て選ばれるパターンで予測するとスコアが非常によい。
#  とはいえ、最後の見積りが使われない可能性もある。
#
#  複数段階で推測するのは、予測に段階分のずれが発生する可能性がある。
#  できるだけ1回で予測したい。
#  1回で予測するとしても、時系列データを使って分析出来るようにしたい
#
#  全てのユーザがshopping_ptが2以上のため、1列でshopping_ptが1,2のものをならべる。
#  そこで予測精度が出なかった場合は値の変化予想をする。

