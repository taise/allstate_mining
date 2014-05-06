#  1行でshopping_pt1,2,3のデータを並べる

source("~/development/R/kaggle/allstate/timeSegment.R")

setwd("~/statistics/data//kaggle/allstate")
train <- read.csv("./train.csv")


# set type $ fill NA by 0
train$customer_ID    <- as.character(train$customer_ID)
train$day            <- as.factor(train$day)
train$segmentedTime  <- as.factor(mapply(timeSegment, train$time))
train$state          <- as.factor(train$state)
train$homeowner      <- as.factor(train$homeowner)
train$risk_factor    <- as.factor(ifelse(is.na(train$risk_factor), 0, train$risk_factor))
train$married_couple <- as.factor(train$married_couple)
train$C_previous     <- as.factor(train$C_previous)
train$A              <- as.factor(train$A)
train$B              <- as.factor(train$B)
train$C              <- as.factor(train$C)
train$D              <- as.factor(train$D)
train$E              <- as.factor(train$E)
train$F              <- as.factor(train$F)
train$G              <- as.factor(train$G)


# subset by shopping_pt & record_type
pt1 <- subset(train, shopping_pt == 1)
pt2 <- subset(train, shopping_pt == 2)
pt3 <- subset(train, shopping_pt == 3 & record_type == 0)
rt1 <- subset(train, record_type == 1)

merge_by_customer_ID <- function(df1, df2, df1_prefix, df2_prefix) {
  df1$shopping_pt = NULL
  df2$shopping_pt = NULL
  df1$record_type = NULL
  df2$record_type = NULL
  df1$time = NULL
  df2$time = NULL
  df1$record_type = NULL
  df2$record_type = NULL
  
  df1_names <- paste(df1_prefix, names(df1), sep="")
  df2_names <- paste(df2_prefix, names(df2), sep="")
  
  res <- merge(df1, df2, by="customer_ID", all=TRUE)
  colnames(res) <- c("customer_ID", df1_names[-1], df2_names[-1])
  return(res)
}

pt1_2 <- merge_by_customer_ID(pt1, pt2, "pt1_", "pt2_")
pt1_3 <- merge_by_customer_ID(pt1_2, pt3, "", "pt3_")
pt1_3[1:10,]

write.csv(pt1_3, "shopping_pt1-3.csv", row.names=F)