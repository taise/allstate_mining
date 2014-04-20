#  1行でshopping_pt1,2のデータを並べる

setwd("~/statistics/data//kaggle/allstate")
train <- read.csv("./train.csv")


timeSegment <- function(time) {
  time_chr <- as.character(time)
  if("00:00" <= time_chr && time_chr <  "06:00") { return("midnight") }
  if("06:00" <= time_chr && time_chr <  "12:00") { return("morning") }
  if("12:00" <= time_chr && time_chr <  "18:00") { return("daytime") }
  return("evening")
}

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
rt1 <- subset(train, record_type == 1)


# create new dataframe using shopping_pt 1 & 2
shopping_pt1_2 <- data.frame(
    pt1$customer_ID,
    pt1$day,
    pt1$segmentedTime,
    pt1$state,
    pt1$location,
    pt1$group_size,
    pt1$homeowner,
    pt1$car_age,
    pt1$car_value,
    pt1$risk_factor,
    pt1$age_oldest,
    pt1$age_youngest,
    pt1$married_couple,
    pt1$C_previous,
    pt1$duration_previous,
    pt1$A,
    pt1$B,
    pt1$C,
    pt1$D,
    pt1$E,
    pt1$F,
    pt1$G,
    pt1$cost,
    pt2$segmentedTime,
    pt2$state,
    pt2$location,
    pt2$group_size,
    pt2$homeowner,
    pt2$car_age,
    pt2$car_value,
    pt2$risk_factor,
    pt2$age_oldest,
    pt2$age_youngest,
    pt2$married_couple,
    pt2$C_previous,
    pt2$duration_previous,
    pt2$A,
    pt2$B,
    pt2$C,
    pt2$D,
    pt2$E,
    pt2$F,
    pt2$G,
    pt2$cost,
    rt1$A,
    rt1$B,
    rt1$C,
    rt1$D,
    rt1$E,
    rt1$F,
    rt1$G
  )

colnames(shopping_pt1_2) <- c(
    "customer_ID",
    "day",
    "pt1_segmentedTime",
    "pt1_state",
    "pt1_location",
    "pt1_group_size",
    "pt1_homeowner",
    "pt1_car_age",
    "pt1_car_value",
    "pt1_risk_factor",
    "pt1_age_oldest",
    "pt1_age_youngest",
    "pt1_married_couple",
    "pt1_C_previous",
    "pt1_duration_previous",
    "pt1_A",
    "pt1_B",
    "pt1_C",
    "pt1_D",
    "pt1_E",
    "pt1_F",
    "pt1_G",
    "pt1_cost",
    "pt2_segmentedTime",
    "pt2_state",
    "pt2_location",
    "pt2_group_size",
    "pt2_homeowner",
    "pt2_car_age",
    "pt2_car_value",
    "pt2_risk_factor",
    "pt2_age_oldest",
    "pt2_age_youngest",
    "pt2_married_couple",
    "pt2_C_previous",
    "pt2_duration_previous",
    "pt2_A",
    "pt2_B",
    "pt2_C",
    "pt2_D",
    "pt2_E",
    "pt2_F",
    "pt2_G",
    "pt2_cost",
    "rt1_A",
    "rt1_B",
    "rt1_C",
    "rt1_D",
    "rt1_E",
    "rt1_F",
    "rt1_G"
)

write.csv(shopping_pt1_2, "shopping_pt1-2.csv", row.names=F)


df_row <- nrow(shopping_pt1_2)
train_pt <- shopping_pt1_2[1:round(df_row*0.7)-1,]
valid <- shopping_pt1_2[round(df_row*0.7):df_row,]

write.csv(train_pt, "shopping_pt1-2_train.csv", row.names=F)
write.csv(valid, "shopping_pt1-2_valid.csv", row.names=F)
