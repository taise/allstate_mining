# prepare test data join each shopping_pt by customer_id

source_dir <- "~/development/mining/allstate/lib/"
source(paste(source_dir, "timeSegment.R", sep=""))

setwd("~/statistics/data/kaggle/allstate")
test_csv <- read.csv("test_v2.csv")

test_csv$segmentedTime  <- as.factor(mapply(timeSegment, test_csv$time))
test_csv$risk_factor    <- as.factor(ifelse(is.na(test_csv$risk_factor), 0, test_csv$risk_factor))


pt1 <- subset(test_csv, shopping_pt == 1)
pt2 <- subset(test_csv, shopping_pt == 2)
rt1 <- data.frame(pt1$customer_ID, pt1$A, pt1$B, pt1$C, pt1$D, pt1$E, pt1$F, pt1$G)
colnames(rt1) <- c("customer_ID", "A", "B", "C", "D", "E", "F", "G")


merge_by_customer_ID <- function(df1, df2, df1_prefix, df2_prefix) {
  df1_names <- paste(df1_prefix, names(df1), sep="")
  df2_names <- paste(df2_prefix, names(df2), sep="")
  
  res <- merge(df1, df2, by="customer_ID", all=TRUE)
  colnames(res) <- c("customer_ID", df1_names[-1], df2_names[-1])
  return(res)
}


pt1_2 <- merge_by_customer_ID(pt1, pt2, "pt1_", "pt2_")
pt_dataset1_2 <- merge_by_customer_ID(pt1_2, rt1, "", "rt1_")
write.csv(pt_dataset1_2, "test_shopping_pt1_2.csv", row.names=F)

