source_dir <- "~/development/mining/allstate/lib/"
source(paste(source_dir, "modelingAtoG.R", sep=""))

modelingByLocation <- function(df, state) {
  trainState <- subset(df, pt1_state == state)
  return(modelingAtoG(trainState))
}
