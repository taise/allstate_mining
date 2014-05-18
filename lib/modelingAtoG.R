library(rpart)

modelingAtoG <- function(df) {
  model <- ""
  vect <- c("A", "B", "C", "D", "E", "F", "G")
  
  for(v in vect) {
    print(v)
    trainDf <- df
    
    trainDf$customer_ID <- NULL
    trainDf$pt1_state <- NULL
    trainDf$pt2_state <- NULL
    rm_vect <- subset(vect, vect != v)
    
    for(null_col in rm_vect) {
      eval(parse(text=paste("trainDf$rt_", null_col, " = NULL", sep="")))
    }
    eval(parse(text=paste("model$", v, " <- execRpart(trainDf, v)", sep="")))
  }
  return(model)
}

execRpart <- function(df, target) {
  rpartStr <- paste("rpart(rt1_", target, "~., data=df, maxdepth=4, cp=-1)", sep="")
  result <- paste("tree.", target, sep="")
  
  eval(parse(text=paste(result, " <- ", rpartStr, sep="")))
  return(eval(parse(text=result)))
}