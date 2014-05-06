score <- function(vc1, vc2) {
  res <- table(vc1 == vc2)
  print(res)
  print("score:")
  print(res[[2]] / (res[[1]] + res[[2]]))
}