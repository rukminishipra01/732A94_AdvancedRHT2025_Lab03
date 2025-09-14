euclidean <- function(a, b) {
  if(!is.numeric(a) || length(a)!=1){
    stop("a must be a numeric scalar")
  } else if(!is.numeric(b) || length(b)!=1){
    stop("b must be a numeric scalar")
  }
  while(b != 0){
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}
