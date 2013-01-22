dsidp <- function(d){
  n=c(0,1,2,3,4,5,6,7,8,9)
  p=c(0,0,0,1,0,2,0,1,2,3)
  if (!(is.element(d,n))) 
    return(0)
  return((p[which(n == d)])/(sum(p)))
}

psidp <- function(d){
  d = floor(d)
  if (d < 0) 
    return(0)
  if (d > 9) 
      return(1)
  n=c(0,1,2,3,4,5,6,7,8,9)
  p=c(0,0,0,1,0,2,0,1,2,3)
  return(sum(p[1:which(n == d)])/(sum(p)))
}

qsidp <- function(q){
  if (q <0)
    return("p can't be less than 0")
  if (q <1/9)
    return(0)
  if (q <3/9)
    return(3)
  if (q <4/9)
    return(5)
  if (q <6/9)
    return(7)
  if (q <1)
    return(8)
  if (q == 1)
    return(9)
  return("p can't be greater than 1")                
}

rsidp <- function(n){
  d=c(0,1,2,3,4,5,6,7,8,9)
  p=c(0,0,0,1,0,2,0,1,2,3)
  return(sample(x = d, size = n, prob = p, replace=T))
}
