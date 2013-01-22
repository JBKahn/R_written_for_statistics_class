source( "a2kahnjosefunc.R" )

foura <- function(){
  print("pmf demo")
  pmf_testval=c(0,1,2,3,4,5,6,7,8,9,-1,100,5.5)
  for(i in pmf_testval) {
    cat('dsidp(',i,'):', dsidp(i), '\n')
  }
  print('-------------------------------')
  
  print("cdf demo")
  cdf_testval=c(-100,0,1,2,3,4,5,6,7,8,9,23,3.5)
  for(i in cdf_testval) {
    cat('psidp(',i,'):', psidp(i), '\n')
  }
  print('-------------------------------')
  
  print("icdf demo")
  icdf_testval=c(-1,0,1/9,2/9,3/9,4/9,4.1/9,5/9,6/9,7/9,8/9,1,10)
  for(i in icdf_testval) {
    cat('qsidp(',i,'):', qsidp(i), '\n')
  }
  print('-------------------------------')
}

fourb <- function(){
  y=c()
  for(i in 1:20) {
    y=append(y,mean(rsidp(i)))
  }
  plot(y, ann=FALSE, ylim=c(1,20), xaxt='n')
  axis(1, las=1, at=c(1:20))
  title(main="4b", xlab="Sample Size", ylab="Mean")
}

fourc <- function(){
  y=c()
  for(i in 2:20) {
    y=append(y,var(rsidp(i)))
  }
  plot(y, ann=FALSE, xaxt='n')
  axis(1, las=1, at=c(1:19), labels=c(2:20), )
  title(main="4c", xlab="Sample Size", ylab="variance")
}

foura()
fourb()
fourc()