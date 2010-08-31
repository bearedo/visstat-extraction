
qtr.f <- function(dat=dat){
qtr=rep(NA,length(dat[,1]))
qtr[dat$MONTH < 4] = 1
qtr[dat$MONTH  > 3 & dat$MONTH  < 7] = 2
qtr[dat$MONTH  > 6 & dat$MONTH  < 10] = 3
qtr[dat$MONTH  > 9 & dat$MONTH  <= 12]= 4
dat$quarter=qtr
dat
}
