

qtrlowercase.f <- function(dat=dat){
qtr=rep(NA,length(dat[,1]))
qtr[dat$month < 4] = 1
qtr[dat$month  > 3 & dat$month  < 7] = 2
qtr[dat$month  > 6 & dat$month  < 10] = 3
qtr[dat$month  > 9 & dat$month  <= 12]= 4
dat$quarter=qtr
dat
}
