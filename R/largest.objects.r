largest.objects <- function(which.ones=ls(),n=10){
## find the 10 largest objects in R
ll=length(which.ones)
z <- sapply(which.ones[1:ll], function(x) object.size(get(x)))
as.matrix(rev(sort(z))[1:n])
}
