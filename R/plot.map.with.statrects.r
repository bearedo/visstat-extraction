plot.map.with.statrects   <-
function(lon.limits=c(-10,10),lat.limits=c(50,60)){
x=29:88
y=c(paste("C",0:9,sep=""),paste("D",0:9,sep=""),paste("E",0:9,sep=""),
paste("F",0:9,sep=""),paste("G",0:9,sep=""),paste("H",0:9,sep=""))
lonny=seq(-29.5,29.5,by=1)
latty=seq(50.25,80,by=0.5)
#par(mfrow=c(1,1),mar=c(3,3,3,3))

plot(lonny,latty,type="n",xlim=lon.limits,ylim=lat.limits,xlab="",ylab="",xaxt="n",yaxt="n")
library(maps);library(mapdata)
abline(v=seq(-20,20,by=1),lty=2)
abline(h=seq(35,80,by=.5),lty=2)

map('worldHires',xlim=c(-35,35),ylim=c(25,85),add=T,fil=T,col="gray")
text(lonny,lat.limits[2]+.25,as.character(y))
text(lon.limits[1]-.25,latty,as.character(x))
}
