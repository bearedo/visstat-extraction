

plot.eflalo.on.statsquare <- function(eflalo=eflalo,min.lon=-20,max.lon=20,min.lat=45,max.lat=65,species="KG_SOL",log.it=T)

{

#Default is to plot the total effort (kwhours) in the eflalo data and the total catch or value for a particular species

library(fields);library(maps);library(mapdata);library(vmstools)

step.lon<-1
step.lat<-0.5

adj.x <- 0.5
adj.y <- 0.25

grid <- expand.grid(lat=seq(min.lat+adj.y,max.lat+adj.y,by=step.lat),lon=seq(min.lon+adj.x,max.lon+adj.x,by=step.lon))  #create grid

statsq <- ICESrectangle(data.frame(SI_LONG=grid$lon,SI_LATI=grid$lat))        #put in ICES statistical rectangle
grid$statsq <- statsq

wo <- grep(species,dimnames(eflalo)[[2]])           #Find species required

eflalo1 <- aggregate(list(LE_EFF=eflalo[,"LE_EFF"],FISH=eflalo[,wo]),list(LE_RECT=eflalo$LE_RECT),sum,na.rm=T)

grid$LE_EFF <- eflalo1[,"LE_EFF"][match(grid$statsq,eflalo1$LE_RECT)]  #match data onto grid
grid$FISH <- eflalo1[,"FISH"][match(grid$statsq,eflalo1$LE_RECT)]
 
lx<-seq(min.lon,max.lon,by=step.lon)
lx <- lx+adj.x
llx<-length(lx) 
ly<-seq(min.lat,max.lat,by=step.lat) 
ly <- ly+adj.y
lly<-length(ly)

mat.LE_EFF<-matrix(grid$LE_EFF,ncol=llx,nrow=lly,byrow=T)         #convert to a matrix reqd by image.plot

mat.FISH<-matrix(grid$FISH,ncol=llx,nrow=lly,byrow=T)

if(log.it){

#Effort
rr <- range(mat.LE_EFF+1,na.rm=T)
image.plot(lx,ly,log(mat.LE_EFF),zlim=log(rr),xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(-5,15),ylim=c(45,65)) 
map('worldHires',add=T,fill=T,col='beige');box(lty=1)
#Fish
rr <- range(mat.FISH+1,na.rm=T)
image.plot(lx,ly,log(mat.FISH),zlim=log(rr),xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(-5,15),ylim=c(45,65)) 
map('worldHires',add=T,fill=T,col='beige');box(lty=1)
}

else{

#Effort
rr <- range(mat.LE_EFF,na.rm=T)
image.plot(lx,ly,mat.LE_EFF,zlim=rr,xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(-5,15),ylim=c(45,65)) 
map('worldHires',add=T,fill=T,col='beige');box(lty=1)
#Fish
rr <- range(mat.FISH+1,na.rm=T)
image.plot(lx,ly,mat.FISH,zlim=rr,xlab="",ylab="",xaxt="n",yaxt="n",xlim=c(-5,15),ylim=c(45,65)) 
map('worldHires',add=T,fill=T,col='beige');box(lty=1)
}

}


#load("n:/projecten/cod-closures-2011/eflalo.06.rda")

#par(mfrow=c(2,1))
#plot.eflalo.on.statsquare(eflalo=eflalo.06[eflalo.06$LE_MET_level6 == 'TBB_DEF_70-99_0_0',],min.lon=-20,max.lon=20,min.lat=45,max.lat=65,species="KG_NEP",log.it=F)
#plot.eflalo.on.statsquare(eflalo=eflalo.06[eflalo.06$LE_MET_level6 == 'OTB_DEF_70-99_0_0',],min.lon=-20,max.lon=20,min.lat=45,max.lat=65,species="EURO_NEP",log.it=F)

