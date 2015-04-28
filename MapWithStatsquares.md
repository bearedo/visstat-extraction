#function to map data by ICES rectangle

# Introduction #

Function to map data by ICES rectangle. The column with ICES rectangles has to be named 'AREA\_CODE'. The colors can be based on log transformed data (may give nicer maps) or linear. An example is given below.

data        = your data;
plot.data   = the column name with the data to be plotted;
log.col     = if True the colors are plotted on a log scale;
plot.ices   = if True the ICES rectangles name is plotted in the squares;
plot.value  = if True, the values of plot.data are plotted in the squares;
title       = title of the plot;
min.lon     = minimal longitude to be plotted;
max.lon     = maximal longitude to be plotted;
min.lat     = minimal latitude to be plotted;
max.lat     = maximal latitude to be plotted;


# Details #

```

##### Function
map.with.statsquares <-
function (data = data, min.lon = -4, max.lon = 10, min.lat = 50,
max.lat = 58, plot.data = plot.data, log.col = F, plot.ices=T, plot.value=T, title="")
{
library(fields)
library(maps)
library(mapdata)
library(vmstools)
step.lon <- 1
step.lat <- 0.5
adj.x <- 0.5
adj.y <- 0.25
grid <- expand.grid(lat = seq(min.lat + adj.y, max.lat +
adj.y, by = step.lat), lon = seq(min.lon + adj.x, max.lon +
adj.x, by = step.lon))
statsq <- ICESrectangle(data.frame(SI_LONG = grid$lon, SI_LATI = grid$lat))
grid$statsq <- statsq
wo <- grep(plot.data, dimnames(data)[[2]])
eflalo1 <- aggregate(list(FISH = data[,
wo]), list(AREA_CODE = data$AREA_CODE), sum, na.rm = T)

grid$FISH <- eflalo1[, "FISH"][match(grid$statsq, eflalo1$AREA_CODE)]
lx  <- seq(min.lon, max.lon, by = step.lon)
lx  <- lx + adj.x
llx <- length(lx)
ly  <- seq(min.lat, max.lat, by = step.lat)
ly  <- ly + adj.y
lly <- length(ly)

mat.FISH <- matrix(grid$FISH, ncol = lly, nrow = llx, byrow = T)

if(log.col){

rr <- range(1,mat.FISH+1,na.rm=T)
image(lx,ly,log(mat.FISH),zlim=log(rr),xlab="longitude",
ylab="latitude",xlim = c(min.lon, max.lon),
ylim = c(min.lat, max.lat),col=rev(heat.colors(100)), main = title)
}

else{

rr <- range(0,mat.FISH,na.rm=T)
image(lx,ly,mat.FISH,zlim=rr,xlab="longitude",
ylab="latitude",xlim = c(min.lon, max.lon),
ylim = c(min.lat, max.lat),col=rev(heat.colors(100)), main = title)
}

box(lty = 1)

if(plot.value){

text(grid$lon,grid$lat-0.1,as.character(round(grid$FISH)),col='black',cex=.6, font=2)

}

if(plot.ices){

text(grid$lon,grid$lat+0.1,as.character(grid$statsq),col='grey27',cex=.5,font=3)

}
map("worldHires", add = T, fill = T, col = "darkgrey")
abline(v = seq(-20, 20, by = 1), lty = 2,col='lightgray')
abline(h = seq(35, 80, by = 0.5), lty = 2,col='lightgray')
}

## end function

##################################################
# EXAMPLE
AREA_CODE<-paste(rep(c(33:41),8),"F",c(0:7), sep="")
data<-data.frame(x=sample(1:100, size=length(AREA_CODE)),AREA_CODE)

map.with.statsquares(data = data, min.lon = -4, max.lon = 10, min.lat = 50,max.lat = 58,
plot.data = 'x', log.col = F, plot.ices=T, plot.value=T, title='example')

```