
FrisbeDatesToRDates <- function(input=sns) 
{
input$month<-as.numeric(format(input$STN_DATE,"%m"))
input$year<-as.numeric(format(input$STN_DATE,"%Y"))

input$day<-as.numeric(format(input$STN_DATE,"%d"))
tdate<-paste(input$year,"-",input$month,"-",input$day,sep="")
hh<-substr(format(input$TIME),1,2)
hh[grep("  ",hh)] <- "00"
mm<-  substr(format(input$TIME),3,4)
mm[grep("  ",mm)] <- "00"
ttime<-paste(paste(hh,mm,sep=":"),":","00",sep="")
input$date<-paste(tdate,ttime)
input$date=as.POSIXct(input$date)
input
}