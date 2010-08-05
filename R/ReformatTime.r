#Format time string
ReformatTime<-function(timestring=timestring){
# Visstat time of day formats at a bit odd so this function reformats them for use with as.Posixct etc.
timestring <- valuebytrip$DEPARTURE_TIME
tim<-format(timestring)
hr<-substr(tim,1,2)
hr<-ifelse(hr=="  ","00",hr)
mi<-substr(tim,3,4)
mi<-ifelse(mi==" 0","00",mi)
dsec<-"00"
ntim<-paste(dhr,dmin,dsec,sep=":")
ntim
}