#Format time string
ReformatTime<-function(timestring=timestring){
# Visstat time of day formats at a bit odd so this function reformats them for use with as.Posixct etc.
tim<-format(timestring)
hr<-substr(tim,1,2)
hr<-ifelse(hr=="  ","00",hr)
mi<-substr(tim,3,4)
mi<-ifelse(mi==" 0","00",mi)
se<-"00"
ntim<-paste(hr,mi,se,sep=":")
ntim
}