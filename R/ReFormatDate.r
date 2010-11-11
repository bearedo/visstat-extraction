

ReformatDate<- function(datestring=datestring)
 {
#Convert from YYYY-MM-DD
#This when preparing eflalo and tacsat data
#datestring <- vms$RGN_UTC_DATE
datestring <- format(datestring)


yr  <-substr(datestring,1,4)
mon <-substr(datestring,6,7)
day <-substr(datestring,9,10)

ndate <- paste(day,mon,yr,sep="/")

ndate

}