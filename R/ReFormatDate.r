

ReformatDate<- function(datestring=datestring,which.lib=which.lib)
 {
#Convert from YYYY-MM-DD
#This when preparing eflalo and tacsat data
#datestring <- vms$RGN_UTC_DATE
datestring <- format(datestring)
if(which.lib=="RODBC"){
yr  <-substr(datestring,1,4)
mon <-substr(datestring,6,7)
day <-substr(datestring,9,10)                                    
ndate <- paste(day,mon,yr,sep="/")
}
if(which.lib=="DBI"){
yr  <-substr(datestring,7,10)
mon <-substr(datestring,4,5)
day <-substr(datestring,1,2)
ndate <- paste(day,mon,yr,sep="/")
}
ndate
}