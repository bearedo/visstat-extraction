
Nephrops.FU <- function (data=data) {

data$NEPFU <- rep(NA,length(data[,1]))
data$NEPFUName <- rep(NA,length(data[,1]))


data$NEPFU[data$QUADRANT %in% c('36F1','36F2','36F3','36F4','37F1','37F2','37F3','37F4','35F2','35F3')]<- "5"
data$NEPFU[data$QUADRANT %in% c('38E8','38E9','40E8','40E9','37E9')]<- "6"
data$NEPFU[data$QUADRANT %in% c(paste(44:49,'E9',sep=""),paste(44:49,'F1',sep=""),paste(45:46,'E8',sep=''))]<- "7"
data$NEPFU[data$QUADRANT %in% c(paste(40:41,'E7',sep=""),'41E6')]<- "8"
data$NEPFU[data$QUADRANT %in% c(paste(44:45,'E6',sep=""),paste(44:45,'E7',sep=""),'44E8')] <- "9"
data$NEPFU[data$QUADRANT %in% '47E6'] <- "10"
data$NEPFU[data$QUADRANT %in% c(paste(44:52,'F2',sep=""),paste(44:52,'F3',sep=""),paste(44:52,'F4',sep="")
,paste(44:52,'F5',sep="") ,paste(44:52,'F6',sep=""),'43F5','43F6','43F7')] <- "32"
data$NEPFU[data$QUADRANT %in% c(paste(39:41,'F4',sep=""),paste(39:41,'F5',sep=""))]<- "33"
data$NEPFU[is.na(data$NEPFU)] <- "OTH"

data$NEPFUName[data$QUADRANT %in% c('36F1','36F2','36F3','36F4','37F1','37F2','37F3','37F4','35F2','35F3')]<- "BotneyGut-SilverPit"
data$NEPFUName[data$QUADRANT %in% c('38E8','38E9','40E8','40E9','37E9')]<- "FarnDeeps"
data$NEPFUName[data$QUADRANT %in% c(paste(44:49,'E9',sep=""),paste(44:49,'F1',sep=""),paste(45:46,'E8',sep=''))]<- "FladenGround"
data$NEPFUName[data$QUADRANT %in% c(paste(40:41,'E7',sep=""),'41E6')]<- "FirthOfForth"
data$NEPFUName[data$QUADRANT %in% c(paste(44:45,'E6',sep=""),paste(44:45,'E7',sep=""),'44E8')] <- "MorayFirth"
data$NEPFUName[data$QUADRANT %in% '47E6'] <- "Noup"
data$NEPFUName[data$QUADRANT %in% c(paste(44:52,'F2',sep=""),paste(44:52,'F3',sep=""),paste(44:52,'F4',sep="")
,paste(44:52,'F5',sep="") ,paste(44:52,'F6',sep=""),'43F5','43F6','43F7')] <- "NorwegianDeeps"
data$NEPFUName[data$QUADRANT %in% c(paste(39:41,'F4',sep=""),paste(39:41,'F5',sep=""))]<- "OffHornsReef"
data$NEPFUName[is.na(data$NEPFUName)] <- "OTH"

data
}