

GetPlatformProperties<-function(which.database='frisbe',user=user,passwd=passwd){

if(which.database=='visstat'){
visstat <- dBConnect(which.lib='RODBC',which.database="visstat",user=user,passwd=passwd)
pp   <- sqlQuery(visstat,"SELECT * from platform_properties;")   #on windoze
pp$ID <- as.character(pp$ID)
pp$PLM_CODE <- as.character(pp$PLM_CODE)  }

if(which.database=='frisbe'){
frisbe <- dBConnect(which.lib='RODBC',which.database="frisbe")
pp   <- sqlQuery(frisbe,"SELECT * from vis_platform_properties;")   #on windoze
pp$NAME <- as.character(pp$NAME)
pp$ID <- as.character(pp$ID)  }


pp
}


#pp <- GetPlatformProperties()