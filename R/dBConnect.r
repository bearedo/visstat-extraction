dBConnect<-function(which.lib = "DBI", which.database="visstat"){
	
  #Options for libraries are RODBC and DBI (DBI is set up to work on raja and RODBC usually on a windoze box
  if(which.lib=="RODBC"){
  library(RODBC)
  odbcCloseAll()
  
	if(which.database=="visstat"){
   conn <- odbcConnect(dsn="visstatp", uid="doug",pwd="oneover")
   }
  if(which.database=="frisbe"){
  conn<-odbcConnect(dsn="frisbep",uid="doug",pwd="ninethirty")
  }
  }
  if(which.lib=="DBI"){
  library(DBI);library(ROracle)                    
  if(which.database=="visstat"){
  drv <- dbDriver("Oracle")
  conn<-dbConnect(drv,user="doug",password="oneover",db="visstatp")
  }
  if(which.database=="frisbe"){
  conn<-dbConnect(drv,user="doug",password="ninethirty",db="frisbep")
  }
  }
	conn
}

                                                            