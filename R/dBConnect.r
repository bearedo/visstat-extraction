dBConnect<-function(which.lib = "RODBC", which.database="visstat",user=user,passwd=passwd){
	
	if(missing(user)) stop("Must specify 'user'")
	if(missing(password)) stop("Must specify 'password'")
	
  #Options for libraries are RODBC and DBI (DBI is set up to work on raja and RODBC usually on a windoze box
  if(which.lib=="RODBC"){
  library(RODBC)
  odbcCloseAll()
  
	if(which.database=="visstat"){
   conn <- odbcConnect(dsn="visstatp", uid=user,pwd=passwd,believeNRows=F)
   }
  if(which.database=="frisbe"){
  conn<-odbcConnect(dsn="frisbep",uid=user,pwd=passwd,believeNRows=F)
  }
  }
  if(which.lib=="DBI"){
  library(DBI);library(ROracle)                    
  if(which.database=="visstat"){
  drv <- dbDriver("Oracle")
  conn<-dbConnect(drv,user=user,password=passwd,db="visstatp")
  }
  if(which.database=="frisbe"){
  conn<-dbConnect(drv,user=user,password=passwd,db="frisbep")
  }
  }
	conn
}

                                                            