
 
GetInfoFrisbe <- function() {
 
frisbe <- dBConnect(which.database="frisbe") 

#Get VIS_STATIONS  

qst <- paste("select VIS_STATIONS.* FROM VIS_STATIONS WHERE rownum < 5")
 
 print(sqlQuery(frisbe,qst))  

  print("VIS_STATIONS")
 
 # Get VIS_POSITIONS 
  
qvp <- paste("select * FROM VIS_POSITIONS WHERE rownum < 5")
 
 print(sqlQuery(frisbe,qvp))    

 print("VIS_POSITIONS")
 
 #Get VIS_SAMPLES

qsa <- paste("select VIS_SAMPLES.* FROM VIS_SAMPLES WHERE rownum < 5")
 
 print(sqlQuery(frisbe,qsa))    

 print("VIS_SAMPLES")

#Get VIS_SUBSAMPLES

qss <- paste("select VIS_SUBSAMPLES.* FROM VIS_SUBSAMPLES WHERE rownum < 5;")
 
 print(sqlQuery(frisbe,qss))  
 
 print("VIS_SUBSAMPLES")  

#Get VIS_CLASSES

qcl <- paste("select VIS_CLASSES.* FROM VIS_CLASSES WHERE rownum < 5")
 print(sqlQuery(frisbe,qcl))
 print("VIS_CLASSES")    
  
#Get VIS_TAXONS

qta <- paste("select * FROM VIS_TAXONS where rownum < 5 ")
 print(sqlQuery(frisbe,qta) )   
 print("VIS_TAXONS") 
  
} 
  
  