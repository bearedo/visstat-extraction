
 
GetInfoVisstat <- function() 
{
 
visstat <- dBConnect(which.database="visstat") 

#Get CATCHES  

qca <- paste("select * FROM CATCHES WHERE rownum < 5")
 
 print(sqlQuery(visstat,qca))  

  print("CATCHES")
 
 # Get GEAR PROPERTIES
  
qgp <- paste("select * FROM GEAR_PROPERTIES WHERE rownum < 5")
 
 print(sqlQuery(visstat,qgp))    

 print("GEAR PROPERTIES")
 
 #Get PLATFORM PROPERTIES

qpp <- paste("select * FROM PLATFORM_PROPERTIES WHERE rownum < 5")
 
 print(sqlQuery(visstat,qpp))    

 print("PLATFORM PROPERTIES")

#Get REGISTRATIONS

qre <- paste("select * FROM REGISTRATIONS WHERE rownum < 5")
 
 print(sqlQuery(visstat,qre))  
 
 print("REGISTRATIONS")  

 #Get TRIPS

qtr <- paste("select * FROM TRIPS WHERE rownum < 5")
 
 print(sqlQuery(visstat,qtr))  
 
 print("TRIPS")  

#Get TAXONS

qtx <- paste("select * FROM TAXONS WHERE rownum < 5")
 
 print(sqlQuery(visstat,qtx))  
 
 print("TAXONS")  
 
 qtp <- paste("select * FROM QUADRANT_PROPERTIES WHERE rownum < 5")
 
 print(sqlQuery(visstat,qtp))  
 
 print("TAXONS")  

  
} 
  
  