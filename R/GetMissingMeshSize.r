
GetMissingMeshSize <- function(data=tacsat,latitude.name="SI_LATI")

{

#Mesh fudge function
#Zeros
#If a beamer is fishing south of 55 and mesh size is 0 then it gets to be 80mm (sole)

data$MESHSIZE[data$MESHSIZE==0 & data$GPY_CODE == "TBB" & data[,latitude.name] <= 55] <-80

#If a beamer is fishing north of 55 and south of 56 and mesh size is NA then it gets to be 100mm (plaice)

data$MESHSIZE[data$MESHSIZE==0 & data$GPY_CODE == "TBB" & data[,latitude.name] >55 & data[,latitude.name] <=56] <- 100

#If a beamer is fishing north of 56 and mesh size is NA then it gets to be 120mm (plaice)

data$MESHSIZE[data$GPY_CODE == "TBB" & data$MESHSIZE==0 & data[,latitude.name] >56 & data[,latitude.name] <=60] <- 120

#If a beamer is fishing north of 55 and south of 56 and mesh size is NA then it gets to be 100mm (plaice)

data$MESHSIZE[ data$GPY_CODE == "TBB" & data$MESHSIZE==0 & data[,latitude.name] >55 & data[,latitude.name] <=56] <- 100


#BEAMERS missing meshes:

#If a beamer is fishing south of 55 and mesh size is NA then it gets to be 80mm (sole)

data$MESHSIZE[is.na(data$MESHSIZE) & data$GPY_CODE == "TBB" & data[,latitude.name] <= 55] <-80

#If a beamer is fishing north of 55 and south of 56 and mesh size is NA then it gets to be 100mm (plaice)

data$MESHSIZE[is.na(data$MESHSIZE) & data$GPY_CODE == "TBB" & data[,latitude.name] >55 & data[,latitude.name] <=56] <- 100

#If a beamer is fishing north of 56 and mesh size is NA then it gets to be 120mm (plaice)

data$MESHSIZE[data$GPY_CODE == "TBB" & is.na(data$MESHSIZE) & data[,latitude.name] >56 & data[,latitude.name] <=60] <- 120

#If a beamer is fishing north of 55 and south of 56 and mesh size is NA then it gets to be 100mm (plaice)

data$MESHSIZE[ data$GPY_CODE == "TBB" & is.na(data$MESHSIZE) & data[,latitude.name] >55 & data[,latitude.name] <=56] <- 100

#OTTERS missing meshes:

#If a otter is fishing south of 55 and mesh size is NA then it gets to be 80mm (sole)

data$MESHSIZE[data$MESHSIZE ==0 & data$GPY_CODE == "OTB" & data[,latitude.name] <= 55] <-80

#If a otter is fishing north of 55 and south of 56 and mesh size is NA then it gets to be 100mm (plaice)

data$MESHSIZE[data$MESHSIZE==0 & data$GPY_CODE == "OTB" & data[,latitude.name] >55 & data[,latitude.name] <=56] <- 100

#If a otter is fishing north of 56 and mesh size is NA then it gets to be 120mm (plaice)

data$MESHSIZE[data$GPY_CODE == "OTB" & data$MESHSIZE==0 & data[,latitude.name] >56 & data[,latitude.name] <=60] <- 120


#OTTERS missing meshes:

#If a otter is fishing south of 55 and mesh size is NA then it gets to be 80mm (sole)

data$MESHSIZE[is.na(data$MESHSIZE) & data$GPY_CODE == "OTB" & data[,latitude.name] <= 55] <-80

#If a otter is fishing north of 55 and south of 56 and mesh size is NA then it gets to be 100mm (plaice)

data$MESHSIZE[is.na(data$MESHSIZE) & data$GPY_CODE == "OTB" & data[,latitude.name] >55 & data[,latitude.name] <=56] <- 100

#If a otter is fishing north of 56 and mesh size is NA then it gets to be 120mm (plaice)

data$MESHSIZE[data$GPY_CODE == "OTB" & is.na(data$MESHSIZE) & data[,latitude.name] >56 & data[,latitude.name] <=60] <- 120

#If a shrimper and mesh is missing then it gets to be 20mm

#If a shrimper and mesh is missing then it gets to be 20mm

data$MESHSIZE[!is.na(data$MESHSIZE) & data$GPY_CODE == "TBS"] <- 20
data$MESHSIZE[data$MESHSIZE ==0 & data$GPY_CODE == "TBS"] <- 20


data

}