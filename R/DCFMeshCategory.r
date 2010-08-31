DCFMeshCategory<-
function(data=data)
{
#NB. We need a FISHERY_TYPE column to say whether it is mobile or static

mob <- data[data$FISHERY_TYPE == 'mobile',]


mesh_cat<-rep(NA,length(mob[,1]))


mesh_cat[mob$MESHSIZE>=1 & mob$MESHSIZE < 15] <- '<16'
mesh_cat[mob$MESHSIZE >=16 & mob$MESHSIZE < 31] <- '16-31'
mesh_cat[mob$MESHSIZE >=32 & mob$MESHSIZE < 54] <- '32-54'
mesh_cat[mob$MESHSIZE >=55 & mob$MESHSIZE < 69] <- '55-69'
mesh_cat[mob$MESHSIZE >=70 & mob$MESHSIZE < 79] <- '70-79'
mesh_cat[mob$MESHSIZE >=80 & mob$MESHSIZE < 89] <- '80-89'
mesh_cat[mob$MESHSIZE >=90 & mob$MESHSIZE < 99] <- '90-99'
mesh_cat[mob$MESHSIZE >=100 & mob$MESHSIZE < 119] <- '100-119'
mesh_cat[mob$MESHSIZE >=120] <- '>120'

mob$MESH_SIZE_RANGE <- mesh_cat

pass <- data[data$FISHERY_TYPE == 'passive',]


mesh_cat<-rep(NA,length(pass[,1]))


mesh_cat[pass$MESHSIZE >=1 & pass$MESHSIZE < 31] <- '10-30'
mesh_cat[pass$MESHSIZE >=31 & pass$MESHSIZE < 50] <- '31-49'
mesh_cat[pass$MESHSIZE >=50 & pass$MESHSIZE < 60] <- '50-59'
mesh_cat[pass$MESHSIZE >=60 & pass$MESHSIZE < 70] <- '60-69'
mesh_cat[pass$MESHSIZE >=70 & pass$MESHSIZE < 80] <- '70-79'
mesh_cat[pass$MESHSIZE >=80 & pass$MESHSIZE < 90] <- '80-89'
mesh_cat[pass$MESHSIZE >=90 & pass$MESHSIZE < 100] <- '90-99'
mesh_cat[pass$MESHSIZE >=100 & pass$MESHSIZE < 110] <- '100-109'
mesh_cat[pass$MESHSIZE >=110 & pass$MESHSIZE < 150] <- '110-149'
mesh_cat[pass$MESHSIZE >=150 & pass$MESHSIZE < 219] <- '150-219'
mesh_cat[pass$MESHSIZE >=220] <- '>=220'
pass$MESH_SIZE_RANGE <- mesh_cat

data <- rbind(mob,pass)

data
}