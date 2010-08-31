mesh_cat.f<-
function(data=data)
{
#NB. We need a FISHERY_TYPE column to say whether it is mobile or static
mesh_cat<-rep(NA,length(effort[,1]))
mesh_cat[effort$MESHSIZE >=1 & effort$MESHSIZE < 15] <- '<16'
mesh_cat[effort$MESHSIZE >=16 & effort$MESHSIZE < 31] <- '16-31'
mesh_cat[effort$MESHSIZE >=32 & effort$MESHSIZE < 54] <- '32-54'
mesh_cat[effort$MESHSIZE >=55 & effort$MESHSIZE < 69] <- '55-69'
mesh_cat[effort$MESHSIZE >=70 & effort$MESHSIZE < 79] <- '70-79'
mesh_cat[effort$MESHSIZE >=80 & effort$MESHSIZE < 89] <- '80-89'
mesh_cat[effort$MESHSIZE >=90 & effort$MESHSIZE < 99] <- '90-99'
mesh_cat[effort$MESHSIZE >=100 & effort$MESHSIZE < 119] <- '100-119'
mesh_cat[effort$MESHSIZE >=120] <- '>120'
mesh_cat
}

mesh_cat.static.f<-
function(effort=neffort)
{
#neffort$V6[neffort$V6 >=800] <-neffort$V6[neffort$V6 >=800]/10
mesh_cat<-rep(NA,length(effort[,1]))
mesh_cat[effort$MESHSIZE >=1 & effort$MESHSIZE < 31] <- '10-30'
mesh_cat[effort$MESHSIZE >=31 & effort$MESHSIZE < 50] <- '31-49'
mesh_cat[effort$MESHSIZE >=50 & effort$MESHSIZE < 60] <- '50-59'
mesh_cat[effort$MESHSIZE >=60 & effort$MESHSIZE < 70] <- '60-69'
mesh_cat[effort$MESHSIZE >=70 & effort$MESHSIZE < 80] <- '70-79'
mesh_cat[effort$MESHSIZE >=80 & effort$MESHSIZE < 90] <- '80-89'
mesh_cat[effort$MESHSIZE >=90 & effort$MESHSIZE < 100] <- '90-99'
mesh_cat[effort$MESHSIZE >=100 & effort$MESHSIZE < 110] <- '100-109'
mesh_cat[effort$MESHSIZE >=110 & effort$MESHSIZE < 150] <- '110-149'
mesh_cat[effort$MESHSIZE >=150 & effort$MESHSIZE < 219] <- '150-219'
mesh_cat[effort$MESHSIZE >=220] <- '>=220'
mesh_cat
}
