DCFMeshCategory<-
function(input=eflalo2,data.type="visstat")
{
#NB. We need a FISHERY_TYPE column to say whether it is mobile or static


if(data.type=="visstat"){

mob <- input[input$FISHERY_TYPE == 'mobile',]
print(dim(mob))

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

pass <- input[input$FISHERY_TYPE %in% c('unknown','passive'),]
print(dim(pass))

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

out <- rbind(mob,pass)
print(dim(out))

}

if(data.type=="eflalo"){

mob <- input[input$FISHERY_TYPE == 'mobile',]
print(dim(mob))

mesh_cat<-rep(NA,length(mob[,1]))


mesh_cat[mob$LE_MSZ>=1 & mob$LE_MSZ < 15] <- '<16'
mesh_cat[mob$LE_MSZ >=16 & mob$LE_MSZ < 31] <- '16-31'
mesh_cat[mob$LE_MSZ >=32 & mob$LE_MSZ < 54] <- '32-54'
mesh_cat[mob$LE_MSZ >=55 & mob$LE_MSZ < 69] <- '55-69'
mesh_cat[mob$LE_MSZ >=70 & mob$LE_MSZ < 79] <- '70-79'
mesh_cat[mob$LE_MSZ >=80 & mob$LE_MSZ < 89] <- '80-89'
mesh_cat[mob$LE_MSZ >=90 & mob$LE_MSZ < 99] <- '90-99'
mesh_cat[mob$LE_MSZ >=100 & mob$LE_MSZ < 119] <- '100-119'
mesh_cat[mob$LE_MSZ >=120] <- '>120'

mob$MESH_SIZE_RANGE <- mesh_cat

pass <- input[input$FISHERY_TYPE %in% c('unknown','passive'),]
print(dim(pass))

mesh_cat<-rep(NA,length(pass[,1]))

mesh_cat[pass$LE_MSZ >=1 & pass$LE_MSZ < 31] <- '10-30'
mesh_cat[pass$LE_MSZ >=31 & pass$LE_MSZ < 50] <- '31-49'
mesh_cat[pass$LE_MSZ >=50 & pass$LE_MSZ < 60] <- '50-59'
mesh_cat[pass$LE_MSZ >=60 & pass$LE_MSZ < 70] <- '60-69'
mesh_cat[pass$LE_MSZ >=70 & pass$LE_MSZ < 80] <- '70-79'
mesh_cat[pass$LE_MSZ >=80 & pass$LE_MSZ < 90] <- '80-89'
mesh_cat[pass$LE_MSZ >=90 & pass$LE_MSZ < 100] <- '90-99'
mesh_cat[pass$LE_MSZ >=100 & pass$LE_MSZ < 110] <- '100-109'
mesh_cat[pass$LE_MSZ >=110 & pass$LE_MSZ < 150] <- '110-149'
mesh_cat[pass$LE_MSZ >=150 & pass$LE_MSZ < 219] <- '150-219'
mesh_cat[pass$LE_MSZ >=220] <- '>=220'
pass$MESH_SIZE_RANGE <- mesh_cat

out <- rbind(mob,pass)
print(dim(out))

}

out
}