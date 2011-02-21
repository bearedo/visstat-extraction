


DCFMeshCategory<- function(input=eflalo2,data.type="visstat")
{
#NB. We need a FISHERY_TYPE column to say whether it is mobile or static

input$MESH_SIZE_RANGE<-rep(NA,length(input[,1]))

if(data.type=="visstat"){

print(data.type)

ww <- (1:length(input[,1]))[input$FISHERY_TYPE == 'mobile']

input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=1 & input$MESHSIZE[ww]  < 15] <- '<16'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=16 & input$MESHSIZE[ww] < 31] <- '16-31'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=32 & input$MESHSIZE[ww] < 54] <- '32-54'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=55 & input$MESHSIZE[ww] < 69] <- '55-69'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=70 & input$MESHSIZE[ww] < 79] <- '70-79'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=80 & input$MESHSIZE[ww] < 89] <- '80-89'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=90 & input$MESHSIZE[ww] < 99] <- '90-99'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=100 & input$MESHSIZE[ww] < 119] <- '100-119'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=120] <- '>=120'

print(length(ww))

ww <- (1:length(input[,1]))[input$FISHERY_TYPE %in% c('unknown','passive')]

input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=10 & input$MESHSIZE[ww]  <= 30] <- '10-30'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=31 & input$MESHSIZE[ww] <=49]   <- '31-49'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=50 & input$MESHSIZE[ww] <= 59]  <- '50-59'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=60 & input$MESHSIZE[ww] <= 69] <- '60-69'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=70 & input$MESHSIZE[ww] <= 79] <- '70-79'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=80 & input$MESHSIZE[ww] <= 89] <- '80-89'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=90 & input$MESHSIZE[ww] <= 99] <- '90-99'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=100 & input$MESHSIZE[ww] <= 109] <- '100-109'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=110 & input$MESHSIZE[ww] <= 149] <- '110-149'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=150 & input$MESHSIZE[ww] <= 219] <- '150-219'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=220] <- '>=220'

print(length(ww))

}

if(data.type=="eflalo"){
print(data.type)

ww <- (1:length(input[,1]))[input$FISHERY_TYPE == 'mobile']

input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=1 & input$LE_MSZ[ww]   < 15] <- '<16'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=16 & input$LE_MSZ[ww] < 31] <- '16-31'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=32 & input$LE_MSZ[ww] < 54] <- '32-54'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=55 & input$LE_MSZ[ww] < 69] <- '55-69'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=70 & input$LE_MSZ[ww] < 79] <- '70-79'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=80 & input$LE_MSZ[ww] < 89] <- '80-89'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=90 & input$LE_MSZ[ww] < 99] <- '90-99'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=100 & input$LE_MSZ[ww] < 119] <- '100-119'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=120] <- '>=120'

print(length(ww))

ww <- (1:length(input[,1]))[input$FISHERY_TYPE %in% c('unknown','passive')]

input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=10 & input$LE_MSZ[ww]   <= 30] <- '10-30'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=31 & input$LE_MSZ[ww]  <=49]  <- '31-49'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=50 & input$LE_MSZ[ww] <= 59] <- '50-59'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=60 & input$LE_MSZ[ww] <= 69] <- '60-69'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=70 & input$LE_MSZ[ww] <= 79] <- '70-79'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=80 & input$LE_MSZ[ww] <= 89] <- '80-89'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=90 & input$LE_MSZ[ww] <= 99] <- '90-99'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=100 & input$LE_MSZ[ww] <= 109] <- '100-109'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=110 & input$LE_MSZ[ww] <= 149] <- '110-149'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=150 & input$LE_MSZ[ww] <= 219] <- '150-219'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=220] <- '>=220'

print(length(ww))
}

return(input)
}