DCFMeshCategory<-
function(input=eflalo2,data.type="visstat")
{
#NB. We need a FISHERY_TYPE column to say whether it is mobile or static


if(data.type=="visstat"){

ww <- (1:length(input[,1]))[input$FISHERY_TYPE == 'mobile']
input$MESH_SIZE_RANGE<-rep(NA,length(input[,1]))

input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww][ww]>=1 & input$MESHSIZE[ww][ww] < 15] <- '<16'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=16 & input$MESHSIZE[ww] < 31] <- '16-31'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=32 & input$MESHSIZE[ww] < 54] <- '32-54'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=55 & input$MESHSIZE[ww] < 69] <- '55-69'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=70 & input$MESHSIZE[ww] < 79] <- '70-79'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=80 & input$MESHSIZE[ww] < 89] <- '80-89'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=90 & input$MESHSIZE[ww] < 99] <- '90-99'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=100 & input$MESHSIZE[ww] < 119] <- '100-119'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=120] <- '>120'

print(length(ww))

ww <- (1:length(input[,1]))[input$FISHERY_TYPE %in% c('unknown','passive')]
input$MESH_SIZE_RANGE<-rep(NA,length(input[,1]))

input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww][ww]>=1 & input$MESHSIZE[ww][ww] < 15] <- '<16'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=16 & input$MESHSIZE[ww] < 31] <- '16-31'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=32 & input$MESHSIZE[ww] < 54] <- '32-54'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=55 & input$MESHSIZE[ww] < 69] <- '55-69'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=70 & input$MESHSIZE[ww] < 79] <- '70-79'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=80 & input$MESHSIZE[ww] < 89] <- '80-89'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=90 & input$MESHSIZE[ww] < 99] <- '90-99'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=100 & input$MESHSIZE[ww] < 119] <- '100-119'
input$MESH_SIZE_RANGE[ww][input$MESHSIZE[ww] >=120] <- '>120'

print(length(ww))

}

if(data.type=="eflalo"){

ww <- (1:length(input[,1]))[input$FISHERY_TYPE == 'mobile']
input$MESH_SIZE_RANGE<-rep(NA,length(input[,1]))

input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww][ww]>=1 & input$LE_MSZ[ww][ww] < 15] <- '<16'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=16 & input$LE_MSZ[ww] < 31] <- '16-31'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=32 & input$LE_MSZ[ww] < 54] <- '32-54'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=55 & input$LE_MSZ[ww] < 69] <- '55-69'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=70 & input$LE_MSZ[ww] < 79] <- '70-79'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=80 & input$LE_MSZ[ww] < 89] <- '80-89'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=90 & input$LE_MSZ[ww] < 99] <- '90-99'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=100 & input$LE_MSZ[ww] < 119] <- '100-119'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=120] <- '>120'

print(length(ww))

ww <- (1:length(input[,1]))[input$FISHERY_TYPE %in% c('unknown','passive')]
input$MESH_SIZE_RANGE<-rep(NA,length(input[,1]))

input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww][ww]>=1 & input$LE_MSZ[ww][ww] < 15] <- '<16'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=16 & input$LE_MSZ[ww] < 31] <- '16-31'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=32 & input$LE_MSZ[ww] < 54] <- '32-54'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=55 & input$LE_MSZ[ww] < 69] <- '55-69'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=70 & input$LE_MSZ[ww] < 79] <- '70-79'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=80 & input$LE_MSZ[ww] < 89] <- '80-89'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=90 & input$LE_MSZ[ww] < 99] <- '90-99'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=100 & input$LE_MSZ[ww] < 119] <- '100-119'
input$MESH_SIZE_RANGE[ww][input$LE_MSZ[ww] >=120] <- '>120'

print(length(ww))
}

input
}