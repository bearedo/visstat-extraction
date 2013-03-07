DCFLengthCats<- function(input=dasbytrip,data.type="eflalo") {
input$VESSEL_LENGTH <- rep(NA,length(input[,1]))
if(data.type=="eflalo")  {
input$VESSEL_LENGTH[input$VE_LEN < 10] <- 'u10m'
input$VESSEL_LENGTH[input$VE_LEN >= 10 & input$VE_LEN < 15] <-  'o10t15m'
input$VESSEL_LENGTH[input$VE_LEN >= 15] <- 'o15m'  }
else{
input$VESSEL_LENGTH[input$LENGTH < 10] <- 'u10m'
input$VESSEL_LENGTH[input$LENGTH >= 10 & input$LENGTH < 15] <-  'o10t15m'
input$VESSEL_LENGTH[input$LENGTH >= 15] <- 'o15m'  }




input
}
