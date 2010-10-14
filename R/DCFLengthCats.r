
DCFLengthCats<- function(input=dasbytrip) {
input$VESSEL_LENGTH <- rep(NA,length(data[,1]))
input$VESSEL_LENGTH[input$LENGTH < 10] <- 'u10m'
input$VESSEL_LENGTH[input$LENGTH > 10 & input$LENGTH <= 15] <-  'o10t15m'
input$VESSEL_LENGTH[input$LENGTH > 15] <- 'o15m'
input
}
