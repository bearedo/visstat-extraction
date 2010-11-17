
DCFPowerCats<- function(input=dasbytrip,data.type="eflalo") {

input$kW_cat <- rep(NA,length(input[,1]))

if(data.type=="eflalo")  {
input$kW_cat[input$VE_KW <= 221] <- 'lt221kW'
input$kW_cat[input$VE_KW > 221] <- 'gt221kW'
 }

else {
input$kW_cat[input$POWER <= 221] <- 'lt221kW'
input$kW_cat[input$POWER >  221] <- 'gt221kW'
 }
input
}
