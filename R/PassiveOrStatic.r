
PassiveOrStatic <- function(input=tacsat1) {
#Note DCF GEAR Codes must be run first for this to work
input$FISHERY_TYPE <- rep(NA,length(input[,1]))


input$FISHERY_TYPE[input$GEAR == 'BEAM'] <- 'mobile'
input$FISHERY_TYPE[input$GEAR == 'OTTER'] <- 'mobile'
input$FISHERY_TYPE[input$GEAR == 'DREDGE'] <- 'mobile'
input$FISHERY_TYPE[input$GEAR == 'DEM_SEINE'] <- 'mobile'
input$FISHERY_TYPE[input$GEAR == 'GILL'] <- 'passive'
input$FISHERY_TYPE[input$GEAR == 'TRAMMEL'] <- 'passive'
input$FISHERY_TYPE[input$GEAR == 'LONGLINE'] <- 'passive'
input$FISHERY_TYPE[input$GEAR == 'POTS'] <- 'passive'
input$FISHERY_TYPE[input$GEAR == 'PEL_TRAWL'] <- 'mobile'
input$FISHERY_TYPE[input$GEAR == 'PEL_SEINE'] <- 'mobile'
input$FISHERY_TYPE <- ifelse(is.na(input$FISHERY_TYPE), 'unknown', input$FISHERY_TYPE)
input
}