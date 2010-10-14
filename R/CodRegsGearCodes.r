
CodRegsGearCodes <- function(input=dasbytrip) {
#NB run DCFCodes and DCFMeshCategory first
input$REG_GEAR <- rep(NA,length(data[,1]))
input$REG_GEAR[input$GEAR == "BEAM" & input$MESH_SIZE_RANGE %in% c(">120")] <- 'BT1'
input$REG_GEAR[input$GEAR == "BEAM" & input$MESH_SIZE_RANGE %in% c("80-89","90-99","100-109","100-119")] <- 'BT2'
input$REG_GEAR[input$GEAR %in% c("OTTER","DEM_SEINE") & input$MESH_SIZE_RANGE %in% c("100-109","100-119",">=120")] <- 'TR1'
input$REG_GEAR[input$GEAR %in% c("OTTER","DEM_SEINE") & input$MESH_SIZE_RANGE %in% c("80-89","90-99")] <- 'TR2'
input$REG_GEAR[input$GEAR %in% c("GILL") ] <- 'GN'
input$REG_GEAR[input$GEAR %in% c("TRAMMEL") ] <- 'GN'
input$REG_GEAR[input$GEAR %in% c("LONGLINE") ] <- 'LL'
input
}