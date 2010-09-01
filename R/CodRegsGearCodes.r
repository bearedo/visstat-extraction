
CodRegsGearCodes <- function(data=data) {
print(dim(data))
#NB run DCFCodes and DCFMeshCategory first
data$REG_GEAR <- rep(NA,length(data[,1]))
data$REG_GEAR[data$GEAR == "BEAM" & data$MESH_SIZE_RANGE %in% c(">=120")] <- 'BT1'
data$REG_GEAR[data$GEAR == "BEAM" & data$MESH_SIZE_RANGE %in% c("80-89","90-99","100-109","100-119")] <- 'BT2'
data$REG_GEAR[data$GEAR %in% c("OTTER","DEM_SEINE") & data$MESH_SIZE_RANGE %in% c("100-109","100-119",">=120")] <- 'TR1'
data$REG_GEAR[data$GEAR %in% c("OTTER","DEM_SEINE") & data$MESH_SIZE_RANGE %in% c("80-89","90-99")] <- 'TR2'
data$REG_GEAR[data$GEAR %in% c("GILL") ] <- 'GN'
data$REG_GEAR[data$GEAR %in% c("TRAMMEL") ] <- 'GN'
data$REG_GEAR[data$GEAR %in% c("LONGLINE") ] <- 'LL'
print(dim(data))
data
}