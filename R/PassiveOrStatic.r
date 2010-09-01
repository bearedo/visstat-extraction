
PassiveOrStatic <- function(data=data) {

data$FISHERY_TYPE <- rep(NA,length(data[,1]))
data$FISHERY_TYPE[data$GEAR == 'BEAM'] <- 'mobile'
data$FISHERY_TYPE[data$GEAR == 'OTTER'] <- 'mobile'
data$FISHERY_TYPE[data$GEAR == 'DREDGE'] <- 'mobile'
data$FISHERY_TYPE[data$GEAR == 'DEM_SEINE'] <- 'mobile'
data$FISHERY_TYPE[data$GEAR == 'GILL'] <- 'passive'
data$FISHERY_TYPE[data$GEAR == 'TRAMMEL'] <- 'passive'
data$FISHERY_TYPE[data$GEAR == 'LONGLINE'] <- 'passive'
data$FISHERY_TYPE[data$GEAR == 'POTS'] <- 'passive'
data$FISHERY_TYPE[data$GEAR == 'PEL_TRAWL'] <- 'mobile'
data$FISHERY_TYPE[data$GEAR == 'PEL_SEINE'] <- 'mobile'
data$FISHERY_TYPE <- ifelse(is.na(data$FISHERY_TYPE), 'unknown', data$FISHERY_TYPE)
data
}