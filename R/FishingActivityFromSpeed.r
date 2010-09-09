FishingActivityFromSpeed <- function(data=tacsat) {
#Function to guess whether a boat is fishing or steaming

data$activity = rep(NA,length(data[,1]))

#Everything except flyshooters are unlikely to be fishing at zero or less than 0.2 knots
data$activity[data$LE_GEAR %in% c("DRB","FPO","GN","GNS","HMD","GTR","MIS","OTB","OTM","LHM","LLS","LHM","LH","OTT","PS","PTM","TBB")
& data$SI_SP < 0.01] <- "not fishing"

data$activity[data$VE_KW > 221 & data$LE_GEAR == "TBB" & data$SI_SP >= 5 & data$SI_SP <= 8] <- "fishing"
data$activity[data$LE_GEAR == "TBB" & data$SI_SP > 8] <- "steaming"
data$activity[data$VE_KW <= 221 & data$LE_GEAR == "TBB" & data$SI_SP >= 3 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_GEAR == "TBB" & data$SI_SP > 6] <- "steaming"



data$activity[data$LE_GEAR == "TBS" & data$SI_SP > 2 & data$SI_SP <= 4] <- "fishing"
data$activity[data$LE_GEAR == "TBS" & data$SI_SP > 5] <- "steaming"

data$activity[data$LE_GEAR %in% c("OTB","OTT","OTM","PTB","SPR") & data$SI_SP >= 3 & data$speed <= 4] <- "fishing"
data$activity[data$LE_GEAR %in% c("OTB","OTT","OTM","PTB","SPR") & data$SI_SP > 4] <- "steaming"

data$activity[data$LE_GEAR %in% c("SSC","SDN") & data$SI_SP >= 1 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_GEAR %in% c("SSC","SDN") & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_GEAR == "DRB" & data$SI_SP > 0 & data$SI_SP <= 0.6] <- "fishing"
data$activity[data$LE_GEAR == "DRB" & data$SI_SP > 5] <- "steaming"

data$activity[data$LE_GEAR %in% c("GNS","GN") & data$SI_SP > 1 & data$SI_SP <= 10] <- "fishing"
data$activity[data$LE_GEAR %in% c("GTR") & data$SI_SP > 1 & data$SI_SP <= 10] <- "fishing"
data$activity[data$LE_GEAR %in% c("GNS","GN") & data$SI_SP > 10] <- "steaming"
data$activity[data$LE_GEAR %in% c("GTR") & data$SI_SP > 10] <- "steaming"
data$activity[data$LE_GEAR %in% c("LHM") & data$SI_SP >= 1 & data$SI_SP <= 10] <- "fishing"
data$activity[data$LE_GEAR %in% c("LHM") & data$SI_SP > 10] <- "steaming"
data$activity[is.na(data$activity)] <- "ambiguous"

data
}
