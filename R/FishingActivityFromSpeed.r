FishingActivityFromSpeed <- function(data=tacsat) {

#Function to guess whether a boat is fishing or steaming based on Dutch data

data$activity = rep(NA,length(data[,1]))

#BEAM

#Beamers targeting demersal finfish > 221 kW and working mesh between 70 and 119mm
#Larger than 221
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEM_70-99_0_0" & data$SI_SP >= 4 & data$SI_SP <= 8] <- "fishing"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEM_90-119_0_0" & data$SI_SP >= 4 & data$SI_SP <= 8] <- "fishing"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEM_70-99_0_0" & data$SI_SP > 8] <- "steaming"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEM_90-119_0_0" & data$SI_SP > 8] <- "steaming"

#Less than 221

data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEM_70-99_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEM_90-119_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEM_70-99_0_0" & data$SI_SP > 6]  <- "steaming"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEM_90-119_0_0" & data$SI_SP > 6] <- "steaming"

data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEM_>=120_0_0" & data$SI_SP >= 3 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEM_>=120_0_0" & data$SI_SP >= 3 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEM_>=120_0_0" & data$SI_SP > 6]  <- "steaming"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEM_>=120_0_0" & data$SI_SP > 6] <- "steaming"

data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEM_>=120_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEM_>=120_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEM_>=120_0_0" & data$SI_SP > 6]  <- "steaming"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEM_>=120_0_0" & data$SI_SP > 6] <- "steaming"

####################

data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEM_100-119_0_0" & data$SI_SP >= 3 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEM_100-119_0_0" & data$SI_SP >= 3 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEM_100-119_0_0" & data$SI_SP > 6]  <- "steaming"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEM_100-119_0_0" & data$SI_SP > 6] <- "steaming"

data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEM_100-119_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEM_100-119_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEM_100-119_0_0" & data$SI_SP > 6]  <- "steaming"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEM_100-119_0_0" & data$SI_SP > 6] <- "steaming"

####

data$activity[data$LE_MET_level6 == "TBB_MCD_70-99_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "TBB_MCD_70-99_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "TBB_MCD_70-99_0_0" & data$SI_SP > 6]  <- "steaming"
data$activity[data$LE_MET_level6 == "TBB_MCD_70-99_0_0" & data$SI_SP > 6] <- "steaming"



#Beamers working small meshes, probably shrimpers

data$activity[data$LE_MET_level6 == "TBB_DEM_16-31_0_0" & data$SI_SP >=2 & data$SI_SP <=4] <- "fishing"
data$activity[data$LE_MET_level6 == "TBB_DEM_16-31_0_0" & data$SI_SP > 4] <- "steaming"

data$activity[data$LE_MET_level6 == "TBB_DEM_<16_0_0" & data$SI_SP >=2 & data$SI_SP <=4] <- "fishing"
data$activity[data$LE_MET_level6 == "TBB_DEM_<16_0_0" & data$SI_SP > 4] <- "steaming"



#Undefined beamers assume they are fishing between 1 and 6 knots

data$activity[data$LE_MET_level6 == "TBB_DEM_UND_0_0" & data$SI_SP >=1 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "TBB_DEM_UND_0_0"  & data$SI_SP > 6] <- "steaming"

#OTB - otter trawls

data$activity[data$LE_MET_level6 == "OTB_DEM_>=120_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTB_DEM_>=120_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTB_DEM_100-119_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTB_DEM_100-119_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTB_DEM_70-99_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTB_DEM_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTB_MCD_70-99_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTB_MCD_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

#OTT - otter twin trawls

data$activity[data$LE_MET_level6 == "OTT_DEM_>=120_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTT_DEM_>=120_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTT_DEM_100-119_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTT_DEM_100-119_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTT_DEM_70-99_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTT_DEM_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTT_MCD_70-99_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTT_MCD_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

#OTM - otter mid-water trawls

data$activity[data$LE_MET_level6 == "OTM_SPF_>=120_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTM_SPF_>=120_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTM_SPF_16_31_0_0" & data$SI_SP >=.1 & data$SI_SP <=2] <- "fishing"
data$activity[data$LE_MET_level6 == "OTM_SPF_16_31_0_0"  & data$SI_SP > 2] <- "steaming"

data$activity[data$LE_MET_level6 == "OTM_SPF_32-54_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTM_SPF_32-54_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTM_SPF_32-69_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTM_SPF_32-69_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTM_SPF_UND_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTM_SPF_UND_0_0"  & data$SI_SP > 6] <- "steaming"


#SSC bottom seiners

data$activity[data$LE_MET_level6 == "SSC_DEM_>=120_0_0" & data$SI_SP >= 0 & data$SI_SP <=2] <- "fishing"
data$activity[data$LE_MET_level6 == "SSC_DEM_>=120_0_0"  & data$SI_SP > 2 & data$SI_SP <= 8] <- "steaming"
data$activity[data$LE_MET_level6 == "SSC_DEM_>=120_0_0"  & data$SI_SP > 8 & data$SI_SP <= 10] <- "fishing"
data$activity[data$LE_MET_level6 == "SSC_DEM_>=120_0_0"  & data$SI_SP > 10] <- "steaming"

data$activity[data$LE_MET_level6 == "SSC_DEM_100-119_0_0" & data$SI_SP >=0 & data$SI_SP <=2] <- "fishing"
data$activity[data$LE_MET_level6 == "SSC_DEM_100-119_0_0"  & data$SI_SP > 2 & data$SI_SP <= 8] <- "steaming"
data$activity[data$LE_MET_level6 == "SSC_DEM_100-119_0_0"  & data$SI_SP > 8 & data$SI_SP <= 10] <- "fishing"
data$activity[data$LE_MET_level6 == "SSC_DEM_100-119_0_0"  & data$SI_SP > 10] <- "steaming"

data$activity[data$LE_MET_level6 == "SSC_DEM_70-99_0_0" & data$SI_SP >=0 & data$SI_SP <=2] <- "fishing"
data$activity[data$LE_MET_level6 == "SSC_DEM_70-99_0_0"  & data$SI_SP > 2 & data$SI_SP <= 8] <- "steaming"
data$activity[data$LE_MET_level6 == "SSC_DEM_0-99_0_0"  & data$SI_SP > 8 & data$SI_SP <= 10] <- "fishing"
data$activity[data$LE_MET_level6 == "SSC_DEM_70-99_0_0"  & data$SI_SP > 10] <- "steaming"


#DRB dredgers

data$activity[data$LE_MET_level6 == "DRB_MOL_0_0_0" & data$SI_SP >0.5  & data$SI_SP <= 4] <- "fishing"
data$activity[data$LE_MET_level6 == "DRB_MOL_0_0_0"  & data$SI_SP > 4] <- "steaming"

data$activity[data$LE_MET_level6 == "HMD_MOL_UND_0_0" & data$SI_SP >=0.001 & data$SI_SP <= 1] <- "fishing"
data$activity[data$LE_MET_level6 == "HMD_MOL_UND_0_0"  & data$SI_SP > 1] <- "steaming"

#MIS Other fishing gear

data$activity[data$LE_MET_level6 == "MIS_MCD_UND_0_0" & data$SI_SP >=0.25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "MIS_MCD_UND_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "MIS_UND_UND_0_0" & data$SI_SP >=0.25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "MIS_UND_UND_0_0"  & data$SI_SP > 6] <- "steaming"

#FPO pots

data$activity[data$LE_MET_level6 == "FPO_CRU_0_0_0" & data$SI_SP >= 1 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "FPO_CRU_0_0_0"  & data$SI_SP > 6] <- "steaming"

#GNS gill netters

data$activity[data$LE_MET_level6 == "GNS_DEM_10-30_0_0" & data$SI_SP >= 1 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "GNS_DEM_10-30_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "GNS_DEM_90-99_0_0" & data$SI_SP >= 1 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "GNS_DEM_90_99_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "GNS_DEM_100-119_0_0" & data$SI_SP >= 1 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "GNS_DEM_100-119_0_0"  & data$SI_SP > 6] <- "steaming"

#PS Purse seiners

data$activity[data$LE_MET_level6 == "PS_SPF_<16_0_0" & data$SI_SP >= 0.1 & data$SI_SP <= 2] <- "fishing"
data$activity[data$LE_MET_level6 == "PS_SPF_<16_0_0"  & data$SI_SP > 2] <- "steaming"

data$activity[data$LE_MET_level6 == "PS_SPF_70-99_0_0" & data$SI_SP >= 0.1 & data$SI_SP <= 2] <- "fishing"
data$activity[data$LE_MET_level6 == "PS_SPF_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

#PTM Mid water pair trawlers

data$activity[data$LE_MET_level6 == "PTM_SPF_32-69_0_0" & data$SI_SP >= 1 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "PTM_SPF_32-69_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[is.na(data$activity)] <- "ambiguous"

data
}
