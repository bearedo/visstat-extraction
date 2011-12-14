


FishingActivityFromSpeed <- function(data=tacsat,data.type="LE_MET_level6") {

#Function to guess whether a boat is fishing or steaming based on Dutch data

data$activity = rep(NA,length(data[,1]))

if(data.type == "LE_MET_level6") {

#BEAM

#Beamers targeting demersal finfish > 221 kW and working mesh between 70 and 119mm
#Larger than 221
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEF_70-99_0_0" & data$SI_SP >= 4 & data$SI_SP <= 8] <- "fishing"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEF_90-119_0_0" & data$SI_SP >= 4 & data$SI_SP <= 8] <- "fishing"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEF_70-99_0_0" & data$SI_SP > 8] <- "steaming"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEF_90-119_0_0" & data$SI_SP > 8] <- "steaming"

#Less than 221

data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEF_70-99_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEF_90-119_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEF_70-99_0_0" & data$SI_SP > 6]  <- "steaming"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEF_90-119_0_0" & data$SI_SP > 6] <- "steaming"

data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEF_>=120_0_0" & data$SI_SP >= 3 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEF_>=120_0_0" & data$SI_SP >= 3 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEF_>=120_0_0" & data$SI_SP > 6]  <- "steaming"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEF_>=120_0_0" & data$SI_SP > 6] <- "steaming"

data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEF_>=120_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEF_>=120_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEF_>=120_0_0" & data$SI_SP > 6]  <- "steaming"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEF_>=120_0_0" & data$SI_SP > 6] <- "steaming"

####################

data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEF_100-119_0_0" & data$SI_SP >= 3 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEF_100-119_0_0" & data$SI_SP >= 3 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEF_100-119_0_0" & data$SI_SP > 6]  <- "steaming"
data$activity[data$VE_KW > 221 & data$LE_MET_level6 == "TBB_DEF_100-119_0_0" & data$SI_SP > 6] <- "steaming"

data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEF_100-119_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEF_100-119_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEF_100-119_0_0" & data$SI_SP > 6]  <- "steaming"
data$activity[data$VE_KW <= 221 & data$LE_MET_level6 == "TBB_DEF_100-119_0_0" & data$SI_SP > 6] <- "steaming"

#1###

data$activity[data$LE_MET_level6 == "TBB_MCD_70-99_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "TBB_MCD_70-99_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "TBB_MCD_70-99_0_0" & data$SI_SP > 6]  <- "steaming"
data$activity[data$LE_MET_level6 == "TBB_MCD_70-99_0_0" & data$SI_SP > 6] <- "steaming"



#Beamers working small meshes, probably shrimpers

data$activity[data$LE_MET_level6 == "TBB_DEF_16-31_0_0" & data$SI_SP >=.1 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "TBB_DEF_16-31_0_0" & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "TBB_DEF_<16_0_0" & data$SI_SP >= .1 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "TBB_DEF_<16_0_0" & data$SI_SP > 6] <- "steaming"



#2Undefined beamers assume they are fishing between 1 and 6 knots

data$activity[data$LE_MET_level6 == "TBB_DEF_UND_0_0" & data$SI_SP >=.1 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "TBB_DEF_UND_0_0"  & data$SI_SP > 6] <- "steaming"

#OTB - otter trawls

data$activity[data$LE_MET_level6 == "OTB_DEF_>=120_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTB_DEF_>=120_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTB_DEF_100-119_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTB_DEF_100-119_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTB_DEF_70-99_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTB_DEF_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTB_MCD_70-99_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTB_MCD_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

#OTT - otter twin trawls

data$activity[data$LE_MET_level6 == "OTT_DEF_>=120_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTT_DEF_>=120_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTT_DEF_100-119_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTT_DEF_100-119_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTT_DEF_70-99_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTT_DEF_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTT_MCD_70-99_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTT_MCD_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

#3OTM - otter mid-water trawls

data$activity[data$LE_MET_level6 == "OTM_SPF_>=120_0_0" & data$SI_SP >= 2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTM_SPF_>=120_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTM_SPF_16_31_0_0" & data$SI_SP >= 0.1 & data$SI_SP <=2] <- "fishing"
data$activity[data$LE_MET_level6 == "OTM_SPF_16_31_0_0"  & data$SI_SP > 2] <- "steaming"

data$activity[data$LE_MET_level6 == "OTM_SPF_32-54_0_0" & data$SI_SP >= 2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTM_SPF_32-54_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTM_SPF_32-69_0_0" &  data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing";
data$activity[data$LE_MET_level6 == "OTM_SPF_32-69_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "OTM_SPF_UND_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$LE_MET_level6 == "OTM_SPF_UND_0_0"  & data$SI_SP > 6] <- "steaming"


#SSC bottom seiners

data$activity[data$LE_MET_level6 == "SSC_DEF_>=120_0_0" & data$SI_SP >= 0 & data$SI_SP <=2] <- "fishing"
data$activity[data$LE_MET_level6 == "SSC_DEF_>=120_0_0"  & data$SI_SP > 2 & data$SI_SP <= 8] <- "steaming"
data$activity[data$LE_MET_level6 == "SSC_DEF_>=120_0_0"  & data$SI_SP > 8 & data$SI_SP <= 10] <- "fishing"
data$activity[data$LE_MET_level6 == "SSC_DEF_>=120_0_0"  & data$SI_SP > 10] <- "steaming"

data$activity[data$LE_MET_level6 == "SSC_DEF_100-119_0_0" & data$SI_SP >=0.1 & data$SI_SP <=2] <- "fishing"
data$activity[data$LE_MET_level6 == "SSC_DEF_100-119_0_0"  & data$SI_SP > 2 & data$SI_SP <= 8] <- "steaming"
data$activity[data$LE_MET_level6 == "SSC_DEF_100-119_0_0"  & data$SI_SP > 8 & data$SI_SP <= 10] <- "fishing"
data$activity[data$LE_MET_level6 == "SSC_DEF_100-119_0_0"  & data$SI_SP > 10] <- "steaming"

data$activity[data$LE_MET_level6 == "SSC_DEF_70-99_0_0" & data$SI_SP >= 0.1 & data$SI_SP <=2] <- "fishing"
data$activity[data$LE_MET_level6 == "SSC_DEF_70-99_0_0"  & data$SI_SP > 2 & data$SI_SP <= 8] <- "steaming"
data$activity[data$LE_MET_level6 == "SSC_DEF_0-99_0_0"  & data$SI_SP > 8 & data$SI_SP <= 10] <- "fishing"
data$activity[data$LE_MET_level6 == "SSC_DEF_70-99_0_0"  & data$SI_SP > 10] <- "steaming"


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

data$activity[data$LE_MET_level6 == "FPO_CRU_0_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "FPO_CRU_0_0_0"  & data$SI_SP > 6] <- "steaming"

#GNS gill netters

data$activity[data$LE_MET_level6 == "GNS_DEF_10-30_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "GNS_DEF_10-30_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "GNS_DEF_90-99_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "GNS_DEF_90_99_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$LE_MET_level6 == "GNS_DEF_100-119_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "GNS_DEF_100-119_0_0"  & data$SI_SP > 6] <- "steaming"

#PS Purse seiners

data$activity[data$LE_MET_level6 == "PS_SPF_<16_0_0" & data$SI_SP >= 0.1 & data$SI_SP <= 2] <- "fishing"
data$activity[data$LE_MET_level6 == "PS_SPF_<16_0_0"  & data$SI_SP > 2] <- "steaming"

data$activity[data$LE_MET_level6 == "PS_SPF_70-99_0_0" & data$SI_SP >= 0.1 & data$SI_SP <= 2] <- "fishing"
data$activity[data$LE_MET_level6 == "PS_SPF_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

#PTM Mid water pair trawlers

data$activity[data$LE_MET_level6 == "PTM_SPF_32-69_0_0" & data$SI_SP >= 1 & data$SI_SP <= 6] <- "fishing"
data$activity[data$LE_MET_level6 == "PTM_SPF_32-69_0_0"  & data$SI_SP > 6] <- "steaming"




data$activity[is.na(data$activity)] <- "ambiguous"

}

if(data.type == "gear_description") {


data$gear_description <-  paste(data$LE_GEAR,data$MESH_SIZE_RANGE,"0_0",sep='_')


#BEAM

#Beamers targeting demersal finfish > 221 kW and working mesh between 70 and 119mm

#Larger than 221
data$activity[data$VE_KW > 221 & data$gear_description == "TBB_70-79_0_0" & data$SI_SP >= 4 & data$SI_SP <= 8] <- "fishing"
data$activity[data$VE_KW > 221 & data$gdataear_description == "TBB_70-79_0_0" & data$SI_SP > 8] <- "steaming"

data$activity[data$VE_KW > 221 & data$gear_description == "TBB_80-89_0_0" & data$SI_SP >= 4 & data$SI_SP <= 8] <- "fishing"
data$activity[data$VE_KW > 221 & data$gdataear_description == "TBB_80-89_0_0" & data$SI_SP > 8] <- "steaming"

data$activity[data$VE_KW > 221 & data$gear_description == "TBB_90-99_0_0" & data$SI_SP >= 4 & data$SI_SP <= 8] <- "fishing"
data$activity[data$VE_KW > 221 & data$gdataear_description == "TBB_90-99_0_0" & data$SI_SP > 8] <- "steaming"

data$activity[data$VE_KW > 221 & data$gear_description == "TBB_100-119_0_0" & data$SI_SP >= 3 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW > 221 & data$gear_description == "TBB_100-119_0_0" & data$SI_SP > 6] <- "steaming"

data$activity[data$VE_KW > 221 & data$gear_description == "TBB_>=120_0_0" & data$SI_SP >= 3 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW > 221 & data$gear_description == "TBB_>=120_0_0" & data$SI_SP > 6] <- "steaming"


#Less than 221

data$activity[data$VE_KW <= 221 & data$gear_description == "TBB_70-79_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$gdataear_description == "TBB_70-79_0_0" & data$SI_SP > 6] <- "steaming"

data$activity[data$VE_KW <= 221 & data$gear_description == "TBB_80-89_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$gdataear_description == "TBB_80-89_0_0" & data$SI_SP > 6] <- "steaming"

data$activity[data$VE_KW <= 221 & data$gear_description == "TBB_90-99_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$gdataear_description == "TBB_90-99_0_0" & data$SI_SP > 6] <- "steaming"

data$activity[data$VE_KW <= 221 & data$gear_description == "TBB_100-119_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$gear_description == "TBB_100-119_0_0" & data$SI_SP > 6] <- "steaming"

data$activity[data$VE_KW <= 221 & data$gear_description == "TBB_>=120_0_0" & data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing"
data$activity[data$VE_KW <= 221 & data$gear_description == "TBB_>=120_0_0" & data$SI_SP > 6] <- "steaming"


#### Beamers working small meshes, probably shrimpers  ###


data$activity[data$gear_description == "TBB_16-31_0_0" & data$SI_SP >=.1 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "TBB_16-31_0_0" & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "TBB_<16_0_0" & data$SI_SP >= .1 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "TBB_<16_0_0" & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "TBB_32-54_0_0" & data$SI_SP >=.1 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "TBB_32-54_0_0" & data$SI_SP > 6] <- "steaming"



####  Undefined beamers assume they are fishing between 1 and 6 knots

data$activity[data$gear_description == "TBB_NA_0_0" & data$SI_SP >=.1 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "TBB_NA_0_0"  & data$SI_SP > 6] <- "steaming"

#OTB - otter trawls

data$activity[data$gear_description == "OTB_>=120_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "OTB_>=120_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "OTB_100-119_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "OTB_100-119_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "OTB_70-99_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "OTB_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "OTB_70-99_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "OTB_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

#OTT - otter twin trawls

data$activity[data$gear_description == "OTT_>=120_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "OTT_>=120_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "OTT_100-119_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "OTT_100-119_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "OTT_70-99_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "OTT_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "OTT_70-99_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "OTT_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

#3OTM - otter mid-water trawls

data$activity[data$gear_description == "OTM_>=120_0_0" & data$SI_SP >= 2 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "OTM_>=120_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "OTM_16_31_0_0" & data$SI_SP >= 0.1 & data$SI_SP <=2] <- "fishing"
data$activity[data$gear_description == "OTM_16_31_0_0"  & data$SI_SP > 2] <- "steaming"

data$activity[data$gear_description == "OTM_32-54_0_0" & data$SI_SP >= 2 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "OTM_32-54_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "OTM_32-69_0_0" &  data$SI_SP >= 2 & data$SI_SP <= 6] <- "fishing";
data$activity[data$gear_description == "OTM_32-69_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "OTM_NA_0_0" & data$SI_SP >=2 & data$SI_SP <=6] <- "fishing"
data$activity[data$gear_description == "OTM_NA_0_0"  & data$SI_SP > 6] <- "steaming"


#SSC bottom seiners

data$activity[data$gear_description == "SSC_>=120_0_0" & data$SI_SP >= 0 & data$SI_SP <=2] <- "fishing"
data$activity[data$gear_description == "SSC_>=120_0_0"  & data$SI_SP > 2 & data$SI_SP <= 8] <- "steaming"
data$activity[data$gear_description == "SSC_>=120_0_0"  & data$SI_SP > 8 & data$SI_SP <= 10] <- "fishing"
data$activity[data$gear_description == "SSC_>=120_0_0"  & data$SI_SP > 10] <- "steaming"

data$activity[data$gear_description == "SSC_100-119_0_0" & data$SI_SP >=0.1 & data$SI_SP <=2] <- "fishing"
data$activity[data$gear_description == "SSC_100-119_0_0"  & data$SI_SP > 2 & data$SI_SP <= 8] <- "steaming"
data$activity[data$gear_description == "SSC_100-119_0_0"  & data$SI_SP > 8 & data$SI_SP <= 10] <- "fishing"
data$activity[data$gear_description == "SSC_100-119_0_0"  & data$SI_SP > 10] <- "steaming"

data$activity[data$gear_description == "SSC_70-99_0_0" & data$SI_SP >= 0.1 & data$SI_SP <=2] <- "fishing"
data$activity[data$gear_description == "SSC_70-99_0_0"  & data$SI_SP > 2 & data$SI_SP <= 8] <- "steaming"
data$activity[data$gear_description == "SSC_70-99_0_0"  & data$SI_SP > 8 & data$SI_SP <= 10] <- "fishing"
data$activity[data$gear_description == "SSC_70-99_0_0"  & data$SI_SP > 10] <- "steaming"


#DRB dredgers

data$activity[data$gear_description == "DRB_80-89_0_0" & data$SI_SP >0.5  & data$SI_SP <= 4] <- "fishing"
data$activity[data$gear_description == "DRB_NA_0_0" & data$SI_SP >0.5  & data$SI_SP <= 4] <- "fishing"
data$activity[data$gear_description == "DRB_32-54_0_0" & data$SI_SP >0.5  & data$SI_SP <= 4] <- "fishing"

data$activity[data$gear_description == "DRB_80-89_0_0" & data$SI_SP > 4] <- "steaming"
data$activity[data$gear_description == "DRB_NA_0_0" & data$SI_SP > 4] <- "steaming"
data$activity[data$gear_description == "DRB_32-54_0_0"  & data$SI_SP > 4] <- "steaming"



data$activity[data$gear_description == "HMD_NA_0_0" & data$SI_SP >=0.001 & data$SI_SP <= 1] <- "fishing"
data$activity[data$gear_description == "HMD_NA_0_0"  & data$SI_SP > 1] <- "steaming"

#MIS Other fishing gear

data$activity[data$gear_description == "MIS_NA_0_0" & data$SI_SP >=0.25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "MIS_NA_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "MIS_10-30_0_0" & data$SI_SP >=0.25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "MIS_10-30_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "MIS_80-89_0_0" & data$SI_SP >=0.25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "MIS_80-89_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "MIS_100-109_0_0" & data$SI_SP >=0.25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "MIS_100-109_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "MIS_110-149_0_0" & data$SI_SP >=0.25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "MIS_110-149_0_0"  & data$SI_SP > 6] <- "steaming"



#FPO pots

data$activity[data$gear_description == "FPO_NA_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "FPO_NA_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "FPO_10-30_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "FPO_10-30_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "FPO_110-149_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "FPO_110-149_0_0"  & data$SI_SP > 6] <- "steaming"

#GNS gill netters

data$activity[data$gear_description == "GNS_10-30_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "GNS_10-30_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "GNS_31-49_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "GNS_31-49_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "GNS_50-59_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "GNS_50-59_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "GNS_60-69_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "GNS_60-69_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "GNS_70-79_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "GNS_70-79_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "GNS_80-89_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "GNS_80-89_0_0"  & data$SI_SP > 6] <- "steaming"


data$activity[data$gear_description == "GNS_90-99_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "GNS_90-99_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "GNS_100-109_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "GNS_100-109_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "GNS_110-149_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "GNS_110-149_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "GNS_150-219_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "GNS_150-219_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "GNS_>=220_0_0" & data$SI_SP >= .25 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "GNS_>=220_0_0"  & data$SI_SP > 6] <- "steaming"



#PS Purse seiners

data$activity[data$gear_description == "PS_<16_0_0" & data$SI_SP >= 0.1 & data$SI_SP <= 2] <- "fishing"
data$activity[data$gear_description == "PS_<16_0_0"  & data$SI_SP > 2] <- "steaming"

data$activity[data$gear_description == "PS_70-99_0_0" & data$SI_SP >= 0.1 & data$SI_SP <= 2] <- "fishing"
data$activity[data$gear_description == "PS_70-99_0_0"  & data$SI_SP > 6] <- "steaming"

data$activity[data$gear_description == "PS_100-119_0_0" & data$SI_SP >= 0.1 & data$SI_SP <= 2] <- "fishing"
data$activity[data$gear_description == "PS_100-119_0_0"  & data$SI_SP > 6] <- "steaming"


#PTM Mid water pair trawlers

data$activity[data$gear_description == "PTM_32-69_0_0" & data$SI_SP >= 1 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "PTM_32-69_0_0"  & data$SI_SP > 6] <- "steaming"


data$activity[data$gear_description == "PTM_32-54_0_0" & data$SI_SP >= 1 & data$SI_SP <= 6] <- "fishing"
data$activity[data$gear_description == "PTM_32-54_0_0"  & data$SI_SP > 6] <- "steaming"




data$activity[is.na(data$activity)] <- "ambiguous"












}










data
}
