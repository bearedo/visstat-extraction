


#Put DCF gears on:
DCFGearCodes<-function(data=data){

data$GEAR <- NA

data$GEAR[data$GPY_CODE %in% c('TBB','TBS','TBZ')] <- 'BEAM'
data$GEAR[data$GPY_CODE %in% c('OTB','TB','PTB','OTT','CTB','OTG')] <- 'OTTER'
data$GEAR[data$GPY_CODE %in% c('SDN','SSC','SB')] <- 'DEM_SEINE'
data$GEAR[data$GPY_CODE %in% c('OTM','TM','PTM')] <- 'PEL_TRAWL'
data$GEAR[data$GPY_CODE %in% c('PS','PSN')] <- 'PEL_SEINE'
data$GEAR[data$GPY_CODE %in% c('DRB','HMD')] <- 'DREDGE'
data$GEAR[data$GPY_CODE %in% c('LL','LX','LH','LLS','LLD','LHP','BTF','LHM','LTL')] <- 'LONGLINE'
data$GEAR[data$GPY_CODE %in% c('GN','GNS','GND','GTN','GNC')]='GILL'
data$GEAR[data$GPY_CODE %in% c('GTR')] <- 'TRAMMEL'
data$GEAR[data$GPY_CODE %in% c('FYK','FPN','FPO','FIX')] <-'POTS'
data$GEAR[data$GPY_CODE %in% c('NK','MIS')] <- NA
data$GEAR <- ifelse(is.na(data$GEAR), NA, data$GEAR)

data

}