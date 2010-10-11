


#Put DCF gears on:
DCFGearCodes<-function(data=data,data.type="visstat"){

data$GEAR <- NA
if(data.type=="visstat"){
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
data$GEAR[data$GPY_CODE %in% c('NK','MIS','TGB')] <- 'unknown'
data$GEAR <- ifelse(is.na(data$GEAR), 'unknown', data$GEAR)
}
if(data.type=="eflalo"){
data$GEAR[data$LE_GEAR %in% c('TBB','TBS','TBZ')] <- 'BEAM'
data$GEAR[data$LE_GEAR %in% c('OTB','TB','PTB','OTT','CTB','OTG')] <- 'OTTER'
data$GEAR[data$LE_GEAR %in% c('SDN','SSC','SB')] <- 'DEM_SEINE'
data$GEAR[data$LE_GEAR %in% c('OTM','TM','PTM')] <- 'PEL_TRAWL'
data$GEAR[data$LE_GEAR %in% c('PS','PSN')] <- 'PEL_SEINE'
data$GEAR[data$LE_GEAR %in% c('DRB','HMD')] <- 'DREDGE'
data$GEAR[data$LE_GEAR %in% c('LL','LX','LH','LLS','LLD','LHP','BTF','LHM','LTL')] <- 'LONGLINE'
data$GEAR[data$LE_GEAR %in% c('GN','GNS','GND','GTN','GNC')]='GILL'
data$GEAR[data$LE_GEAR %in% c('GTR')] <- 'TRAMMEL'
data$GEAR[data$LE_GEAR %in% c('FYK','FPN','FPO','FIX')] <-'POTS'
data$GEAR[data$LE_GEAR %in% c('NK','MIS','TGB')] <- 'unknown'
data$GEAR <- ifelse(is.na(data$GEAR), 'unknown', data$GEAR)
}

data

}