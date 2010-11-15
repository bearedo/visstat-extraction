


#Put DCF gears on:
DCFGearCodes<-function(input=tacsat2f,data.type="visstat"){

input$GEAR <- NA
if(data.type=="visstat"){
input$GEAR[input$GPY_CODE %in% c('TBB','TBS','TBZ')] <- 'BEAM'
input$GEAR[input$GPY_CODE %in% c('OTB','TB','PTB','OTT','CTB','OTG')] <- 'OTTER'
input$GEAR[input$GPY_CODE %in% c('SDN','SSC','SB')] <- 'DEM_SEINE'
input$GEAR[input$GPY_CODE %in% c('OTM','TM','PTM')] <- 'PEL_TRAWL'
input$GEAR[input$GPY_CODE %in% c('PS','PSN')] <- 'PEL_SEINE'
input$GEAR[input$GPY_CODE %in% c('DRB','HMD')] <- 'DREDGE'
input$GEAR[input$GPY_CODE %in% c('LL','LX','LH','LLS','LLD','LHP','BTF','LHM','LTL')] <- 'LONGLINE'
input$GEAR[input$GPY_CODE %in% c('GN','GNS','GND','GTN','GNC')]='GILL'
input$GEAR[input$GPY_CODE %in% c('GTR')] <- 'TRAMMEL'
input$GEAR[input$GPY_CODE %in% c('FYK','FPN','FPO','FIX')] <-'POTS'
input$GEAR[input$GPY_CODE %in% c('NK','MIS','TGB')] <- 'unknown'
input$GEAR <- ifelse(is.na(input$GEAR), 'unknown', input$GEAR)
}
if(data.type=="eflalo"){
input$GEAR[input$LE_GEAR %in% c('TBB','TBS','TBZ')] <- 'BEAM'
input$GEAR[input$LE_GEAR %in% c('OTB','TB','PTB','OTT','CTB','OTG')] <- 'OTTER'
input$GEAR[input$LE_GEAR %in% c('SDN','SSC','SB')] <- 'DEM_SEINE'
input$GEAR[input$LE_GEAR %in% c('OTM','TM','PTM')] <- 'PEL_TRAWL'
input$GEAR[input$LE_GEAR %in% c('PS','PSN')] <- 'PEL_SEINE'
input$GEAR[input$LE_GEAR %in% c('DRB','HMD')] <- 'DREDGE'
input$GEAR[input$LE_GEAR %in% c('LL','LX','LH','LLS','LLD','LHP','BTF','LHM','LTL')] <- 'LONGLINE'
input$GEAR[input$LE_GEAR %in% c('GN','GNS','GND','GTN','GNC')]='GILL'
input$GEAR[input$LE_GEAR %in% c('GTR')] <- 'TRAMMEL'
input$GEAR[input$LE_GEAR %in% c('FYK','FPN','FPO','FIX')] <-'POTS'
input$GEAR[input$LE_GEAR %in% c('NK','MIS','TGB')] <- 'unknown'
input$GEAR <- ifelse(is.na(input$GEAR), 'unknown', input$GEAR)
}

input

}