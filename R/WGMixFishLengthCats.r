WGMixFishLengthCats <- function(data=data) {
data$VESSEL_LENGTH <- rep(NA,length(data[,1]))
data$VESSEL_LENGTH[data$LENGTH < 10] <- '<10'
data$VESSEL_LENGTH[data$LENGTH >= 10 & data$LENGTH < 12] <-  '10<12'
data$VESSEL_LENGTH[data$LENGTH >= 12 & data$LENGTH < 18] <-  '12<18'
data$VESSEL_LENGTH[data$LENGTH >= 18 & data$LENGTH < 24] <-  '18<24'
data$VESSEL_LENGTH[data$LENGTH >= 24 & data$LENGTH < 40] <-  '24<40'
data$VESSEL_LENGTH[data$LENGTH >= 40] <- '>=40'
data
}
