

WGMixFishLengthCats <- function(data=data) {
data$VESSEL_LENGTH <- rep(NA,length(data[,1]))
data$VESSEL_LENGTH[data$LENGTH < 12] <- 'u12m'
data$VESSEL_LENGTH[data$LENGTH >= 12 & data$LENGTH < 24] <-  'o12t24m'
data$VESSEL_LENGTH[data$LENGTH >= 24 & data$LENGTH < 40] <-  'o24t40m'
data$VESSEL_LENGTH[data$LENGTH >= 40] <- 'o40m'
data
}
