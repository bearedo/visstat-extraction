
DCFLengthCats.r <- function(data=data) {
data$VESSEL_LENGTH <- rep(NA,length(data[,1]))
data$VESSEL_LENGTH[data$LENGTH < 10] <- 'u10m'
data$VESSEL_LENGTH[data$LENGTH > 10 & data$LENGTH <= 15] <-  'o10t15m'
data$VESSEL_LENGTH[data$LENGTH > 15] <- 'o15m'
data
}
