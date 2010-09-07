formatTacsat <- function(x){
  x$VE_REF    <- ac(x$VE_REF)
  x$SI_LATI   <- an(ac(x$SI_LATI))
  x$SI_LONG   <- an(ac(x$SI_LONG))
  x$SI_DATE   <- ac(x$SI_DATE)
  x$SI_TIME   <- ac(x$SI_TIME)
  x$SI_SP     <- an(ac(x$SI_SP))
  x$SI_HE     <- an(ac(x$SI_HE))
  return(x)
}