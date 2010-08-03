# FUNCTION THAT ADDS THE CORRECT QUOTES TO AVOID GETTING TOTALLY CONFUSED
WriteSQLString <- function(thevect) {
  if ( length(thevect)==1 ) {
    SQLvect <- paste("('",thevect,"')",sep="")
  }
  if ( length(thevect)>1 ) {
    SQLvect <- c("(")
      for ( rr in 1:(length(thevect)-1) ) {
      SQLvect <- paste(SQLvect,"'",thevect[rr],"',",sep="")
      }
    SQLvect <- paste(SQLvect,"'",thevect[rr+1],"'",sep="")
    SQLvect <- paste(SQLvect,")",sep="")
  }
return(SQLvect)
}