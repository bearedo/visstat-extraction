
############################
predict.na <-  function(model,newdata,what.variable.is.null = 'SI_LONG'){
    n=dim(newdata)[1]
    ww=is.na(newdata[,what.variable.is.null])
    mat1=newdata[ww == F,]
    dd=data.frame(mat1)
    out=predict(model,dd,type="response")
    nout=rep(NA,n)
    nout[ww == F]=out
    nout
}

