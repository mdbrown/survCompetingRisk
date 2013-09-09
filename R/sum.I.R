sum.I <-
function(yy,FUN,Yi,Vi=NULL){

   if (FUN=="<"|FUN==">=") { yy <- -yy; Yi <- -Yi}

   pos <- rank(c(yy,Yi),ties.method='f')[1:length(yy)]-rank(yy,ties.method='f')

   if (substring(FUN,2,2)=="=") pos <- length(Yi)-pos
   
   if (!is.null(Vi)) {
      if(substring(FUN,2,2)=="=") tmpind <- order(-Yi) else  tmpind <- order(Yi)
      Vi <- apply(as.matrix(Vi)[tmpind,,drop=F],2,cumsum)
      return(rbind(0,Vi)[pos+1,])
   } else return(pos)
}
