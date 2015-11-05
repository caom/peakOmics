#***************************************************************************
#Function: normalize.runorder 
#  to normalize run-order effect within a batch
#Copyright (C) 2015  Mingshu Cao (mingshu.cao@agresearch.co.nz)
#License: GNU  General Public License v3.0 or later
#http://www.gnu.org/licenses/
#***************************************************************************
normalize.runorder <- function(dmat, run.seq, log2=FALSE){
  if(log2) warnings("fitting log-linear")
  dmat=as.matrix(dmat)
  if(is.null(colnames(dmat)))
    stop("provide a matrix with colnames")
  if(nrow(dmat)!=length(run.seq))
    stop("data matrix should be in the dimension of sample x ions");
  dn=matrix(0, nrow(dmat), ncol(dmat))
  colnames(dn)=colnames(dmat)
  for(j in 1:ncol(dmat)){
    ion.d=dmat[,j]
    if(any(is.na(ion.d))){
       ion.d[which(is.na(ion.d))]=min(ion.d, na.rm=T)
    }
    if(log2) ion.d=log2(ion.d) 
    lm1=lm(ion.d ~ run.seq)
    bsln=fitted(lm1)
    margin=max(bsln)-bsln
    baseoff=ion.d+margin
    dn[,j]=baseoff
  }
  dn
}
