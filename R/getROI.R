#***************************************************************************
#Function: getROI
#  to retrieve a region of interest either XIC or mass spectra          
#Copyright (C) 2015  Mingshu Cao (mingshu.cao@agresearch.co.nz)
#License: GNU  General Public License v3.0 or later
#http://www.gnu.org/licenses/
#***************************************************************************
#ms1raw is a list from getMSnRaw function
#x=getROI(ms1raw, rtRange=c(2,18)*60, mzRange=c(502.5, 503.5), returnXIC=F)
#plot(x), or plotSpectrum(x)
getROI=function(ms1raw, rtRange=c(823, 923), mzRange=c(502.5, 503.5), 
                returnXIC=FALSE){
  if(returnXIC){
    #xcms package, getEIC is a processed data
    tmp=list()
    for(k in 1:length(ms1raw$rt)){
      tmp[[k]]=rep(ms1raw$rt[k], nrow(ms1raw$sp[[k]]))
    }
    tmp=unlist(tmp)
    mz=unlist(lapply(ms1raw$sp, function(x) x[,1]));
    intensity=unlist(lapply(ms1raw$sp, function(x) x[,2]));
    idx=which(mz>=mzRange[1] & mz<=mzRange[2])
    y=intensity[idx]
    xic=cbind(rt=tmp[idx], inten=y)
    if(is.null(rtRange))
      return(xic)
    else{
      rt.idx=which(xic[,1]>=rtRange[1] & xic[,1]<=rtRange[2])
      return(xic[rt.idx,])
    }
  }else{ #return mass spec
    #------------
    if(is.null(rtRange)){
      mz=unlist(lapply(ms1raw$sp, function(x) x[,1]));
      intensity=unlist(lapply(ms1raw$sp, function(x) x[,2]));
      mzRangeIdx = (mz >= mzRange[1]  & mz <= mzRange[2])
      return(cbind(mz=mz[mzRangeIdx], inten=intensity[mzRangeIdx]))
    }else{
     rtRangeIdx = (ms1raw$rt > rtRange[1] & ms1raw$rt < rtRange[2])
     rtSlotSpec=ms1raw$sp[rtRangeIdx];
     mz=unlist(lapply(rtSlotSpec, function(x) x[,1]));
     intensity=unlist(lapply(rtSlotSpec, function(x) x[,2]));

     mzRangeIdx = (mz >= mzRange[1]  & mz < mzRange[2])
     return(cbind(mz=mz[mzRangeIdx], inten=intensity[mzRangeIdx]))
    }
  }#mass spec
}#>>>
