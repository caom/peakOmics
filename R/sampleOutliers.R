#***************************************************************************
#Function: sampleOutliers
#  Sample outlier detection and return sample indices for removal
#Copyright (C) 2015  Mingshu Cao (mingshu.cao@agresearch.co.nz)
#License: GNU  General Public License v3.0 or later
#http://www.gnu.org/licenses/
#***************************************************************************
sampleOutliers=function(dat, repGroup, cv.thresh=0.2){
  .cv=function(x){return(sd(x)/mean(x))}

  cat("data should be in sample x feature \n")
  cat("the number of treatment/replicate groups",length(unique(repGroup)),"\n")
  sample.mean=rowMeans(dat)
  CV=tapply(sample.mean, as.factor(repGroup), .cv)
  k=which(CV > cv.thresh)  
  length(k)
  sampleName.varied=names(CV[which(CV > cv.thresh)])
  ratio=length(k)/length(unique(repGroup))   

  #for the list of PG###, check outliers within reps
  sample.outlier.idx=NULL
  for(i in 1:length(sampleName.varied)){
    rep.idx=which(repGroup==sampleName.varied[i])
    rep.inten=dat[rep.idx,]
    y=rowMeans(rep.inten)
    rep.Z=0.6745*(y-median(y))/mad(y, constant=1)
    thresh=which(abs(rep.Z)>3.0)
    if(length(thresh)!=0){
     cat("Found outliers in sample: ",sampleName.varied[i], "\n")
     cat(length(thresh), "out of", length(rep.idx), "reps to be removed.\n")
     #keep original sample idex -- to be removed
     sample.outlier.idx=c(sample.outlier.idx, rep.idx[thresh])
    }
  }
  sample.outlier.idx
}#>>>
