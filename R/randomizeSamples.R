randomizeSamples=function(filename, nBatch=10, prefix="MS", seed=321L){
  if(!file.exists(filename))
    stop(paste(filename, "could not be found in this folder", getwd()))
  d=read.csv(filename)
  N=nrow(d)
  set.seed(seed)
  order1=sample(1:N)
  d.rand=d[order1,]
  nB=floor(N/nBatch)
  remains=N%%nB
  if(remains==0){
    batch=rep(paste("batch", 1:nBatch, sep=""), each=nB) 
  }else{
    part1=1:(nBatch-remains)
    part2=(nBatch-remains+1):nBatch
    batch=c(rep(paste("batch", part1, sep=""), each=nB),
            rep(paste("batch", part2, sep=""), each=nB+1))
  }
  runSeq=paste(prefix, sprintf("%03.0f", 1:N), sep="")  

  tmp=data.frame(runSeq, batch, d.rand)
  filename.tosave=gsub(".csv", "_randomized.csv", filename)
  write.csv(tmp, file=filename.tosave, row.names=FALSE, na=" ")
  cat("---------------------------------------------------\n",
      "Randomized sample list can be found", getwd(),"\n",
      "with a file name:", filename.tosave, "\n",
      "---------------------------------------------------\n") 
}#>>>
