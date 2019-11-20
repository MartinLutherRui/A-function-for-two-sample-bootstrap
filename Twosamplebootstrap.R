twosampleboot<-function(data1,data2,R,n,statistic){
  #data1 is your first sample
  #data2 is your second sample
  #R:The number of bootstrap replicates
  #n:The sample size everytime you choose from your original dataset.
  #staticstic: The statistics you want to compute
  obs1<-dim(data1)[1]
  obs2<-dim(data2)[1]
  result<-vector()
  for (i in c(1:R)) {
    v1<-sample(c(1:obs1),n)
    v2<-sample(c(1:obs2),n)
    result[i]=statistic(data1,data2,v1,v2)
  }
  return(result)
}