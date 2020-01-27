rescale<-function(v){
  rng<-range(vec,na.rm=TRUE)
  newscale<-(vec-rng[1]) / (rng[2] - rng[1])
  return(newscale)

}
