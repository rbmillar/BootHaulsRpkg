#Double bootstrap of multihaul data in SELECT format

#Crude weighted average (0.25,0.5,0.25) with immediate neighbhours
WgtAvg=function(y,w=c(0.25,0.5,0.25)) {
  n=length(y)
  y.left=c(y[1],y)
  y.right=c(y,y[n])
  wgt.y=w[2]*y+w[1]*y.left[1:n]+w[3]*y.right[2:(n+1)]
  wgt.y
}


#' Double bootstrap function
#'
#' @param catch Stacked matrix or dataframe of catches in SELECT format
#' @param varnames Length-3 vector containing name of haulID variable, and 2 catch freq vars
#' @param smooth Smooth at within-haul phase to avoid losing degrees of freedom (from increasing number of zero freqs)
#'
#' @return Dataframe of double-bootstrapped freqs
#' @export
#'
#' @examples
Dble.bootstrap=function(catch,varnames=c("Haul","nwide","nfine"),smooth=T) {
  Tow=catch[,varnames[1]]
  uniqTows=unique(Tow)
  nTows=length(uniqTows)
  #cat("\n",nTows,"hauls to be double bootstrapped")
  BootCatchList=as.list(1:nTows)
  jstars=sample(uniqTows,nTows,replace=T)
  for(j in 1:nTows) {
    BootCatchList[[j]] <- catch %>% filter(Tow==jstars[j])
    BootCatchList[[j]]$Tow <- j
    BootCatchList[[j]]$Orig.Tow <- jstars[j]
  }
  boot.catch=as.data.frame(rbindlist(BootCatchList))
  #Parametric bootstrap within tows
  nObs=nrow(boot.catch)
  w=ifelse(smooth,c(0.25,0.5,0.25),c(0,1,0))
  boot.catch[,varnames[2]]=rpois(nObs,WgtAvg(boot.catch[,varnames[2]],w=w))
  boot.catch[,varnames[3]]=rpois(nObs,WgtAvg(boot.catch[,varnames[3]],w=w))
  return(as.data.frame(boot.catch))
}

#' Randomization of gear type within each haul
#'
#' @param Stacked matrix or dataframe of catches in SELECT format
#' @param varnames Length-3 vector containing name of haulID variable, and 2 catch freq vars
#'
#' @return Dataframe, with randomized gear treatment within each haul
#' @export
#'
#' @examples
Randomize=function(catch,varnames=c("Haul","nwide","nfine")) {
  Tow=catch[,varnames[1]]
  uniqTows=unique(Tow)
  nTows=length(uniqTows)
  #cat("\n",nTows,"hauls to be randomized")
  RanCatchList=as.list(1:nTows)
  for(j in 1:nTows) {
    RanCatchList[[j]] <- catch %>% filter(Tow==uniqTows[j])
    RanCatchList[[j]][,varnames[2:3]] <- RanCatchList[[j]][,varnames[sample(2:3)]]
  }
  ran.catch=as.data.frame(rbindlist(RanCatchList))
  return(as.data.frame(ran.catch))
}
