### survVis

### functiong to visualise survival estimates from a flexsurvreg object

setwd("~/Documents/GitHub/pscShiny/Data")
load("cfm.ob.R")

cfm <- cfm.ob


cfm.survEst <- function(range=c(0,24),lam,kn,haz_co,k,beta){
  
  logt <- log(seq(min(range),max(range),length=100)+1e-6)

  z <- NULL
  ### basis functions
  for(i in 1:k){
    zt <- modp(logt-kn[(i+1)])^3 - lam[(i+1)]*modp(logt-kn[1])^3 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^3
    z <- cbind(z,zt)
  }
  
  H0 <- exp(haz_co[1]+ haz_co[2]*logt+z%*%haz_co[3:(2+k)])
  H<- H0*exp(beta)
  S <- exp(-H)
 
  ord <- order(logt)
  ret <- list("time"=exp(logt)[ord],"S"=S[ord]) 
  return(ret)
  
}



cmfVis.flexsurvreg <- function(cfm,beta=0,...){

  lam <- cfm$model$lam
  kn <- cfm$model$kn
  haz_co <- cfm$model$haz_co  
  k <- cfm$model$k
  s.est <- cfm.survEst(range=c(0,24),lam,kn,haz_co,k,beta=beta)
  
  plot(s.est$time,s.est$S,typ="l",...)
  
  }


cmfVis.flexsurvreg(cfm,beta=0)
cmfVis.flexsurvreg(cfm,beta=0.5)
cmfVis.flexsurvreg(cfm,beta=0.5,col=4,lwd=5,ylab="Overall Survival (%)",xlab="Time (Months)")
