
####. R shiny output

remove.packages("psc")
library(devtools)
install_github("RichJJackson/psc")

??github
library(psc)

setwd("/Volumes/richj23/Fellowship/Methodology/Data and Models/Models/HCC")

##loading model

dir()
load("hcc_soraf_os_k3.R")


fpm

### Inputs needed on the webpage
vi <- "no"
ecog <- 0
allmets <- "no"
logafp <- 1.9
alb <- 20
logcreat <- 4.2
logast <- 3.5
aet <- "HBV"
age60 <- 3

### Create 'new' dataset
newData <- data.frame(vi,ecog,allmets,logafp,alb,logcreat,logast,aet,age60)



### Estimate Survival function
newSurv <- surv_fpm_rs(fpm,newData)

plot(newSurv$time,newSurv$S,typ="l",col="darkorchid",lwd=4)



dataComb(fpm,newData)



#### Functions

modelExtract <- function(CFM,...){  UseMethod("modelExtract")}


modelExtract.flexsurvreg <- function(CFM){  co <- CFM$coefficients  k <- CFM$k  kn <- CFM$knots  sig <- vcov(CFM)  max(kn)  lam <- (max(kn)-kn)/(max(kn)-min(kn))  form <- formula(CFM)  mf <- model.frame(CFM)  n_haz_co <- k+2  haz_co <- co[1:n_haz_co]  cov_co <- co[(n_haz_co+1):length(co)]  ret <- list("model.frame"=mf,"cov_co"=cov_co,"sig"=sig,"haz_co"=haz_co,"k"=k,"kn"=kn,"lam"=lam,"formula"=form)  return(ret)}

dataComb <- function(CFM,DC,id=NULL){  UseMethod("dataComb")}
### Survival Functionsurv_fpm_rs <- function(fpm,newData,maxTim=60, beta=0){
	
  me <- modelExtract(fpm)
  time <- seq(0,maxTim,length=1000)  logt <- log(time+1e-06)  lam <- me$lam  kn <- me$kn  cov_co <- me$cov.co  haz_co <- me$haz_co  k <- me$k
  linPred <- lp_fpm_rs(fpm,newData)  adjLP <- linPred + beta
  z <- NULL  ### basis functions  for(i in 1:k){    zt <- modp(logt-kn[(i+1)])^3 - lam[(i+1)]*modp(logt-kn[1])^3 - (1-lam[(i+1)])*modp(logt-kn[length(kn)])^3    z <- cbind(z,zt)  }  H0 <- exp(haz_co[1]+ haz_co[2]*logt+z%*%haz_co[3:(2+k)])  H<- H0*exp(adjLP)  S <- exp(-H)  ord <- order(time)  ret <- list("time"=time[ord],"S"=S[ord])  ret}


#### Linear Predictorlp_fpm_rs <- function(fpm,newData){  
  me <- modelExtract(fpm)
  mf <- model.frame(fpm)

  nm.id <- which(names(mf)%in%names(newData))
  mf[1,nm.id] <- newData
   
  mm <-  model.matrix(me$formula,data=mf)[1,-1]
  lp <- (mm)%*%(me$cov_co)
   c(lp)}

modp <- function(x){  x*(sign(x)+1)/2}