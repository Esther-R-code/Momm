#' This function is for sensitvity analysis used in senstivityanalysis.txt
#' It will calculate the measured confounder without the disturbance(epsilon)
#' reference: 'Sensitivity analysis.pdf' in 'ref' file folder
#' @title PSE with measured confounder add in the model
#' @param eta_msdatap the measured confounder without esp
#' @param msdata the orginal data without measured confounder
#' @param confounder_ms the values of the confounder
#' @param n_cate the number of categories of the outcome
#' @param sc the category of Y that users have "NO" interesting in
#' @param intv Number of intervention, only 4.
#' @param intval the value of exposure used in Intervention , Default is c(0,1).
#' @param nb Number of bootstrapping. Default is 0 (no bootstrapping applied).
#' @param etasd the stand deviation of \eqn{\eta}
#' @param n_core number of cores that will be used in parallel computing. default=1
#'
mean_PSE_U<- function(eta_msdatap, msdata, confounders_ms, n_cate, sc, intv, intval, nb=0, etasd, n_core=1){
  ndata<- cbind(msdata, eta_msdatap)
  if(dim(msdata)[1]==length(eta_msdatap)){
    n_sample<-length(eta_msdatap)
  }else{cat("Error: the sample size of measured confounder isn't epual to the sample size of data.")}
  wc<- seq(1,n.cate,by=1); wc<- wc[-sc]
  wc_count<-length(wc)
  Outcome<-paste0("Outcome=",wc)
  path<- c("W>Y","W>S>Y","W>Q>Y","W>Q>S>Y")
  #repeat 100 times
  i<- 1
  PSE_100_RD<- 0
  PSE_100_RR<- 0
  repeat{
    if(i>101){break}
    ndata$Ums<- ndata$eta_msdatap+rnorm(n_sample,mean=0,sd=etasd)
    confoundersU<- c(confounders_ms, median(ndata$Ums))
    PSE_ms<-Ord_mediation_analysis_pal_sc(Indata=ndata[,-which(colnames(ndata)=="eta_msdatap")], n.cate=n_cate, confounders=confoundersU, intv=intv, intval=intval, nb=nb, w_c=wc, n_core=n_core)
    #matrix of PSE under RD
    PSE_RD_vector<- NULL
    for(pathcount in 1:4){
      for(catecount in 1:wc_count){PSE_RD_vector<-c(PSE_RD_vector,PSE_ms[paste0("RD ",path[pathcount]), paste0("Outcome=",wc[catecount])])}
    }
    PSE_RD<- matrix(PSE_RD_vector, ncol = wc_count, nrow = 4, byrow = T)
    PSE_100_RD<- PSE_100_RD+PSE_RD
    #matrix of PSE under RD

    #matrix of PSE under RR
    PSE_RR_vector<- NULL
    for(pathcount in 1:4){
      for(catecount in 1:wc_count){PSE_RR_vector<-c(PSE_RR_vector,PSE_ms[paste0("RR ",path[pathcount]), paste0("Outcome=",wc[catecount])])}
    }
    PSE_RR<- matrix(PSE_RR_vector, ncol = wc_count, nrow = 4, byrow = T)
    PSE_100_RR<- PSE_100_RR+log(PSE_RR)
    #matrix of PSE under RR
    i<- i+1
  }
  #repeat 100 times
  PSE_mean_RD<- PSE_100_RD/100
  PSE_mean_RR<- PSE_100_RR/100
  colnames(PSE_mean_RD)<- Outcome; colnames(PSE_mean_RR)<- Outcome
  return(list(PSE_mean_RD,PSE_mean_RR))
}
