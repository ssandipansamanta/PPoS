succ_ia <- function(type, nsamples, null.value=NULL, alternate="greater", 
                    N = NULL, n = NULL, D = NULL, d = NULL, a = 1,
                    meandiff.ia = NULL, mean.ia = NULL, propdiff.ia = NULL, prop.ia = NULL, hr.ia = NULL, stderr.ia = NULL, sd.ia = NULL, 
                    succ.crit, Z.crit.final=NULL, alpha.final=NULL,  clin.succ.threshold = NULL, 
                    meandiff.exp = NULL, mean.exp = NULL, propdiff.exp = NULL, prop.exp = NULL, hr.exp = NULL, 
                    meandiff.prior = NULL, mean.prior = NULL, sd.prior = NULL, propdiff.prior = NULL, prop.prior = NULL, hr.prior = NULL, D.prior = NULL) 
{
  if(nsamples>2 | nsamples<1) stop("Number of samples should be either 1 or 2")
  if(succ.crit=="clinical" & is.null(clin.succ.threshold)) stop("For clinical success, clin.succ.threshold must be specified.\n")
  # if(succ.crit=="trial" & (is.null(Z.crit.final) | is.null(alpha.final))) stop("For trial success, Z.crit.final or alpha.final must be specified.\n")
  if(type!="cont" & type!="bin" & type!="surv") stop("Endpoint type can be either cont (for continuous endpoint) or 
                                                      bin (for binary endpoint) or surv (for survival endpoint)\n")
  if(type=="cont"){
    if(N<=n)stop("Number of Total Sample Size should be MORE than Interim Sample Size")
    if(is.null(stderr.ia) & is.null(sd.ia)) stop("For continuous endpoint, either stderr.ia or sd.ia must be specified.\n")
    if(is.null(meandiff.ia) & is.null(mean.ia)) stop("For continuous endpoint, either meandiff.ia or mean.ia must be specified.\n")
    if(nsamples==1 & is.null(mean.ia)) stop("For one sample continuous case, mean.ia must be specified.\n")
    if(nsamples==2 & is.null(meandiff.ia)) stop("Mean difference must be specified for two samples continuous case. \n")
  }
  if(type=="bin"){
    if(N<=n)stop("Number of Total Sample Size should be MORE than Interim Sample Size")
    if(nsamples==2 & is.null(stderr.ia)) stop("For two sample binary case, stderr.ia must be specified.\n")
    if(is.null(propdiff.ia) & is.null(prop.ia)) stop("For binary endpoint, either propdiff.ia or prop.ia must be specified.\n")
    if(nsamples==1 & is.null(prop.ia)) stop("For one sample binary case, prop.ia must be specified.\n")
    if(nsamples==2 & is.null(propdiff.ia)) stop("For two samples binary case, propdiff.ia must be specified.\n")
  }
  if(type=="surv"){
    if(D<=d)stop("Number of Total Sample Size should be MORE than Interim Sample Size")
    if(is.null(hr.ia)) stop("For survival endpoint, hr.ia must be specified.\n")
    if(nsamples==1 ) stop("For survival endpoint, one sample case is not supported.\n")
    if(nsamples==2 & is.null(hr.ia)) stop("For survival endpoint, hr.ia must be specified.\n")
  }
  
  if(succ.crit=="trial"){
    if(is.null(alpha.final)){
      Z1.crit=abs(Z.crit.final)
    } else if(is.null(Z.crit.final)){
      Z1.crit=abs(qnorm(alpha.final))
    }  
  }
  
  if(!is.null(hr.prior)){hr.prior=exp(hr.prior)}
  if(!is.null(D.prior)){D.prior=4/D.prior^2}
  
  #--- Determine "r"
  r=ifelse(nsamples==1, 1, (a+1)/sqrt(a))
  
  #--- Determine "t"
  t=ifelse(type=="surv", d/D, n/N)
  
  #--- Determine "k.ia"
  if(type=="cont"){
    if(is.null(stderr.ia)) stderr.ia=r*sd.ia/sqrt(n)
  }
  
  if(type=="bin"){
    if(nsamples==1) stderr.ia=sqrt(prop.ia*(1-prop.ia)/n)
  }
  if(type=="surv"){
    if(nsamples==2 & is.null(stderr.ia)) stderr.ia=2/sqrt(d)
  }
  k.ia=sqrt(t)*stderr.ia
  
  #--- null.value
  if(is.null(null.value)) null.value=ifelse(type=="surv",1,0)
  
  #--- Determine "theta.min"
  if(succ.crit=="clinical"){
    if(type=="surv"){
      theta.min=log(clin.succ.threshold) - log(null.value)
    } else theta.min=clin.succ.threshold - null.value
  }
  
  #---------------------------------------------------------#
  #    Determine "theta.est", "theta.exp", "theta.prior"    #
  #---------------------------------------------------------#
  
  psi=NULL
  theta.exp<- NULL
  
  #--- Continuous
  if(type=="cont"){
    if(nsamples==1){ 
      theta.est=mean.ia - null.value
      if(!is.null(mean.exp)) theta.exp=mean.exp-null.value
    } else if(nsamples==2){
      theta.est=meandiff.ia - null.value
      if(!is.null(meandiff.exp)) theta.exp=meandiff.exp-null.value
    }
    if(nsamples==1 & !is.null(mean.prior) & !is.null(sd.prior) ){
      theta.prior=mean.prior-null.value; psi=sd.prior^2/(sd.prior^2+(k.ia^2/t)) 
    } else if(nsamples==2 & !is.null(meandiff.prior) & !is.null(sd.prior)){
      theta.prior=meandiff.prior-null.value; psi=sd.prior^2/(sd.prior^2+(k.ia^2/t)) 
    }
  }
  
  #--- Binary endpoint
  if(type=="bin"){
    if(nsamples==1){ 
      theta.est=prop.ia - null.value
      if(!is.null(prop.exp)) theta.exp=prop.exp-null.value
    } else if(nsamples==2){
      theta.est=propdiff.ia - null.value
      if(!is.null(propdiff.exp)) theta.exp=propdiff.exp-null.value
    }
    if(nsamples==1 & !is.null(prop.prior) & !is.null(sd.prior) ){
      theta.prior=prop.prior-null.value; psi=sd.prior^2/(sd.prior^2+(k.ia^2/t)) 
    } else if(nsamples==2 & !is.null(propdiff.prior) & !is.null(sd.prior)){
      theta.prior=propdiff.prior-null.value; psi=sd.prior^2/(sd.prior^2+(k.ia^2/t)) 
    }
  }
  
  #--- Survival endpoint
  if(type=="surv"){
    theta.est=log(hr.ia) - log(null.value)
    if(!is.null(hr.exp)) theta.exp=log(hr.exp)-log(null.value)
    if(!is.null(hr.prior) & !is.null(D.prior) ){
      theta.prior=log(hr.prior)-log(null.value); sd.prior=2/sqrt(D.prior); psi=sd.prior^2/(sd.prior^2+(k.ia^2/t)) 
    } 
  }
  
  #--- Adjust depending on direction on alternate hypothesis
  if(alternate=="smaller"){
    theta.est=-theta.est
    if(succ.crit=="clinical") theta.min=-theta.min
    if(!is.null(theta.exp)) theta.exp=-theta.exp
    if(!is.null(psi)) theta.prior=-theta.prior
  }
    
  #--- Determine "gamma"
  gamma=ifelse(succ.crit=="clinical", theta.min/k.ia, Z1.crit)
  
  quantile.cp.est= ((theta.est/k.ia) - gamma)/sqrt(1-t)
  
  output_interim_trend <- paste0(round(pnorm(quantile.cp.est), digits=3))
  output_ppos_interim_data <- paste0(round(pnorm(quantile.cp.est*sqrt(t)), digits=3))
  
  output_sp_trend <- NULL
  if(!is.null(theta.exp)){
    quantile.cp= ((theta.est/k.ia)*(t+(1-t)*(theta.exp/theta.est)) - gamma)/sqrt(1-t)
    output_sp_trend <- paste0(round(pnorm(quantile.cp), digits=3)) 
  }
  
  output_ppos_interim_with_prior <- mean.pred2 <- sd.pred2 <- NULL
  if(!is.null(psi)){ 
    quantile.ppos.prior= sqrt(t/(1-t))*(1/sqrt((1-psi)*t + psi))*((theta.est/k.ia)*(t*(1-psi)+psi) + (1-t)*(1-psi)*(theta.prior/k.ia) - gamma)
    # cat("psi=", psi, "\n")
    output_ppos_interim_with_prior <- paste0(round(pnorm(quantile.ppos.prior), digits=3))
	mean.pred2<- psi*theta.est + (1-psi)*theta.prior
	sd.pred2<- k.ia^2*(1/(1-t) + psi/t)
	}
	
	
	mean.pred1<- theta.est
	sd.pred1<- k.ia^2*(1/(1-t) + 1/t)
	# x1 <- seq(mean.pred1-4*sd.pred1, mean.pred1+4*sd.pred1, length=100)
	# hx1 <- dnorm(x1, mean=mean.pred1, sd=sd.pred1)
	
	
	
  output <- list("result_interim_trend" = paste0("The CP is ",output_interim_trend," by expecting the interim trend in post interim"), 
                 "result_ppos_interim_data" = paste0("The PPoS is ", output_ppos_interim_data," based on interim data"), 
                 "result_sp_trend" = paste0(". It will be ", output_sp_trend, " with the specified trend in post interim."),
                 "result_ppos_interim_with_prior" = paste0(". It will be ", output_ppos_interim_with_prior, " after incoroprating prior information to the interim data."),
				         "mean_pred_wo_prior"=mean.pred1,"mean_pred_w_prior"=mean.pred2,"sd_pred_wo_prior"=sd.pred1, "sd_pred_w_prior"=sd.pred2
                 )
  return(output)
  
}