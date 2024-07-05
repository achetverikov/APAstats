mygetSummary.lmer <- function (obj, alpha = 0.05, ...)
{
  require(lme4)
  smry <- summary(obj)
  coef <- smry@coefs
  lower <- qnorm(p = alpha/2, mean = coef[, 1], sd = coef[,2])
  upper <- qnorm(p = 1 - alpha/2, mean = coef[, 1], sd = coef[,2])
  if (ncol(coef) == 3) {
    p <- (1 - pnorm(abs(smry@coefs[, 3]))) * 2
    coef <- cbind(rownames(coef),coef, p, lower, upper)
  } else {
    coef <- cbind(rownames(coef),coef, lower, upper) # glmer will have 4 columns with p-values
  }
  colnames(coef) <- c("effect","est", "se", "stat", "p", "lwr", "upr")
  
  ll <- smry@AICtab[3][,1]
  deviance <- smry@AICtab[4][,1]
  
  AIC <- smry@AICtab[1][,1] 
  BIC <- smry@AICtab[2][,1]
  
  sumstat <- c(logLik = ll, deviance = deviance, AIC = AIC, BIC = BIC)
  list(coef = coef, sumstat = sumstat,
       contrasts = attr(model.matrix(obj), "contrasts"),
       xlevels = NULL, call = obj@call)
}

my.mtable<-function (..., recode_labels=NULL,digits=1){
  x <- list(...)
  x<-lapply(x, mygetSummary.lmer)
  coef<-lapply(x, function(x) {
    
    x$coef<-as.data.frame(x$coef)
    x$coef[2:7]<-lapply(x$coef[2:7], function(x){as.numeric(as.character(x))})
    x$coef$psymb<-as.character(symnum(x$coef$p, corr=FALSE,   cutpoints = c(0,  .001,.01,.05, .1, 1), symbols = c("***","**","*","."," ")))
    x$coef<-transform(x$coef,text=sprintf(paste0("%.",digits,"f [%.",digits,"f, %.",digits,"f]"), est, lwr, upr),text1=sprintf(paste0("%.",digits,"f%s"), est, psymb), se_text=sprintf(paste0("(%.",digits,"f)"),est))
    x$coef
  })
  
  #coef<-lapply(coef, function(x) x[,c("effect","text","psymb")])
  coef<-lapply(coef, function(x) x[,c("effect","text1","se_text")])
  
  coef<-lapply(coef, function(x) {x=melt(x,measure.vars=c(2:3)); x=x[order(x$effect,-x$variable),]; x})
  
  coef1<-Reduce(function(x, y) merge(x,y, sort=T, all=T, by="effect"), coef, accumulate=F)
  rownames(coef1)<-coef1$effect
  coef<-coef1[rownames(coef[[3]]),]
  if (!is.null(recode_labels)){
    require(car)
    coef$effect<-car::recode(coef$effect,recode_labels)
  }
  #  print(coef)
  coef<-data.frame(sapply(coef, function(x){car::recode(as.character(x),"NA=''")}), row.names=rownames(coef))
  
  print(latex(coef[,2:length(coef)], file='', title='', rowname=as.character(coef$effect), table.env=FALSE, longtable=FALSE,center="none",booktabs=T,cgroup=,col.just=rep('r',length(coef)-1,collapse=''), colheads=names(x),extracolheads=NULL))
}


getSummary.mer.orig<-function (obj, alpha = 0.05, ...) 
{
  smry <- summary(obj)
  mod_coef <- smry@coefs
  lower <- qnorm(p = alpha/2, mean = mod_coef[, 1], sd = mod_coef[, 
                                                                  2])
  upper <- qnorm(p = 1 - alpha/2, mean = mod_coef[, 1], sd = mod_coef[, 
                                                                      2])
  if (ncol(smry@coefs) == 3) {
    p <- (1 - pnorm(abs(smry@coefs[, 3]))) * 2
    mod_coef <- cbind(mod_coef, p, lower, upper)
  }
  else {
    mod_coef <- cbind(mod_coef, lower, upper)
  }
  colnames(mod_coef) <- c("est", "se", "stat", "p", "lwr", "upr")
  ll <- smry@logLik[1]
  deviance <- smry@deviance[["ML"]]
  AIC <- smry@AICtab[1][, 1]
  BIC <- smry@AICtab[2][, 1]
  sumstat <- c(logLik = ll, deviance = deviance, AIC = AIC, 
               BIC = BIC, N = obj@dims[["n"]], phi = NA, LR = NA, df = NA, 
               p = NA, McFadden = NA, Cox.Snell = NA, Nagelkerke = NA, 
               Aldrich.Nelson = NA)
  list(coef = mod_coef, sumstat = sumstat, contrasts = attr(obj@X, 
                                                            "contrasts"), xlevels = NULL, call = obj@call)
}
fit_with_retimes <- function(x) {
  tf<-timefit(x, )
  data.frame(t(tf@par))
}
