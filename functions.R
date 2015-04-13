source('within_subj_ci.R')
source('misc.R')
source('plotting.R')
source('numbers2words.R')

omit.zeroes <- function (x, digits=2) 
{
  sub("^.", "", format(round.p(x), nsmall = digits))
}

f.round <- function (x, digits){
  require(stringr)
  str_trim(format(round(as.numeric(x), digits), nsmall=digits))
}

round.p <- function(list, include.rel=1,digits=3){  
  list<-as.numeric(list)  
  ifelse(list<=0.001,"< .001",paste(ifelse(include.rel,"= ",""),sub("^.", "", format(round(list,digits=digits),nsmall=digits)),sep=""))
}

format.results <- function(res_str, type='pandoc'){
  require(stringr)
  if (type=='latex'){
    res_str
  }
  else if (type=='pandoc'){
    str_replace_all(res_str,'\\\\emph\\{(.*?)\\}','_\\1_')
  } 
}

describe.r <- function(rc,...){
  format_results(sprintf("\\emph{r}(%.0f) = %.2f, \\emph{p} %s",  rc$parameter, rc$estimate, round.p(rc$p.value)),...)
}

describe.ttest <- function (t,show.mean=F, abs=F,...){
  require(stringr)
  if (abs) t$statistic<-abs(t$statistic)
  if (show.mean==T)
    res_str=sprintf("\\emph{M} = %.2f [%.2f, %.2f], \\emph{t}(%i) = %.2f, \\emph{p} %s", t$estimate, t$conf.int[1], t$conf.int[2],t$parameter, t$statistic, round.p(t$p.value))
  else 
    res_str=sprintf("\\emph{t}(%.1f) = %.2f, \\emph{p} %s",  t$parameter, t$statistic, round.p(t$p.value))
  format_results(res_str, ...)
}

describe.mean.and.t <- function(x, by, which.mean=1, digits=2, type='pandoc', paired=F){
  library(Hmisc)
  summaries=Hmisc::summarize(x, by, smean.cl.boot)
  summaries<-transform(summaries, mean.descr=sprintf(paste0("\\emph{M} = %.",digits,"f [%.",digits,"f, %.",digits,"f]"), x, Lower, Upper))
  
  if (which.mean==3)
    means=paste(summaries[1,"mean.descr"],"vs.",summaries[2,"mean.descr"])
  else 
    means=summaries[which.mean,"mean.descr"];
  res_str=paste0(means,', ',describe.ttest(t.test(x~by, paired=paired)))
  format_results(res_str, ...)
  
}

lmer.fixef <- function (fit.lmer){
  ss <- sqrt(diag(as.matrix(vcov(fit.lmer))))
  cc <- fixef(fit.lmer)
  data.frame(Estimate =cc, Std.Err = ss, t = cc/ss)
}

describe.roc.diff <- function (roc_diff){
  sprintf("\\emph{D} = %0.2f, \\emph{p} %s",roc_diff$statistic,round.p(roc_diff$p.value))
}

describe.chi <- function (tbl, v=T, addN=T,...){
  if (length(dim(tbl))!=2&!is.matrix(tbl)) tbl<-table(tbl)
  chi<-chisq.test(tbl)
  cv <- sqrt(chi$statistic / (sum(tbl) * min(dim(tbl) - 1 )))
  n <- sum(tbl)
  res<-sprintf("$\\chi^2$(%i%s) = %.2f, \\emph{p} %s%s",chi$parameter,ifelse(addN,paste0(', \\emph{N} = ', sum(tbl)),''),chi$statistic,round.p(chi$p.value), ifelse(v, paste0(', \\emph{V} = ',omit_zeroes(round(cv,2)))))
  format_results(res, ...)
}


describe.aov <- function (fit, term, type=2,...){
  require(car)
  afit<-as.data.frame(Anova(fit, type=type))
  
  describe.Anova(afit, term, ...)
}

describe.Anova <- function (afit, term, f.digits=2, ...){
  res_str<-sprintf(paste0("\\emph{F}(%i, %i) = %.",f.digits,"f, \\emph{p} %s"), afit[term,"Df"], afit["Residuals","Df"], afit[term, "F value"], round.p(afit[term, "Pr(>F)"]))
  format_results(res_str, ...)
}

describe.glm <- function (fit, term, short=T, f.digits=2, latex=T){
  if (class(fit)== "lm.circular.cl"){
    mod_coef<-data.frame(fit$coefficients, fit$se.coef, fit$t.values, fit$p.values)
  }
  else {
    mod_coef <- coef(summary(fit))
  }
  if (latex){
  data.frame(B = f_round(mod_coef[, 1], 2), SE = f_round(mod_coef[, 2], 2), Z = f_round(mod_coef[, 3], 2), p = round.p(mod_coef[, 4]), eff=row.names(mod_coef),row.names = row.names(mod_coef), str=sprintf(paste0("\\emph{B} = %.",f.digits,"f (%.",f.digits,"f), \\emph{p} %s"), mod_coef[, 1], mod_coef[, 2], round.p(mod_coef[, 4])))
  } else {
    data.frame(B = f_round(mod_coef[, 1], 2), SE = f_round(mod_coef[, 2], 2), Z = f_round(mod_coef[, 3], 2), p = round.p(mod_coef[, 4]),eff=row.names(mod_coef), row.names = row.names(mod_coef), str=sprintf(paste0("*B* = %.",f.digits,"f (%.",f.digits,"f), *t*(%i) = %.2f, *p* %s"), mod_coef[, 1], mod_coef[, 2], summary(fit)$df[2], mod_coef[, 3], round.p(mod_coef[, 4])))    
  }
}

describe.glm.s <- function (fit, term=1, short=1, f.digits=2,...){
  require('stringr')
  if (class(fit)== "lm.circular.cl"){
    print(1)
    afit<-data.frame(fit$coefficients, fit$se.coef, fit$t.values, fit$p.values)
    t_z<-'t'
  }
  else {
    afit <- coef(summary(fit))
    t_z<-'Z'
  }
  if (length(attr(fit$terms, "term.labels"))==(length(rownames(afit))+1))
    rownames(afit)<-c("Intercept", attr(fit$terms, "term.labels"))
  if (short==1) {
    res_str=sprintf(paste0("\\emph{",t_z,"} = %.",f.digits,"f, \\emph{p} %s"), afit[term, 3], round.p(afit[term, 4]))
  }
  else if (short==2){
    res_str=sprintf(paste0("\\emph{B} = %.",f.digits,"f (%.",f.digits,"f), \\emph{p} %s"), afit[term, 1], afit[term, 2], round.p(afit[term, 4]))
  }
  else {
    res_str=sprintf(paste0("\\emph{B} = %.",f.digits,"f, \\emph{SE} = %.",f.digits,"f,  \\emph{",t_z,"} = %.",f.digits,"f, \\emph{p} %s"), afit[term, 1], afit[term, 2], afit[term, 3], round.p(afit[term, 4]))
  }
  format_results(res_str, ...)
  
}

describe.mean.sd <- function(x, digits=2,...){
  require(Hmisc)
  format_results(with(as.list(smean.sd(x)),sprintf(paste0("\\emph{M} = %.",digits,"f (\\emph{SD} = %.",digits,"f)"), Mean, SD)), ...)
}

describe.mean.conf <- function(x, digits=2,...){
  require(Hmisc)
  format_results(with(as.list(smean.cl.normal(x)),sprintf(paste0("\\emph{M} = %.",digits,"f [%.",digits,"f, %.",digits,"f]"), Mean, Lower, Upper)), ...)
}

describe.lmert <- function (sfit, factor, dtype='t',...){
  require('stringr')
  coef<-sfit$coefficients[factor,]
  if (sfit$objClass=='glmerMod'){
    test_name='z'
    test_df=''
    names(coef)<-str_replace(names(coef),'z','t')
  } else {
    test_name='t'
    test_df=paste0('(', round(coef['df']),')')
  }
  if (dtype=="t"){
    res_str<-sprintf("\\emph{%s}%s = %.2f, \\emph{p} %s",test_name,test_df, coef['t value'],round.p(coef['Pr(>|t|)']))
  }
  else if (dtype=="B"){
    res_str<-sprintf("\\emph{B} = %.2f (%.2f), \\emph{p} %s", coef['Estimate'], coef['Std. Error'],  round.p(coef['Pr(>|t|)']))
  }
  format_results(res_str,...)
}

describe.lmer <- function (fm, pv, digits = c(2, 2, 2), incl.rel = 0, dtype="B", incl.p=T) 
{
  require(lme4)
  cc <- lme4::fixef(fm)
  ss <- sqrt(diag(as.matrix(vcov(fm))))
  data <- data.frame(Estimate = cc, Std.Err = ss, t = cc/ss, 
                     p = pv[["fixed"]][, "Pr(>|t|)"], row.names = names(cc))
  for (i in c(1:3)) {
    data[, i] <- format(round(data[, i], digits[i]), nsmall = digits[i])
  }
  if (incl.p==F){
    data$str<-sprintf("\\emph{t} = %s, \\emph{p} %s", data$t, round.p(data[, 4],1))
  }
  if (dtype=="t"){
  data$str<-sprintf("\\emph{B} = %s (%s), \\emph{t} = %s", data$Estimate, data$Std.Err, data$t)
  }
  else if (dtype=="B"){
    data$str<-sprintf("\\emph{B} = %s (%s), \\emph{p} %s", data$Estimate, data$Std.Err,  round.p(data[, 4],1))
  }
  data[, 4] <- round.p(data[, 4], incl.rel)
  data
}

ins.lmer <- function (fm, term=NULL){
  cc <- lme4::fixef(fm)
  ss <- sqrt(diag(as.matrix(vcov(fm))))
  data <- data.frame(Estimate = cc, Std.Err = ss, t = cc/ss, row.names = names(cc))
  
  data$str<-sprintf("_B_ = %.2f (%.2f), _t_ = %.2f", data$Estimate, data$Std.Err, data$t)
  if (!is.null(term)) {
    data[term , 'str']
  }
  else {
    data
  }
}

describe.binom.mean.conf <- function(x, digits=2){
  require(Hmisc)
  
  format_results(with( data.frame(binconf(sum(x),length(x))),sprintf(paste0("\\emph{M} = %.",digits,"f [%.",digits,"f, %.",digits,"f]"), PointEst, Lower, Upper)))
}


ins.ezlmt <- function(eza, term, include_eta=T){
  eza<-eza$ANOVA
  rownames(eza)<-eza$Effect
  suffix <- ifelse(include_eta, sprintf(', $\\eta$^2^~G~ = %.3f', eza[term, "ges"]),'')
  sprintf("_F_(%.0f, %.0f) = %.2f, _p_ %s%s", eza[term,"DFn"],eza[term,"DFd"],eza[term,"F"],round.p(eza[term,"p"]), suffix) 
}


describe.dip.test <- function(x,...){
  require(diptest)
  res<-dip.test(x)
  res<-sprintf('\\emph{D} = %.2f, \\emph{p} %s', res$statistic, round.p(res$p.value))
  format_results(res,...)
}

table.mean.conf.by <- function(x, by, digits=2, binom=F){
  tapply(x, by, table_mean_conf, digits=2, binom=F)
}

table.mean.conf <- function(x, digits=2, binom=F, ...) {
  if (binom){
    res<-data.frame(binconf(sum(x),length(x)))
    colnames(res)<-c('Mean', 'Lower', 'Upper')
  }
  else{
    res<-as.list(smean.cl.normal(x))
  }
  res<-with(res,c(f_round(Mean, digits), sprintf(paste0("[%.",digits,"f, %.",digits,"f]"), Lower, Upper)))
  res
}

