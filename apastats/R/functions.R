#' Omit leading zero from number
#'
#' @param x A number
#' @param digits Number of decimal digits to keep
#'
#' @return A number without leading zero
#' @export
#'
#' @examples
#' omit.zeroes(0.2312)
#' omit.zeroes(0.2312, digits=3)
#' omit.zeroes('000.2312', digits=1)

omit.zeroes <- function (x, digits=2)
{
  sub("^.", "", f.round(x, digits))
}

#' Formatted rounding
#'
#' @param x A number
#' @param digits Number of decimal digits to keep ()
#'
#' @return Value A number rounded to the specified number of digits
#' @export
#'
#' @examples
#' f.round(5.8242)
#' f.round(5.8251)
#' f.round(5.82999, digits=3)
#' f.round(5.82999, digits=4)
f.round <- function (x, digits=2){
  stringr::str_trim(format(round(as.numeric(x), digits), nsmall=digits))
}

#' Round *p*-value
#'
#' If p-value is <= 0.001, returns ".001" else returns p-value rounded to the specified number of digits, optionally including relation sign ("<" or "=").
#'
#' @param list a vector of p-values
#' @param include.rel include relation sign
#' @param digits a number of decimal digits
#' @param strip.lead.zeros remove zero before decimal point
#'
#' @return Formatted p-value
#' @export round.p
#'
#' @method generic class
#' #'
#' @examples
#' round.p(c(0.025, 0.0001, 0.001, 0.568))
#' round.p(c(0.025, 0.0001, 0.001, 0.568), digits=2)
#' round.p(c(0.025, 0.0001, 0.001, 0.568), include.rel=F)
#' round.p(c(0.025, 0.0001, 0.001, 0.568), include.rel=F, strip.lead.zeros=F)
round.p <- function(list, include.rel=1,digits=3, strip.lead.zeros=T){
  list<-as.numeric(list)
  if (strip.lead.zeros){
    ifelse(list<=0.001,"< .001",paste(ifelse(include.rel,"= ",""),sub("^.", "", format(round(list,digits=digits),nsmall=digits)),sep=""))
  }
  else {
    paste(ifelse(include.rel,"= ",""),sub("^.", "", format(round(list,digits=digits),nsmall=digits)),sep="")
  }
}

#' Format results
#'
#' Internal function used to convert latex-formatted results to pandoc style.
#'
#' @param res_str text
#' @param type 'pandoc' or 'latex'
#'
#' @return \code{res_str} with latex 'emph' tags replaced with pandoc '_'
#' @export format.results
#' @method generic class
format.results <- function(res_str, type='pandoc'){
  if (type=='latex'){
    res_str
  }
  else if (type=='pandoc'){
    stringr::str_replace_all(res_str,'\\\\emph\\{(.*?)\\}','_\\1_')
  }
}

#' Describe Pearson test results
#'
#' @param rc an object from \code{cor.test}
#' @param ... other arguments passed to \code{format.results}
#'
#' @return A string with correlation coefficient, sample size, and p-value.
#' @export
#'
#' @examples
#' x<-rnorm(40)
#' y<-x*2+rnorm(40)
#' rc<-cor.test(x,y)
#' describe.r(rc)
describe.r <- function(rc,...){
  format.results(sprintf("\\emph{r}(%.0f) = %.2f, \\emph{p} %s",  rc$parameter, rc$estimate, round.p(rc$p.value)),...)
}

#' Describe t-test results
#'
#' @param t an object from \code{t.test}
#' @param show.mean  include mean value in results (useful for one-sample test)
#' @param abs should we show the sign of t-test
#' @param ... other arguments passed to \code{format.results}
#'
#' @return A string with t-test value, degrees of freedom, p-value, and, optionally, a mean with 95% confidence interval in square brackets.
#' @export
#'
#' @examples
#' t_res<-t.test(rnorm(20, mean = -10, sd=2))
#' describe.ttest(t_res)
#' describe.ttest(t_res, show.mean=T)
#' describe.ttest(t_res, show.mean=T, abs=T)

describe.ttest <- function (t,show.mean=F, abs=F,...){
  if (abs) t$statistic<-abs(t$statistic)
  if (show.mean==T)
    res_str=sprintf("\\emph{M} = %.2f [%.2f, %.2f], \\emph{t}(%.1f) = %.2f, \\emph{p} %s", t$estimate, t$conf.int[1], t$conf.int[2],t$parameter, t$statistic, round.p(t$p.value))
  else
    res_str=sprintf("\\emph{t}(%.1f) = %.2f, \\emph{p} %s",  t$parameter, t$statistic, round.p(t$p.value))
  format.results(res_str, ...)
}



#' Describe two-sample t-test with means and effect sizes
#'
#' @param x dependent variable
#' @param by independent variable
#' @param which.mean which mean to show (0 - none, 1 - first group (default), 2 - second group, 3 - both)
#' @param digits number of digits in results (default: 2)
#' @param paired should it be a paired test (default: F)
#' @param eff.size should we include effect size (default: F)
#' @param abs should we show the absolute value if the t-test (T) or keep its sign (F, default)
#' @param ...
#'
#' @return result
#' @export
#'

describe.mean.and.t <- function(x, by, which.mean=1, digits=2, paired=F, eff.size=F, abs=F, ...){
  if (lengthu(by)!=2)
    stop('"by" should have exactly two levels')

  summaries=Hmisc::summarize(x, by, smean.cl.boot)
  summaries<-transform(summaries, mean.descr=sprintf(paste0("\\emph{M} = %.",digits,"f [%.",digits,"f, %.",digits,"f]"), x, Lower, Upper))

  if (which.mean==3)
    means = paste0(summaries[1,"mean.descr"],"vs. ",summaries[2,"mean.descr"],', ')
  else if (which.mean==0)
    means = ''
  else
    means = paste0(summaries[which.mean,"mean.descr"], ', ')

  res_str=paste0(means,describe.ttest(t.test(x~by, paired=paired), abs=abs))
  if (eff.size){
    requireNamespace('lsr')
    eff_size = lsr::cohensD(x~by, method = ifelse(paired, 'paired', 'unequal'))
    res_str=paste0(res_str,', \\emph{d} = ',f.round(eff_size, digits=digits))
  }

  format.results(res_str, ...)

}


#' Get nice matrix of fixed effects from lmer
#'
#' @param fit.lmer
#'
#' @return result
#' @export
#'

lmer.fixef <- function (fit.lmer){
  ss <- sqrt(diag(as.matrix(vcov(fit.lmer))))
  cc <- fixef(fit.lmer)
  data.frame(Estimate =cc, Std.Err = ss, t = cc/ss)
}

#' Describe differences between ROC curves
#'
#' @param roc_diff
#'
#' @return result
#' @export
#'

describe.roc.diff <- function (roc_diff){
  sprintf("\\emph{D} = %0.2f, \\emph{p} %s",roc_diff$statistic,round.p(roc_diff$p.value))
}

#' Describe $chi^2$ results
#'
#' @param tbl
#' @param v
#' @param addN
#' @param ...
#'
#' @return result
#' @export
#'

describe.chi <- function (tbl, v=T, addN=T,...){
  if (length(dim(tbl))!=2&!is.matrix(tbl)) tbl<-table(tbl)
  chi<-chisq.test(tbl)
  cv <- sqrt(chi$statistic / (sum(tbl) * min(dim(tbl) - 1 )))
  n <- sum(tbl)
  res<-sprintf("$\\chi^2$(%i%s) = %.2f, \\emph{p} %s%s",chi$parameter,ifelse(addN,paste0(', \\emph{N} = ', sum(tbl)),''),chi$statistic,round.p(chi$p.value), ifelse(v, paste0(', \\emph{V} = ',omit.zeroes(round(cv,2)))))
  format.results(res, ...)
}


#' Describe aov results
#'
#' @param fit
#' @param term
#' @param type
#' @param ...
#'
#' @return result
#' @export
#'

describe.aov <- function (fit, term, type=2,...){
  afit<-as.data.frame(car::Anova(fit, type=type))

  describe.Anova(afit, term, ...)
}

#' Describe the results of model comparison with anova
#'
#' @param anova_res anova results
#' @param rown row number (default: 2)
#' @param f.digits number of digits in the results (default: 2)
#'
#' @return
#' @export
#'
#' @examples
#'
describe.anova <- function (anova_res, rown=2, f.digits=2,...){
  if ('F' %in% names(anova_res)){
    res_str<-sprintf(paste0("\\emph{F}(%.0f, %.0f) = %.",f.digits,"f, \\emph{p} %s"), anova_res[rown,"Df"], anova_res[rown,"Res.Df"], anova_res[rown, "F"], round.p(anova_res[rown, "Pr(>F)"]))
  } else res_str<-sprintf(paste0("$\\chi^2$(%i) = %.",f.digits,"f, \\emph{p} %s"), anova_res[rown,"Df"], anova_res[rown,"Chisq"], round.p(anova_res[rown,"Pr(>Chisq)"]))
  format.results(res_str, ...)

}

#' Describe lmerTest anova results
#'
#' @param afit - lmerTest anova results
#' @param term - term name
#' @param f.digits - decimal digits for F
#' @param ...
#'
#' @return formatted string describing the results of anova
#' @export
#'
#' @examples
#' load.libs(c('lme4','lmerTest'))
#' data(faces)
#'
#' fit <- lmer(answerTime~correct*stim_gender+(1+correct*stim_gender|uid), faces)
#' afit <- anova(fit)
#' describe.lmtaov(afit, 'correct:stim_gender')
describe.lmtaov <- function (afit, term, f.digits=2, ...){
  res_str<-sprintf(paste0("\\emph{F}(%.0f, %.1f) = %.",f.digits,"f, \\emph{p} %s"), afit[term,"NumDF"], afit[term,"DenDF"], afit[term, "F.value"], round.p(afit[term, "Pr(>F)"]))
  format.results(res_str, ...)
}

#' Describe Anova results
#'
#' @param afit
#' @param term
#' @param f.digits
#' @param ...
#'
#' @return result
#' @export
#'

describe.Anova <- function (afit, term, f.digits=2, ...){
  res_str<-sprintf(paste0("\\emph{F}(%i, %i) = %.",f.digits,"f, \\emph{p} %s"), afit[term,"Df"], afit["Residuals","Df"], afit[term, "F value"], round.p(afit[term, "Pr(>F)"]))
  format.results(res_str, ...)
}

#' Describe regression model (GLM, GLMer, lm, lm.circular, ...)
#'
#' @param fit model object
#' @param term model term to describe
#' @param short description type (1, 2, 3, or other)
#' @param b.digits how many digits to use for _B_ and _SE_
#' @param t.digits how many digits to use for _t_
#' @param test.df should we include degrees of freedom in description?
#' @param eff.size should we include effect size (currently implemented only for simple regression)?
#' @param adj.digits automatically adjusts digits so that B or SE would not show up as "0.00"
#' @param ...
#'
#' @return result
#' @export
#'

describe.glm <- function (fit, term=NULL, short=1, b.digits=2, t.digits=2, test.df=F, p.as.number=F, term.pattern=NULL, eff.size = F, adj.digits=F, ...){
  requireNamespace('plyr')

  fit_package = attr(class(fit),'package')
  fit_class = class(fit)[1]
  fit_family = family(fit)[1]

  if (!is.numeric(short) | short<0 |short>4) short = 3

  if (fit_class== "lm.circular.cl"){
    print(1)
    afit<-data.frame(fit$coefficients, fit$se.coef, fit$t.values, fit$p.values)
    t_z<-'t'
    if (test.df){
      test.df=F
      warning('df for lm.circular are not implemented')
    }
  }
  else {
    afit <- data.frame(coef(summary(fit)))

    if (fit_family=='gaussian'){
      t_z<-'t'
    } else {
      t_z<-'Z'
    }
    if (eff.size){
      requireNamespace('rockchalk')
      ess <- rockchalk::getDeltaRsquare(fit)
    }
  }
  if (!is.null(term) && term %nin% row.names(afit)){
    stop(sprintf('Term %s is absent from the model', term))
  }
  if (!is.null(term) && adj.digits)
    while (plyr::round_any(min(abs(afit[term, 1]),abs(afit[term, 2])), 10^(-b.digits))<(10^(-b.digits)))
      b.digits = b.digits + 1
  else if (adj.digits)
    while (plyr::round_any(abs(afit[which.min(pmin(abs(afit[, 1]),abs(afit[, 2]))),1]), 10^(-b.digits))<(10^(-b.digits)))
      b.digits = b.digits + 1

  if (length(attr(terms(fit), "term.labels"))==(length(rownames(afit))+1))
    rownames(afit)<-c("Intercept", attr(terms(fit), "term.labels"))
  if (fit_class=='lmerMod'){
    if (short!=4)
      warning('p-values for lmer are only a rough estimate from z-distribution, not suitable for the real use')
    afit$pvals<-2*pnorm(-abs(afit[,3]))
  }


  res_df<-data.frame(B = f.round(afit[, 1], 2), SE = f.round(afit[, 2], 2), Stat = f.round(afit[, 3], t.digits), p = if(p.as.number) zapsmall(as.vector(afit[,4]),4) else round.p(afit[, 4]), eff=row.names(afit),row.names = row.names(afit))

  if (short==1) {
    res_df$str<-sprintf(paste0("\\emph{",t_z,"} %s, \\emph{p} %s"), round.p(afit[, 3], digits=t.digits, strip=F), round.p(afit[, 4]))
  }
  else if (short==2){
    res_df$str<-sprintf(paste0("\\emph{B} = %.",b.digits,"f (%.",b.digits,"f), \\emph{p} %s"), afit[, 1], afit[, 2], round.p(afit[, 4]))
  }
  else if (short==3){
    res_df$str<-sprintf(paste0("\\emph{B} = %.",b.digits,"f, \\emph{SE} = %.",b.digits,"f,  \\emph{",t_z,"}",ifelse(test.df,paste0('(',summary(fit)$df[2],')'),'')," %s, \\emph{p} %s"), afit[, 1], afit[, 2], round.p(afit[, 3], digits=t.digits, strip=F), round.p(afit[, 4]))
  }
  else if (short==4) {
    res_df$str<-sprintf(paste0("\\emph{B} = %.",b.digits,"f (%.",b.digits,"f),  \\emph{",t_z,"}",ifelse(test.df,paste0('(',summary(fit)$df[2],')'),'')," %s"), afit[, 1], afit[, 2], round.p(afit[, 3], digits=t.digits, strip=F))
  }

  if (eff.size&!exists('ess')){
    stop('Effect sizes are not implemented for THAT kind of models yet.')
  } else if (eff.size) {
    res_df$str<-paste0(res_df$str, ', $R_part^2$', round.p(c(NA,ess),digits = ifelse(min(ess)<0.01,3,2) ))
  }

  res_df$str<-format.results(res_df$str, ...)
  if (!is.null(term)){
    res_df[term, 'str']
  } else if (!is.null(term.pattern)){
    res_df[grepl(term.pattern,res_df$eff),]
  } else res_df
}

#' Describe mean and SD
#'
#' @param x
#' @param digits
#' @param ...
#'
#' @return result
#' @export
#'

describe.mean.sd <- function(x, digits=2,...){
  format.results(with(as.list(Hmisc::smean.sd(x)),sprintf(paste0("\\emph{M} = %.",digits,"f (\\emph{SD} = %.",digits,"f)"), Mean, SD)), ...)
}

#' Describe mean and confidence intervals
#'
#' @param x
#' @param digits
#' @param ...
#'
#' @return result
#' @export
#'

describe.mean.conf <- function(x, digits=2,...){
  format.results(with(as.list(Hmisc::smean.cl.normal(x)),sprintf(paste0("\\emph{M} = %.",digits,"f [%.",digits,"f, %.",digits,"f]"), Mean, Lower, Upper)), ...)
}

#' Describe lmerTest results
#'
#' @param sfit
#' @param factor
#' @param dtype
#' @param ...
#'
#' @return result
#' @export
#'

describe.lmert <- function (sfit, factor, dtype='t',...){

  coef<-sfit$coefficients[factor,]
  if (sfit$objClass=='glmerMod'){
    test_name='z'
    test.df=''
    names(coef)<-stringr::str_replace(names(coef),'z','t')
  } else {
    test_name='t'
    test.df=paste0('(', round(coef['df']),')')
  }
  if (dtype=="t"){
    res_str<-sprintf("\\emph{%s}%s = %.2f, \\emph{p} %s",test_name,test.df, coef['t value'],round.p(coef['Pr(>|t|)']))
  }
  else if (dtype=="B"){
    res_str<-sprintf("\\emph{B} = %.2f (%.2f), \\emph{p} %s", coef['Estimate'], coef['Std. Error'],  round.p(coef['Pr(>|t|)']))
  }
  format.results(res_str,...)
}



#' Describe contrasts created by lsmeans
#'
#' @param obj - summary object from lsmeans::contrast
#' @param term - contrast number
#' @param dtype - description type, "t", "B", or any other letter
#' @param df - should we include DF in t-test description
#' @param ... other parameters passed to format.results
#'
#' @return string with formatted results
#' @export
#'
#' @examples
#'
#' require(lsmeans)
#' warp.lm <- lm(breaks ~ wool*tension, data = warpbreaks)
#' warp.lsm <- lsmeans(warp.lm, ~ tension | wool)
#' (sum_contr<-summary(contrast(warp.lsm, 'trt.vs.ctrl')))
#' describe.lsmeans(sum_contr, 1)
#' describe.lsmeans(sum_contr, 3)
#' describe.lsmeans(sum_contr, 3, dtype='t')
#' describe.lsmeans(sum_contr, 3, dtype='c')
#' describe.lsmeans(sum_contr, 3, dtype='c', df=T)


describe.lsmeans<-function(obj, term, dtype='B', df=F, ...){
  obj<-obj[term,]
  if (dtype=="t"){
    res_str<-sprintf("\\emph{%s}%s = %.2f, \\emph{p} %s", 't', ifelse(df, paste0('(', round(obj['df']),')'),''), obj['t.ratio'], round.p(obj['p.value']))
  }
  else if (dtype=="B"){
    res_str<-sprintf("\\emph{B} = %.2f (%.2f), \\emph{p} %s", obj['estimate'], obj['SE'], round.p(obj['p.value']))
  }
  else{
    res_str<-sprintf("\\emph{B} = %.2f (%.2f), \\emph{%s}%s = %.2f, \\emph{p} %s", obj['estimate'], obj['SE'],  't', ifelse(df, paste0('(', round(obj['df']),')'),''), obj['t.ratio'], round.p(obj['p.value']))
  }
  format.results(res_str,...)
}

#' Describe lmer results
#'
#' @param fm
#' @param pv
#' @param digits
#' @param incl.rel
#' @param dtype
#' @param incl.p
#'
#' @return result
#' @export
#'

describe.lmer <- function (fm, pv, digits = c(2, 2, 2), incl.rel = 0, dtype="B", incl.p=T)
{
  .Deprecated("describe.glm")
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

#' Describe lmer in-text
#'
#' A shortcut for describe.glm(..., short=4)
#'
#' @param fm
#' @param term
#' @param  digits number of digits for B and SD
#' @param  adj.digits automatically adjusts digits so that B would not show up as "0.00"
#' @return result
#' @export
#'

ins.lmer <- function (fm, term=NULL, digits=2, adj.digits=T){
  describe.glm(fm, term=term, b.digits=digits, adj.digits = adj.digits, short=4)
}

#' Describe mean and confidence intervals for binomial variable
#'
#' @param x
#' @param digits
#'
#' @return result
#' @export
#'

describe.binom.mean.conf <- function(x, digits=2){
  format.results(with( data.frame(Hmisc::binconf(sum(x),length(x))),sprintf(paste0("\\emph{M} = %.",digits,"f [%.",digits,"f, %.",digits,"f]"), PointEst, Lower, Upper)))
}


#' Describe ezANOVA results
#'
#' @param ezfit
#' @param term
#' @param include_eta
#' @param spher_corr
#' @param ...
#'
#' @return result
#' @export
#'

describe.ezanova <- function(ezfit, term, include_eta=T, spher_corr=T,...){
  eza<-ezfit$ANOVA
  if (spher_corr&('Sphericity Corrections' %in% names(ezfit))){
    eza<-merge(eza, ezfit$`Sphericity Corrections`, by='Effect', all.x=T)
    eza[!is.na(eza$GGe),'p']<-eza[!is.na(eza$GGe),]$`p[GG]`
  }
  rownames(eza)<-eza$Effect

  suffix <- ifelse(include_eta, sprintf(', $\\eta$^2^~G~ = %.2f', eza[term, "ges"]),'')
  res<-format.results(sprintf("\\emph{F}(%.0f, %.0f) = %.2f, \\emph{p} %s%s", eza[term,"DFn"],eza[term,"DFd"],eza[term,"F"],round.p(eza[term,"p"]), suffix))
  res
}


#' Describe Hartigans' dip test results
#'
#' @param x
#' @param ...
#'
#' @return result
#' @export
#'

describe.dip.test <- function(x,...){
  res<-diptest::dip.test(x)
  res<-sprintf('\\emph{D} = %.2f, \\emph{p} %s', res$statistic, round.p(res$p.value))
  format.results(res,...)
}

#' Describe bimodality test results
#'
#' @param x
#' @param start_vec
#' @param ...
#'
#' @return result
#' @export
#'

describe.bimod.test <- function(x,start_vec=NA,...){
  res<-bimodalitytest::bimodality.test(x,start_vec=start_vec)
  res<-sprintf('\\emph{LR} = %.2f, \\emph{p} %s', res@LR, round.p(res@p_value))
  format.results(res,...)
}

#' Show table of means and confidence intervals by group
#'
#' @param x
#' @param by
#' @param digits
#' @param binom
#'
#' @return result
#' @export
#'

table.mean.conf.by <- function(x, by, digits=2, binom=F){
  tapply(x, by, table.mean.conf, digits=2, binom=F)
}

#' Show table of means and confidence interval
#'
#' @param x
#' @param digits
#' @param binom
#' @param ...
#'
#' @return result
#' @export
#'

table.mean.conf <- function(x, digits=2, binom=F, ...) {
  if (binom){
    res<-data.frame(binconf(sum(x),length(x)))
    colnames(res)<-c('Mean', 'Lower', 'Upper')
  }
  else{
    res<-as.list(smean.cl.normal(x))
  }
  res<-with(res,c(f.round(Mean, digits), sprintf(paste0("[%.",digits,"f, %.",digits,"f]"), Lower, Upper)))
  res
}

#' Paste several strings, add 'and' before last
#'
#' @param x
#' @param sep
#' @param suffix
#'
#' @return result
#' @export
#'

paste_and<-function(x, sep=', ', suffix=''){
  collapse <- paste0(suffix, sep)
  last_sep <- ifelse(length(x)>2, paste0(collapse, 'and '), paste0(suffix,' and '))
  paste0(paste0(x[1:(length(x)-1)], collapse=collapse), last_sep,x[length(x)], suffix)
}

#' Run lmer with Julia
#'
#' Fits maximal model
#'
#' @param myform
#' @param dataset
#'
#' @return result
#' @export
#' @examples
#'
#' load.libs(c('lme4','rjulia','data.table'))
#' julia_init()
#' data(faces)
#'
#' ptm <- proc.time()
#' summary(lmer(answerTime~correct*stim_gender+(1+correct*stim_gender|uid), faces))
#' proc.time() - ptm
#'
#' ptm <- proc.time()
#' lmer_with_julia(answerTime~correct*stim_gender+(1|uid), faces)
#' proc.time() - ptm
#'
#' #Bizzare model - user gender cannot be a random effect within users, only for demonstration
#' ptm <- proc.time()
#' summary(lmer(answerTime~correct*stim_gender*user_gender+(1+correct*stim_gender*user_gender|uid), faces))
#' proc.time() - ptm
#'
#' ptm <- proc.time()
#' lmer_with_julia(answerTime~correct*stim_gender*user_gender+(1|uid), faces)
#' proc.time() - ptm
#'


lmer_with_julia<-function(myform, dataset){
  #Note that julia_init() should be run before using that function
  require('formula.tools')
  requireNamespace('ordinal')
  rjulia::j2r('using MixedModels')

  myform<-formula(myform)
  grouping_var<-all.vars(lme4::findbars(myform)[[1]])

  dataset<-na.omit(dataset[,all.vars(myform), with=F])
  mm<-model.matrix(lme4::nobars(myform), dataset)
  mm<-ordinal::drop.coef(mm)
  truenames<-colnames(mm)
  mm<-mm[,2:ncol(mm)] #removing Intercept
  names_for_julia<-letters[1:ncol(mm)]
  colnames(mm)<-names_for_julia
  mm<-cbind(mm, dataset)
  new.formula<-paste0(formula.tools::lhs(myform),'~',paste(names_for_julia, collapse = '+'),'+(',paste(names_for_julia, collapse = '+'),'|',grouping_var,')')
  rjulia::r2j(mm,'mm')
  expr<-paste0('mod_fit = fit(lmm(',new.formula,',mm))')
  print(expr)
  rjulia::j2r(expr)

  res<-rjulia::j2r('DataFrame(Estimate=fixef(mod_fit), StdError = stderr(mod_fit), Z = fixef(mod_fit)./stderr(mod_fit))')
  row.names(res)<-truenames
  res<-round(res, 2)
  res
}

#' Double aggregation
#'
#' Aggregates value twice providing mean of means, SD of SDs, etc.
#'
#' @param x value to aggregate
#' @param by vector to aggregate by (e.g., ID of participant)
#' @param fun function to apply
#' @param ... additional parameters passed to fun
#'
#' @return value aggregated first by specified vector and then aggregated again
#' @export
#'
#' @examples
#'
#' x<-rnorm(100)
#' id<-rep(1:10, each=10)
#'
#' aggregate(x~id, FUN=mean)
#' aggr2(x, id, mean)
#'
aggr2<-function (x, by, fun, ...){
  if (!is.list(by)) by<-list(by)
  fun(aggregate(x,by, FUN=fun, ...)$x)
}
