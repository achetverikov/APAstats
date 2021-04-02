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
#' @param digits Number of decimal digits to keep
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
#' @param values a vector of p-values
#' @param include.rel include relation sign
#' @param digits a number of decimal digits
#' @param strip.lead.zeros remove zero before decimal point
#' @param replace.very.small replace values lower than this criteria (NULL to keep values as is)
#' @return Formatted p-value
#' @export round.p
#'
#' @examples
#' round.p(c(0.025, 0.0001, 0.001, 0.568))
#' round.p(c(0.025, 0.0001, 0.001, 0.568), digits=2)
#' round.p(c(0.025, 0.0001, 0.001, 0.568), include.rel=F)
#' round.p(c(0.025, 0.0001, 0.001, 0.568), include.rel=F, strip.lead.zeros=F)
#' round.p(c(0.025, 0.0001, 0.001, 0.568), include.rel=F, strip.lead.zeros=F, replace.very.small = 0.01)
round.p <- function(values, include.rel=1,digits=3, strip.lead.zeros=T, replace.very.small = 0.001){
  values<-as.numeric(values)
  rel <- ifelse(include.rel,"= ","")
  values_string <- format(round(values,digits=digits),nsmall=digits)
  if (strip.lead.zeros){
    values_string <- sub("^0", "", values_string)
  }
  values_string <- paste(rel,values_string, sep="")

  if (!is.null(replace.very.small)){
    values_string[abs(values)<=replace.very.small]<-paste0("< ",ifelse(strip.lead.zeros,sub("^0", "", replace.very.small),replace.very.small))
  }

  values_string

}

#' Format results
#'
#' Internal function used to convert latex-formatted results to pandoc style.
#'
#' @param res_str text
#' @param type 'pandoc', 'latex', or 'plotmath' (the latter is very poorly implemented)
#'
#' @return \code{res_str} with latex 'emph' tags replaced with pandoc '_'
#' @export format.results

format.results <- function(res_str, type='pandoc'){
  if (type=='latex'){
    res_str
  } else if (type=='pandoc'){
    stringr::str_replace_all(res_str,'\\\\emph\\{(.*?)\\}','_\\1_')
  } else if (type == "plotmath") {
    res_str<-stringi::stri_replace_all(res_str, regex = c("\\\\emph\\{(.*?)\\}",'='),
                                        replacement = c("italic($1)",'=='), vectorize_all=F)
    if (grepl(',',res_str)){
      res_str <- paste0('list(',res_str,')')
    }
  }
}

#' Describe Pearson test results
#'
#' @param rc an object from \link[stats]{cor.test}
#' @param ... other arguments passed to \link{format.results}
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
#' @param t an object from \link[stats]{t.test}
#' @param show.mean  include mean value in results (useful for one-sample test)
#' @param abs should we show the sign of t-test
#' @param ... other arguments passed to \link{format.results}
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
#' @param aggregate_by do the aggregation by the thrird variable(s): either NULL (default), a single vector variable, or a list of variables to aggregate by.
#' @param transform.means a function to transform means and confidence intervals (default: NULL)
#' @param ... other parameters passed to \link{format.results}
#'
#' @return result
#' @export
#'
#' @examples
#' data(faces)
#' rt <- faces$answerTime
#' gr <- faces$stim_gender
#' describe.mean.and.t(rt, gr)
#' describe.mean.and.t(rt, gr, which.mean = 3)
#' describe.mean.and.t(rt, gr, eff.size = T)
#'
#' sid <- faces$sid
#' describe.mean.and.t(rt, gr, which.mean = 3, aggregate_by = sid)
#' log_rt <- log(rt*1000)
#' describe.mean.and.t(log_rt, gr, which.mean = 3, aggregate_by = sid)
#' describe.mean.and.t(log_rt, gr, which.mean = 3, aggregate_by = sid, transform.means = exp)


describe.mean.and.t <- function(x, by, which.mean=1, digits=2, paired=F, eff.size=F, abs=F, aggregate_by=NULL, transform.means = NULL, ...){
  requireNamespace('Hmisc')
  if (lengthu(by)!=2)
    stop('"by" should have exactly two levels')
  if (!is.null(aggregate_by)){
    if (is.list(aggregate_by)) aggregate_by<-append(aggregate_by,list(by=by))
    else aggregate_by<-list(aggregate_by=aggregate_by, by=by)
    aggr_df<-Hmisc::summarize(x,aggregate_by,mymean)
    x<-as.numeric(aggr_df$x)
    by<-aggr_df$by
  }
  summaries<-Hmisc::summarize(x, by, Hmisc::smean.cl.boot)

  if (!is.null(transform.means)){
    summaries[,c('x','Lower','Upper')]<-sapply(summaries[,c('x','Lower','Upper')], exp)
  }

  summaries<-transform(summaries, mean.descr=sprintf(paste0("\\emph{M} = %.",digits,"f [%.",digits,"f, %.",digits,"f]"), x, Lower, Upper))

  if (which.mean==3)
    means = paste0(summaries[1,"mean.descr"]," vs. ",summaries[2,"mean.descr"],', ')
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
#' Describe Pearson $chi^2$ results. Note that the function takes table as an input rather than $chi^2$ object - this is necessary to get Cramer's V.
#'
#' @param tbl - MxN table on which to compute chi^2 (if not a table, will try to make a table out of it)
#' @param v - add Cramer's V (default: T)
#' @param addN - add N (default: T)
#' @param ... other parameters passed to \link{format.results}
#'
#' @return result
#' @export
#'
#' @examples
#'
#' ## From chisq.test help page
#' ## ---------------------
#' ## From Agresti(2007) p.39
#' M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
#' dimnames(M) <- list(gender = c("F", "M"),
#'                     party = c("Democrat","Independent", "Republican"))
#' (Xsq <- chisq.test(M))  # Prints test summary
#' ## ----------------------
#' describe.chi(M) # Note that describe.chi should be used for the table, not the chi2
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
#' @param fit fitted aov model
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param sstype anova SS type (e.g., 2 or 3)
#' @param ... other parameters passed to describe.Anova
#'
#' @return formatted string with F(df_numerator, df_denomiator) = F_value, p =/< p_value
#' @export
#'

describe.aov <- function (fit, term, sstype = 2, ...){
  afit<-as.data.frame(car::Anova(fit, type = sstype))

  describe.Anova(afit, term, ...)
}

#' Describe the results of model comparison with anova
#'
#' @param anova_res anova results
#' @param rown row number (default: 2)
#' @param f.digits number of digits in the results (default: 2)
#'
#' @return Formatted string with F (or chi2), df, and p
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
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param f.digits - decimal digits for F
#' @param ... other parameters passed to \link{format.results}
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
  afit <- data.frame(afit)
  res_str<-sprintf(paste0("\\emph{F}(%.0f, %.1f) = %.",f.digits,"f, \\emph{p} %s"), afit[term,"NumDF"], afit[term,"DenDF"], afit[term, "F.value"], round.p(afit[term, "Pr..F."]))
  format.results(res_str, ...)
}

#' Describe Anova results
#'
#' @param afit fitted \link[car]{Anova} model
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param f.digits number of digits for F
#' @param ... other parameters passed to \link{format.results}
#'
#' @return result
#' @export
#'
#' @examples
#' library(car)
#' mod <- lm(conformity ~ fcategory*partner.status, data=Moore, contrasts=list(fcategory=contr.sum, partner.status=contr.sum))
#' afit <- Anova(mod)
#' describe.Anova(afit, 'fcategory')
#' describe.Anova(afit, 2, 4)
#'

describe.Anova <- function (afit, term, f.digits=2, ...){
  res_str<-sprintf(paste0("\\emph{F}(%i, %i) = %.",f.digits,"f, \\emph{p} %s"), afit[term,"Df"], afit["Residuals","Df"], afit[term, "F value"], round.p(afit[term, "Pr(>F)"]))
  format.results(res_str, ...)
}

#' Describe regression model (GLM, GLMer, lm, lm.circular, ...)
#'
#' @param fit model object
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param dtype description type (1: t, p;  2: B(SE), p; 3: B, SE, t, p; or other: B (SE), t)
#' @param b.digits how many digits to use for _B_ and _SE_
#' @param t.digits how many digits to use for _t_
#' @param test.df should we include degrees of freedom in description?
#' @param eff.size should we include effect size (currently implemented only for simple regression)?
#' @param adj.digits automatically adjusts digits so that B or SE would not show up as "0.00"
#' @param ... other parameters passed to \link{format.results}
#'
#' @return result
#' @export
#'
#' @examples
#'
#' utils::data(Animals, package = "MASS")
#' Animals$body<-log(Animals$body)
#' Animals$brain<-log(Animals$brain)
#' fit <- lm(brain~body, Animals)
#' describe.glm(fit, 'body')
#' describe.glm(fit, '(Intercept)')
#' describe.glm(fit, 'body', 2)
#' describe.glm(fit, 'body', 3)
#' describe.glm(fit, 'body', 4)
#' describe.glm(fit, 'body', 3, test.df = 1)
#' describe.glm(fit)
#' ## Not run:
#' require(rockchalk)
#' describe.glm(fit, 'body', 4, eff.size = TRUE)
#' ## End(Not run)



describe.glm <- function (fit, term=NULL, dtype=1, b.digits=2, t.digits=2, test.df=F, p.as.number=F, term.pattern=NULL, eff.size = F, adj.digits=F, ...){
  requireNamespace('plyr')
  requireNamespace('Hmisc')

  fit_package = attr(class(fit),'package')
  fit_class = class(fit)[1]
  fit_family = family(fit)[1]

  if (!is.numeric(dtype) | dtype<0 |dtype>4) dtype = 3

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

    if (grepl('merModLmerTest',fit_class)){
      requireNamespace('lmerTest')
      afit <- data.frame(coef(summary(fit)))
      if (test.df){
        dfs<-afit[,3]
      }
      afit <- afit[,c(1,2,4,5)]
    }
    else {
      afit <- data.frame(coef(summary(fit)))
    }
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
  if (!is.null(term) && !(term %in% row.names(afit))){
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
    if (dtype!=4)
      warning('p-values for lmer are only a rough estimate from z-distribution, not suitable for the real use')
    afit$pvals<-2*pnorm(-abs(afit[,3]))
  }

  if (test.df){
    if (!grepl('merModLmerTest', fit_class)) {
      dfs<-summary(fit)$df[2]
    }
    if (isTRUE(all.equal(dfs, as.integer(dfs)))){
      dfs <- as.character(round(dfs))
    }
    else {
      dfs <- f.round(dfs, t.digits)
    }
    dfs <- paste0('(',dfs,')')
  }
  else dfs<-""

  res_df<-data.frame(B = f.round(afit[, 1], 2), SE = f.round(afit[, 2], 2), Stat = f.round(afit[, 3], t.digits), p = if(p.as.number) zapsmall(as.vector(afit[,4]),4) else round.p(afit[, 4]), eff=row.names(afit),row.names = row.names(afit))

  if (dtype==1) {
    res_df$str<-sprintf(paste0("\\emph{",t_z,"}",dfs," %s, \\emph{p} %s"), round.p(afit[, 3], digits=t.digits, strip.lead.zeros=F), round.p(afit[, 4]))
  }
  else if (dtype==2){
    res_df$str<-sprintf(paste0("\\emph{B} = %.",b.digits,"f (%.",b.digits,"f), \\emph{p} %s"), afit[, 1], afit[, 2], round.p(afit[, 4]))
  }
  else if (dtype==3){
    res_df$str<-sprintf(paste0("\\emph{B} = %.",b.digits,"f, \\emph{SE} = %.",b.digits,"f, \\emph{",t_z,"}", dfs," %s, \\emph{p} %s"), afit[, 1], afit[, 2], round.p(afit[, 3], digits=t.digits, strip.lead.zeros=F, replace.very.small = 0.01), round.p(afit[, 4]))
  }
  else if (dtype==4) {
    res_df$str<-sprintf(paste0("\\emph{B} = %.",b.digits,"f (%.",b.digits,"f), \\emph{",t_z,"}", dfs," %s"), afit[, 1], afit[, 2], round.p(afit[, 3], digits=t.digits, strip.lead.zeros=F, replace.very.small = 0.01))
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
#' Computes mean and SD for a vector (or just uses mean and sd if provided) and returns them as formatted string
#'
#' @param x vector of values
#' @param m mean (not used if x is provided)
#' @param sd SD (not used if x is provided)
#' @param digits number of digits used for description (default: 2)
#' @param dtype "p" for SD in parentheses (default), "c" for SD after comma
#' @param ... other arguments passed to \link{format.results}
#'
#' @return formatted string
#' @export
#'
#' @examples
#'
#' x <- rnorm(1000, 50, 25)
#' describe.mean.sd(x)
#' describe.mean.sd(x, 0)
#' describe.mean.sd(x, dtype = 'c')

describe.mean.sd <- function(x = NULL, m = NULL, sd = NULL, digits=2, dtype = 'p', ...){
  if (dtype == 'c'){s1 <- ', '; s2 <- ''}
  else {s1 <- ' ('; s2 <- ')'}

  if (!is.null(x)){
    m_sd <- as.list(Hmisc::smean.sd(x))
    m <- m_sd$Mean
    sd <- m_sd$SD
  }
  format.results(sprintf("\\emph{M} = %.*f%s\\emph{SD} = %.*f%s", digits, m, s1, digits, sd, s2), ...)
}

#' Describe mean and confidence intervals
#'
#' @param x value that should be described
#' @param bootCI use bootstrapped (T, default) or Gaussian (F) confidence intervals
#' @param addCI add "95\% CI =" before confidence intervals (default: F)
#' @param digits number of digits to use
#' @param transform.means an optional function to transform the means and CI to another scale
#' @param ... other arguments passed to \link{format.results}
#'
#' @return a string with a mean followed by confidence intervals in square brackets.
#'
#' @export
#'
#' @examples
#'
#' x <- runif(100, 0, 50)
#' describe.mean.conf(x)
#' describe.mean.conf(x, bootCI = F)
#' describe.mean.conf(x, digits = 5)
#' describe.mean.conf(x, transform.means = function(val) val*2)

describe.mean.conf<-function (x, bootCI = T, addCI = F, digits = 2, transform.means = NULL, ...)
{
  if (bootCI)
    res <- Hmisc::smean.cl.boot(x)
  else res <- Hmisc::smean.cl.normal(x)
  if (!is.null(transform.means)){
    res <- sapply(res, transform.means)
  }
  ci_str <- ifelse(addCI, ", 95%% \\emph{CI} =", "")
  format.results(with(as.list(res), sprintf(paste0("\\emph{M} = %.",
                                                   digits, "f", ci_str, " [%.", digits, "f, %.", digits,
                                                   "f]"), Mean, Lower, Upper)), ...)
}

#' Describe lmerTest results
#'
#' Note that this function uses *summary* object from lmerTest::lmer model and not the model itself (see the example). Otherwise p-values will be computed during the call and everything will be very slow.
#'
#' @param sfit *summary* object from lmerTest::lmer model
#' @param factor name or number of the factor that needs to be describe
#' @param dtype description type ("B"/"t")
#' @param ... other parameters passed to \link{format.results}
#'
#' @return Formatted string
#' @export
#'
#' @examples
#'
#' library(lmerTest)
#' fm <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
#' fms <- summary(fm)
#' describe.lmert(fms, 'Days')
#' describe.lmert(fms, '(Intercept)')
#' describe.lmert(fms, 2)
#' describe.lmert(fms, 'Days', 'B')
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


#' Describe linearHypothesis test results
#'
#' @param hyp hypothesis from \link[car]{linearHypothesis}
#' @param ... additional papameters passed to \link{format.results}
#'
#' @return results of \eqn{\chi^2} or _F_ test
#' @export
#'
#' @examples
#'
#' library(car)
#' mod.davis <- lm(weight ~ repwt, data=Davis)
#'
#' res <- linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"))
#' describe.lht(res)
#'
#' res.chi <- linearHypothesis(mod.davis, c("(Intercept) = 0", "repwt = 1"), test = "Chisq")
#' describe.lht(res.chi)


describe.lht <- function (hyp, ...){
  res<-hyp[2,]

  if ('Chisq' %in% names(res)) {
    res<-res[c('Df','Chisq','Pr(>Chisq)')]
    res[3]<-round.p(res[3])
    format.results(do.call(sprintf, c(list('$\\chi^2$(%g) = %.2f, \\emph{p} %s'), res)),...)
  } else {
    res[6]<-round.p(res[6])
    res<-res[c(3, 1, 5, 6)]
    format.results(do.call(sprintf, c(list('\\emph{F}(%g, %g) = %.2f, \\emph{p} %s'), res)),...)
  }
}

#' Describe contrasts created by lsmeans
#'
#' @param obj summary object from lsmeans::contrast
#' @param term contrast number
#' @param dtype description type, "t", "B", or any other letter
#' @param df include DF in t-test description (default: False)
#' @param ... other parameters passed to \link{format.results}
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


#' Describe contrasts created by emmeans
#'
#' @param obj summary object from emmeans::contrast
#' @param term contrast number
#' @param dtype description type, "t", "B", or any other letter
#' @param df include DF in t-test description (default: False)
#' @param ... other parameters passed to \link{format.results}
#'
#' @return string with formatted results
#' @export
#'
#' @examples
#'
#' require(emmeans)
#' warp.lm <- lm(breaks ~ wool*tension, data = warpbreaks)
#' warp.lsm <- emmeans(warp.lm, ~ tension | wool)
#' (sum_contr<-summary(contrast(warp.lsm, 'trt.vs.ctrl')))
#' describe.emmeans(sum_contr, 1)
#' describe.emmeans(sum_contr, 3)
#' describe.emmeans(sum_contr, 3, dtype='t')
#' describe.emmeans(sum_contr, 3, dtype='c')
#' describe.emmeans(sum_contr, 3, dtype='c', df=T)


describe.emmeans<-function(obj, term, dtype='B', df=F, ...){
  obj <- as.data.frame(obj[term,])
  if (dtype=="t"){
   res_str<-sprintf("\\emph{%s}%s = %.2f, \\emph{p} %s", 't', ifelse(df, paste0('(', round(obj['df']),')'),''), obj$t.ratio, round.p(obj$p.value))
  }
  else if (dtype=="B"){
   res_str<-sprintf("\\emph{B} = %.2f (%.2f), \\emph{p} %s", obj$estimate, obj$SE, round.p(obj$p.value))
  }
  else{
   res_str<-sprintf("\\emph{B} = %.2f (%.2f), \\emph{%s}%s = %.2f, \\emph{p} %s", obj$estimate, obj$SE,  't', ifelse(df, paste0('(', round(obj$df),')'),''), obj$t.ratio, round.p(obj$p.value))
  }
  format.results(res_str,...)
}


#' Describe lmer results
#'
#' This function is deprecated in favor of \link{describe.glm}
#'
#' @param fm LMER model from lme4
#' @param pv p values table (from Anova)
#' @param digits number of digits in results (default: 2)
#' @param incl.rel include the relation sign for _p_
#' @param dtype description type (B or t)
#' @param incl.p include p values
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
#' @param fm LMER model from lme4
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param  digits number of digits for B and SD
#' @param  adj.digits automatically adjusts digits so that B would not show up as "0.00"
#' @return result
#' @export
#'

ins.lmer <- function (fm, term=NULL, digits=2, adj.digits=T){
  describe.glm(fm, term=term, b.digits=digits, adj.digits = adj.digits, dtype=4)
}

#' Describe mean and confidence intervals for binomial variable
#'
#' @param x a vector of values
#' @param digits number of digits in results (default: 2)
#'
#' @return a string with the mean and confidence interval in square brackets
#' @export
#'
#' @examples
#'
#' describe.binom.mean.conf(faces$correct[1:100])
#' # note that it is slightly different from asymptotic CI
#' describe.mean.conf(faces$correct[1:100], bootCI = F)
#' # although similar to the bootstrapped CI
#' describe.mean.conf(faces$correct[1:100], bootCI = T)

describe.binom.mean.conf <- function(x, digits=2){
  format.results(with( data.frame(Hmisc::binconf(sum(x),length(x))),sprintf(paste0("\\emph{M} = %.",digits,"f [%.",digits,"f, %.",digits,"f]"), PointEst, Lower, Upper)))
}


#' Describe ezANOVA results
#' Provides formatted string like _F_(DFn, DFd) = ..., _p_ ..., eta2 = ... based on ezANOVA results
#'
#' @param ezfit ezANOVA object
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param include_eta add eta^2 for the model (default: True)
#' @param spher_corr use sphericity corrections	(default: True)
#' @param eta_digits number of digits to use for eta^2 (default: 2)
#' @param f_digits number of digits to use for F (default: 2)
#' @param df_digits number of digits to use for df (default: 0)
#' @param ... other parameters passed to \link{format.results}
#'
#' @return string with formatted results
#' @export
#'
#' @examples
#' library(ez)
#' data(faces)
#' ez_res <- ezANOVA(faces[faces$correct==1], dv = answerTime, wid = uid, within = stim_gender, between=user_gender)
#' describe.ezanova(ez_res, 'user_gender')
#' describe.ezanova(ez_res, 'user_gender', eta_digits = 3)
#' describe.ezanova(ez_res, 'user_gender:stim_gender', eta_digits = 3)
#' describe.ezanova(ez_res, 3, eta_digits = 3)

describe.ezanova <- function(ezfit, term, include_eta=T, spher_corr=T, eta_digits = 2, f_digits = 2, df_digits = 0, ...){
  eza<-ezfit$ANOVA
  if (spher_corr&('Sphericity Corrections' %in% names(ezfit))){
    eza<-merge(eza, ezfit$`Sphericity Corrections`, by='Effect', all.x=T)
    eza[!is.na(eza$GGe),'p']<-eza[!is.na(eza$GGe),]$`p[GG]`
  }
  rownames(eza)<-eza$Effect

  suffix <- ifelse(include_eta, sprintf(', $\\eta$^2^~G~ %s', round.p(eza[term, "ges"], digits = eta_digits, replace.very.small = 10^(-eta_digits))),'')
  res<-format.results(sprintf("\\emph{F}(%.*f, %.*f) = %.*f, \\emph{p} %s%s", df_digits, eza[term,"DFn"], df_digits, eza[term,"DFd"],f_digits,eza[term,"F"],round.p(eza[term,"p"]), suffix),...)
  res
}

#' Describe ezStats results
#'
#' Returns formatted string with mean and SD from ezStats object
#'
#' @param ez_stats data.frame returned by ezStats
#' @param term model term to describe (a string with the term name or its sequential number)
#' @param ... other parameters passed to describe.mean.sd
#'
#' If a string is used for a term and there is more than one factor, "X:Y:Z" format is assumed (value in first column : value in the second, and so on). If no term is supplied, the first row is used. So if you need to select a term based on two or more variables, you can also just filter ezStats result beforehand (or use a row number as a term).
#'
#' @return string with formatted results
#' @export
#'
#' @examples
#' library(ez)
#' data(faces)
#' ezstats_res <- ezStats(faces, dv = answerTime, wid = uid, within = .(stim_gender,correct))
#' ezstats_res
#' describe.ezstats(ezstats_res, 'M:0')
#' describe.ezstats(ezstats_res, 2)
#' describe.ezstats(ezstats_res, 2, digits=1)
#'

describe.ezstats <- function(ezstats_res, term = 1, ...){
  rownames(ezstats_res)<-apply(ezstats_res[,1:(which(colnames(ezstats_res)=='N')-1)],1, paste,collapse = ':')
  with(ezstats_res[term,], describe.mean.sd(m = Mean, sd = SD, ...))
}


#' Describe Hartigans' dip test results
#'
#' @param x Hartigans' dip test results
#' @param ... other parameters passed to \link{format.results}
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
#' @param x value vector for \link[bimodalitytest]{bimodality.test}
#' @param start_vec start vector for \link[bimodalitytest]{bimodality.test}
#' @param ... other parameters passed to \link{format.results}
#'
#' @return a string with the LR (likelihood ratio) and p
#' @export
#'

describe.bimod.test <- function(x,start_vec=NA,...){
  res<-bimodalitytest::bimodality.test(x,start_vec=start_vec)
  res<-sprintf('\\emph{LR} = %.2f, \\emph{p} %s', res@LR, round.p(res@p_value))
  format.results(res,...)
}

#' Get a list of means and confidence intervals by group
#'
#' @param x value to compute the mean for
#' @param by group labels
#' @param digits number of digits in results (default: 2)
#' @param binom compute binomial CI instead of the usual ones
#'
#' @return a list of means and CI with keys corresponding to the group labels
#' @export
#'
#' @examples
#' table.mean.conf.by(faces$answerTime, faces$correct, 4)
#'

table.mean.conf.by <- function(x, by, digits=2, binom=F){
  tapply(x, by, table.mean.conf, digits, binom)
}

#' Get a list with means and confidence intervals
#'
#' @param x value to compute the mean for
#' @param digits number of digits in results (default: 2)
#' @param binom compute binomial CI instead of the usual ones
#' @param ... other parameters passed to \link{format.results}
#'
#' @return a list with a mean and CI
#' @export
#'
#' @examples
#' table.mean.conf(faces$correct)
#'

table.mean.conf <- function(x, digits=2, binom=F, ...) {
  if (binom){
    res<-data.frame(binconf(sum(x),length(x)))
    colnames(res)<-c('Mean', 'Lower', 'Upper')
  }
  else{
    res<-as.list(smean.cl.boot(x))
  }
  res<-with(res,c(f.round(Mean, digits), sprintf(paste0("[%.",digits,"f, %.",digits,"f]"), Lower, Upper)))
  res
}

#' Paste several strings, add 'and' before last
#'
#' @param x vector of strings
#' @param sep separator (is not used for only two groups)
#' @param suffix suffix to append to each value before the separator
#'
#' @return a string iterating the values in x
#' @export
#'
#' @examples
#' data(iris)
#' # get mean petal width and SD by group
#' res <- as.vector(by(iris$Sepal.Width, iris$Species, describe.mean.sd))
#' res
#' paste_and(res)
#' paste_and(res, sep = ';')
#'
#' data(faces)
#' # get mean response times (in ms) by response accuracy
#' res <- as.vector(by(faces$answerTime*1000, faces$correct, describe.mean.sd))
#' res
#' # no comma with two groups
#' paste_and(res)
#' paste_and(res, suffix = ' ms')
#'
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
#' @param myform formula to use
#' @param dataset dataset to use
#'
#' @return result
#' @export
#' @examples
#' \dontrun{
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
#' }


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


#' Describe BayesFactor results
#'
#' @param bf an object of \link[BayesFactor]{BFBayesFactor} class
#' @param digits number of digits to use
#' @param top_limit numbers above that limit (or below the digits limit) will be converted to exponential notation (if convert_to_power is TRUE)
#' @param convert_to_power enable or distable converting of very small or very large numbers to exponential notation
#' @param ... other parameters passed to \link{format.results}
#' @return string describing the result
#' @note Code for converting to exponential notation is based on http://dankelley.github.io/r/2015/03/22/scinot.html
#' @export
#'
#' @examples
#'
#' require('BayesFactor')
#' data(puzzles)
#'
#' bfs <- anovaBF(RT ~ shape*color + ID, data = puzzles, progress=FALSE)
#' describe.bf(bfs[1])
#' describe.bf(bfs[2]/bfs[14])
#'

describe.bf<-function(bf, digits = 2, top_limit = 10000, convert_to_power = T, ...){
  bf_val <- exp(bf@bayesFactor[1])
  if ((bf_val<(10^(-digits)) | bf_val > top_limit) & convert_to_power){
    exponent <- floor(log10(bf_val))
    bf_val <- round(bf_val / 10^exponent, digits=digits)
    format.results(paste0("\\emph{BF} = $",bf_val, "\\times 10^{", as.integer(exponent), "}$"),...)
  }
  else format.results(sprintf(paste0("\\emph{BF} = %.",digits,"f"),exp(bf@bayesFactor[1])),...)
}

#' Describe brms model results
#'
#' @param mod model object
#' @param term model term to describe
#' @param trans an optional function to transform the results to another scale (default: NULL)
#' @param digits number of digits in the output
#' @param eff.size string describing how to compute an effect size (currently, either 'fe_to_all', 'part_fe', or 'part_fe_re')
#' @param eff.size.type type of the effect size ('r' or 'r2')
#' @param nsamples number of samples to use for the effect size computations
#'
#' @param ... other parameters passed to \link{format.results}
#'
#' @return string describing the result
#' @export
#'
#' @examples
#' ## Not run:
#' require(brms)
#' x <- rnorm(500, sd = 6)
#' y <- 4*x + rnorm(500)
#' fit <- brm(y~x, data=data.frame(x,y), chains = 1, iter = 500)
#' describe.brm(fit, 'x')
#' # convert x to another scale for output
#' describe.brm(fit, 'x', trans = function(val) val+5)
#' ## End(Not run)


describe.brm<-function(mod, term, trans = NULL, digits = 2, eff.size = F, eff.size.type = 'r', nsamples = 100, ci.type = 'HPDI', ...){

  post_samp <- posterior_samples(mod, c(paste0('b_',term)), exact_match = T)
  if (ncol(post_samp)>1){
    warning(paste('More than one match using',term,'term' ))
  }
  post_samp = post_samp[,1]
  if (is.function(trans)){
    post_samp = trans(post_samp)
  }
  if (ci.type == 'HPDI')
    ci <- bayestestR::hdi(post_samp, ci = 0.95)
  else
    ci <- bayestestR::eti(post_samp, ci = 0.95)
  # it is tricky to compute partial R2, this is just one of the approaches
  es = NULL
  if (eff.size!=F){
    var_res = var(resid(mod, nsamples = nsamples)[,1])
    var_term = var(mod$data[,term, with = F]*fixef(mod)[term,'Estimate'])

  }
  if ('fe_to_all'== eff.size){
    var_tot = var(fitted(mod, nsamples = nsamples)[,1])+var_res
    es = var_term/var_tot
  }  else if ('part_fe'==eff.size){
    # var_f = var(fitted(mod, re_formula = NA)[,1])
    es = var_term/(var_term+var_res)
  } else if ('part_fe_re'==eff.size){

    term_re <- data.table(ranef(mod)[[1]][,,term], keep.rownames = T)
    mod_data_preds <- merge(mod$data, term_re, by.x = names(ranef(mod)), by.y = 'rn')
    var_term = var(mod_data_preds[,term, with = F]*(fixef(mod)[term,'Estimate'] + mod_data_preds[,'Estimate']))
    var_tot = var_term+var_res

    es = var_term/var_tot
  }


  res_str <- sprintf(paste0('\\emph{b} = %.',digits, 'f, 95%% %s = [%.',digits,'f, %.',digits,'f]'),mean(post_samp), ci.type, ci[,'CI_low'], ci[,'CI_high'])
  if (!is.null(es)) {
    if (eff.size.type=='r')
      res_str <- paste0('_r_ = ', f.round(sqrt(es), digits = digits),', ',res_str)
    else
      res_str <- paste0(res_str, ', $R^2_part$ = ', f.round(es, digits = digits))
  }

  format.results(res_str,...)

}
