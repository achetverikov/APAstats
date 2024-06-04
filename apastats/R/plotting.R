#' Tufte-like breaks for axes
#'
#' @param x vector of numbers to create breaks from
#' @param scale scale (x or y) to create breaks for
#' @param addSegment should we add a line to the scale? (T/F)
#' @param ... other parameters passed to scale_(x or y)_continuous
#'
#' @return scale_(x or y)_continuous with pretty breaks and accompaniying geom_segment if addSegment == TRUE
#' @export
#'
base.breaks<-function(x, scale='x', addSegment= TRUE, ...){
  b <- pretty(x)

  b[1]<-min(x)
  b[length(b)]<-max(x)

  if (round((b[length(b)]-b[length(b)-1])-(b[length(b)-1]-b[length(b)-2]),5)>0){
    b<-b[c(1:(length(b)-2),length(b))]
  }
  print(b)


  if (scale=='x')
    scale_fun <- scale_x_continuous(breaks=b,...)
  else if (scale=='y') scale_fun <- scale_y_continuous(breaks=b,...)
  else stop('Scale parameter should be "x" or "y"')

  if (addSegment){
    if (scale=='y')
      list(geom_segment(data=data.frame(y=min(b), yend=max(b), x=-Inf, xend=-Inf), aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE), scale_fun)
    else list(geom_segment(data=data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b)), aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE), scale_fun)
  }
  else scale_fun
}


#' Tufte-like breaks for X axis
#'
#' @param x vector of numbers to create breaks from
#' @param addSegment should we add a line to the scale? (T/F)
#'
#' @return scale_x_continuous with pretty breaks
#' @export
#'
#'

base.breaks.x <- function(x, addSegment= TRUE, ...){
  base.breaks(x, scale = 'x', addSegment = addSegment, ...)
}


#' Tufte-like breaks for Y axis
#'
#' @param x vector of numbers to create breaks from
#' @param addSegment should we add a line to the scale? (T/F)
#'
#' @return scale_y_continuous with pretty breaks
#' @export
#'
#'

base.breaks.y <- function(x, addSegment= TRUE, ...){
  base.breaks(x, scale = 'y', addSegment = addSegment, ...)
}


#' Pointrange plot
#'
#' @param ... the first element is assumed to be the dataset and the second one is assumed to be a list of plot aesthetics (e.g., aes(x = varA, y=varB))
#' @param pos position adjustment function (e.g., position_dodge())
#' @param pointsize size for the points (e.g., "I(1)")
#' @param linesize size for the lines (e.g., "I(1)")
#' @param pointfill fill for the points (e.g., "I('white')")
#' @param pointshape shape for the points (e.g., "I(22)")
#' @param within_subj should we use within-subject adjustment?
#' @param wid within-subject ID variable
#' @param bars should we use confidence intervals ("ci") or standard errors ("se")?
#' @param withinvars within-subject variables
#' @param betweenvars between-subject variables
#' @param x_as_numeric should we treat x as numeric (otherwise it is treated as is)?
#' @param custom_geom_before any custom geom to add before points and connecting line (geoms that should be on top of the pointrange can be added in a usual manner)
#' @param connecting_line should we add connecting line (T/F)?
#' @param pretty_breaks_y prettify y breaks
#' @param pretty_y_axis prettify y axis
#' @param exp_y exponentiate y (it's better to use this than scale_y_exp if pretty_breaks_y is used)
#' @param print_aggregated_data print aggregated data used for plotting to console
#' @param do_aggregate aggregate data by all conditions before plotting (False)
#' @param add_margin add margin that would show the aggregate over all or subset of x-axis values (T/F)
#' @param margin_label margin label to use
#' @param margin_x_vals which levels of x-axis variable to aggregate over (NULL means all of them)
#' @param bars_instead_of_points use geom_bar instead of geom_point
#' @param geom_bar_params parameters for geom_bar if it is used
#' @param add_jitter add jittered individual data points
#' @param individual_points_params a list of parameters for the individual data points
#' @param drop_NA_subj drop subjects that have NAs after aggregation when computing within-subject errorbars
#'
#' @details For point and line properties (e.g., pointfill) passing NULL allows to avoid setting these values (useful when they are mapped to some variables).
#'
#' @return plot of pointrange
#' @export plot.pointrange
#' @method generic class
#' @examples
#' data(faces)
#' plot.pointrange(faces, aes(x=user_gender, color=stim_gender, y=answerTime))+ylab('RT')
#' plot.pointrange(faces, aes(x=user_gender, color=stim_gender, y=answerTime), wid='uid', within_subj= TRUE, withinvars=c('stim_gender'), betweenvars=c('user_gender'))+ylab('RT')
#' plot.pointrange(faces, aes(x=user_gender, shape=stim_gender, y=answerTime), wid='uid', withinvars=c('stim_gender'), betweenvars=c('user_gender'), within_subj= TRUE, bars='se')+ylab('RT')
#' plot.pointrange(faces, aes(x=user_gender, shape=stim_gender, y=answerTime), wid='uid', withinvars=c('stim_gender'), betweenvars=c('user_gender'), within_subj= TRUE, bars='se', print_aggregated_data= TRUE)+ylab('RT')
#' plot.pointrange(faces, aes(x=user_gender, shape=stim_gender, y=answerTime), withinvars=c('stim_gender'), betweenvars = c('user_gender'), print_aggregated_data = TRUE, wid='uid', within_subj= TRUE, exp_y= TRUE, bars='ci', do_aggregate = TRUE)+ylab('RT')


plot.pointrange<-function (data, mapping, pos = position_dodge(0.3), pointsize = I(3), linesize = I(1),
                           pointfill = I("white"), pointshape = NULL, within_subj = F,
                           wid = "uid", bars = "ci", withinvars = NULL, betweenvars = NULL,
                           x_as_numeric = F, custom_geom_before = NULL, connecting_line = F,
                           pretty_breaks_y = F, pretty_y_axis = F, exp_y = F, print_aggregated_data = F,
                           do_aggregate = F, add_margin = F, margin_label = 'all', margin_x_vals = NULL,
                           bars_instead_of_points = F, geom_bar_params = NULL, add_jitter = F,
                           individual_points_params = list(), drop_NA_subj = F){
  library(ggplot2)
  # ellipses <- list(...)
  # plot_f <- ellipses[[2]]
  plot_f <- mapping
  if (class(plot_f["y"]) == "uneval") {
    plot_f <- sapply(plot_f, function(x) sub("~", "", deparse(x)))
  }
  dv <- as.character(plot_f["y"][[1]])
  plot_vars <- as.character(unlist(plot_f[names(plot_f) != "y"]))
  if (length(setdiff(plot_vars, c(betweenvars, withinvars)))>0){
    warning(paste0('Variables ', paste_and(unique(setdiff(plot_vars, c(betweenvars, withinvars)))),' are not listed in withinvars or betweenvars but are used as plot parameters. They would be considered as betweenvars. '))
  }
  betweenvars <- union(betweenvars, setdiff(plot_vars, withinvars))
  withinvars <- unique(withinvars)
  # plot_data <- as.data.frame(ellipses[[1]])
  plot_data <- as.data.frame(data)
  plot_data <- plot_data[!is.na(plot_data[, dv]), ]
  aes_list <- modifyList(lapply(plot_f[names(plot_f) != "y"],
                                as.character), list(y = dv, ymin = "ymin", ymax = "ymax"))
  if (do_aggregate) {
    plot_data <- apastats:::summarySE(plot_data, measurevar = dv,
                                      groupvars = c(withinvars, betweenvars, wid), na.rm = TRUE)
  }
  if (within_subj) {
    if (length(withinvars)==0 || is.null(withinvars)){
      stop('Within-subject plot can only be made if there is at least one within-subject variable listed in withinvars parameter.')
    }
    # aggr_data <- apastats:::summarySEwithin(plot_data, measurevar = dv,
    #                                         withinvars = withinvars, betweenvars = betweenvars,
    #                                         idvar = wid, na.rm = TRUE)

    aggr_data <- plot_data[!is.na(plot_data[[dv]]),] |> 
      apastats::get_superb_ci(value_var = dv, within =  withinvars, between = betweenvars, wid = wid, errorbar = bars, drop_NA_subj = drop_NA_subj)
    
  }
  else {
    aggr_data <- apastats:::summarySE(plot_data, measurevar = dv,
                                      groupvars = c(withinvars, betweenvars),
                                      na.rm = TRUE)
  }
  if (x_as_numeric) {
    aggr_data[, aes_list$x] <- as.numeric(as.character(aggr_data[,                                                                  aes_list$x]))
  }
  if (add_margin){
    if (is.null(margin_x_vals)) {
      margin_x_vals <- unique(aggr_data[, aes_list$x])
    }
    data_for_margin<-aggr_data[aggr_data[, aes_list$x] %in% margin_x_vals,]
    if (within_subj){
      summ_data_for_margin<-apastats:::summarySEwithin(data_for_margin, measurevar = dv,
                                                       withinvars = withinvars[withinvars %nin% aes_list$x], betweenvars = betweenvars[betweenvars %nin% aes_list$x], idvar = aes_list$x, na.rm = TRUE)
      
    } else {
      groupvars = c(withinvars, betweenvars)
      summ_data_for_margin<-apastats:::summarySE(data_for_margin, measurevar = dv,
                                                 groupvars = groupvars[groupvars %nin% aes_list$x], na.rm = TRUE)
    }
    summ_data_for_margin[,aes_list$x]<-margin_label
    aggr_data<-plyr::rbind.fill(aggr_data, summ_data_for_margin)

    x_levels <- unique(aggr_data[, aes_list$x])
    aggr_data[, aes_list$x]<-factor(aggr_data[, aes_list$x], levels = c(x_levels[x_levels!=margin_label], margin_label))
  }
  if (within_subj) {
    aggr_data$ymin <- aggr_data[, 'center'] + aggr_data[, 'lowerwidth']
    aggr_data$ymax <- aggr_data[, 'center'] + aggr_data[, 'upperwidth']
    aggr_data[,dv] <- aggr_data$center
  } else {
    aggr_data$ymin <- aggr_data[, dv] - aggr_data[, bars]
    aggr_data$ymax <- aggr_data[, dv] + aggr_data[, bars]
  }
  if (exp_y) {
    aggr_data$ymin <- exp(aggr_data$ymin)
    aggr_data$ymax <- exp(aggr_data$ymax)
    aggr_data[, dv] <- exp(aggr_data[, dv])
    plot_data[,dv] <- exp(plot_data[,dv])
  }
  if (print_aggregated_data) {
    print(aggr_data)
  }
  p <- ggplot(aggr_data, do.call(aes_string, aes_list))
  if (!is.null(custom_geom_before)) {
    p <- p + custom_geom_before
  }
  line_params <- list(position = pos)
  if (!is.null(linesize))
    line_params <- append(line_params, list(size = linesize))
  if (connecting_line)
    p <- p + do.call(geom_line, line_params)
  p <- p + do.call(geom_linerange, line_params)
  point_params <- list(position = pos)
  if (!is.null(pointshape)) {
    point_params <- append(point_params, list(shape = pointshape))
  }
  if (!is.null(pointfill)) {
    point_params <- append(point_params, list(fill = pointfill))
  }
  if (!is.null(pointsize)) {
    point_params <- append(point_params, list(size = pointsize))
  }
  if (add_jitter){
    default_ind_pp <- list(size = pointsize/2, fill = 'lightgray', position = position_jitterdodge(dodge.width = pos$width, jitter.width = 0.1))
    individual_points_params <- append(individual_points_params, default_ind_pp[setdiff(names(default_ind_pp), names(individual_points_params))])
    p <- p+do.call(geom_jitter, append(individual_points_params, list(data = plot_data, mapping = do.call(aes_string, aes_list[!names(aes_list)%in%c('ymax','ymin')]), inherit.aes = F)))
  }
  if (!bars_instead_of_points){
    p <- p + do.call(geom_point, point_params)
  } else {
    p <- p + do.call(geom_bar, geom_bar_params)
  }

  if (pretty_breaks_y) {
    y_range <- c(min(aggr_data$ymin), max(aggr_data$ymax))
    breaks <- labeling::extended(y_range[1], y_range[2], 5)
    limits <- range(c(breaks, y_range))
    p <- p + scale_y_continuous(breaks = breaks) + coord_cartesian(ylim = limits)
    if (pretty_y_axis) {
      d <- data.frame(x = -Inf, xend = -Inf, y = min(breaks),
                      yend = max(breaks))
      p <- p + geom_segment(data = data.frame(x = -Inf,
                                              xend = -Inf, y = min(breaks), yend = max(breaks)),
                            aes(x = x, xend = xend, y = y, yend = yend),
                            inherit.aes = F)
    }
  }
  p
}
#' Exponentiate scale
#'
#' Helps to transform log-scale back to normal
#'
#' @param digits number of digits to use in lables
#' @param  ... other arguments passed to scale_y_continuous
#'
#' @return scale
#' @export
#'
scale_y_exp<-function(digits=0, ...){
  requireNamespace('scales')
  scale_y_continuous(breaks=trans_breaks('exp',function (x) log(x)), labels=trans_format('exp', function(x) as.character(f.round(x, digits=digits))), ...)
}

#' Extract grob element by name
#' Not mine, found somewhere
#' @param myggplot plot to extract grob from
#' @param el element name to extract
#'
#' @return grob element
#' @export
#'
#'
get_grob_element<-function(myggplot, el='guide-box'){
  tmp <- ggplot_gtable(ggplot_build(myggplot))

  leg <- which(tmp$layout$name==el)
  legend <- tmp$grobs[[leg]]
  return(legend)
}


#' Internal function for palette of shapes
#'
#' @return palette of shapes
#' @export
#'
#'
ac_shape_pal<-function(){
    shapes <- c(22,24,25,23,21,1:5)
    max_n <- length(shapes)
    f <- manual_pal(shapes)
    attr(f, "max_n") <- max_n
    f
}

#' Nice palette of shapes
#'
#' @param ...
#'
#' @return scale with nice shapes palette
#' @export
#'
#'
scale_shape_ac<-function(...)
{
  discrete_scale("shape", "ac", ac_shape_pal(), ...)

}

mkMyTable <- function(X){
  Table <- data.frame( table(X) )
  Table$Prop <- prop.table( Table$Freq )
  Table$CumProp <-  cumsum( Table$Prop )
  Table
}

mosaic.plot <- function(tbl){

  ptbl<-prop.table(tbl)

  df<-as.data.frame(tbl, responseName='n')

  #calculate column heights as a cumulative proprortion within levels of field
  grants<-ddply(grants, .(field), transform, ymax=cumsum(n)/sum(n), ymin=(cumsum(n)-n)/sum(n), ymid=(cumsum(n)-n/2)/sum(n))

  #calculate columns widths as a summary n within the levels of field
  grants<-ddply(grants, .(field), transform, xmax=sum(n))

  #add data about residuals in the data.frame
  grants<-merge(grants, as.data.frame(chi$stdres, responseName='res'), by=c("field","npersons_bin") )

  #add human-readable labels
  grants$field<-factor(grants$field, levels=c("soc","beh_cog","chem","physics"), labels=c("Social","Behavioral/cognitive","Chemistry","Physics"))

  #set basic plot structur
  p<-ggplot(data=grants,aes(xmin=0, ymin=ymin, xmax=xmax, ymax=ymax, fill=res)) +
    #squares
    geom_rect(color=I("white"), show_guide=F, size=I(3)) +
    #labels with frequecies
    geom_text(aes(label=n, x=25, y=ymin+0.05), color=I("white"), size=3) +
    #use facets to separate into columns
    facet_grid(~field,scales="free_x",space="free_x")

  #add theme, remove noise
  p<-p+ mytheme+theme(panel.margin = unit(0, "lines"),panel.grid=element_blank(),strip.background=element_blank(), line = element_blank(),
                      axis.text.x = element_blank(),  axis.line = element_blank(), axis.title = element_blank())+scale_x_continuous(expand=c(0,1))

  #add labels for counts on the right
  p<-p+scale_y_continuous(expand=c(0,0), breaks=grants[grants$field=="Social","ymid"], labels=c("1 person", "2-3 persons", "3 and more"))

  #colorize based on residuals
  p<-p+scale_fill_gradientn('Stand.\nres.',colours=c("#B2182B","#D6604D","#F4A582","#92C5DE","#4393C3","#2166AC"), values=rescale(c(min(grants$res), -3, -2,2,3,max(grants$res))),guide=guide_colorbar(nbin=4, raster=F, ticks=F, title.hjust =0,label.position="left",label.hjust =1))

  p
}
