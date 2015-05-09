
base.breaks.x <- function(x, addSegment=T){
  b <- pretty(x)
  b[1]<-min(x)
  b[length(b)]<-max(x)
  d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
  if (addSegment) 
  {list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
        scale_x_continuous(breaks=b))}
  else scale_x_continuous(breaks=b)
}

base.breaks.y <- function(x, expand = c(0,0),...){
  b <- pretty(x)
  #print(b)
  b[1]<-min(x)
  b[length(b)]<-max(x)
  
  d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b),...)
  list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=F),
       scale_y_continuous(breaks=b, expand = expand))
}

plot.pointrange <- function (..., pos=position_dodge(0.3), pointsize=I(3), linesize=I(1), pointfill=I('white'), within_subj=F, wid='uid', bars='ci', withinvars=NULL, betweenvars=NULL){
  if (within_subj){
    ellipses<-list(...)
    plot_f<-ellipses[[2]]
    withinvars<-c(withinvars, as.character(unlist(plot_f[names(plot_f)!='y'])))
    dv<-as.character(plot_f['y'][[1]])
    plot_data<-as.data.frame(ellipses[[1]])
    plot_data<-plot_data[!is.na(plot_data[,dv]),]
    aggr_data<-summarySEwithin(plot_data, measurevar=dv, withinvars = withinvars, betweenvars=betweenvars, idvar=wid, na.rm=T)
    aes_list<-modifyList(lapply(plot_f[names(plot_f)!='y'],as.character),list(y=dv, ymin=paste0(dv,'-', bars), ymax=paste0(dv,'+', bars )))
    ggplot(aggr_data, do.call(aes_string,aes_list))+ geom_linerange(size=linesize, position=pos)+geom_point(size=pointsize, position=pos, fill=pointfill)
  } else {
    ggplot(...)+ geom_linerange(stat='summary', size=linesize, fun.data=mean_cl_boot, position=pos)+geom_point(stat='summary', fun.y=mean, size=pointsize, position=pos, fill=pointfill)
  }
}

scale_y_exp<-function(digits=0){
  require(ggplot2)
  require(scales) 
  scale_y_continuous(breaks=trans_breaks('exp',function (x) log(x)), labels=trans_format('exp', function(x) as.character(f.round(x, digits=digits))))
}

#Not mine, found somewhere
get_grob_element<-function(myggplot, el='guide-box'){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  
  leg <- which(tmp$layout$name==el)
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#from https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
grid_arrange_shared_legend <- function(..., stack = 'v', one_sub=F) {
  require(ggplot2)
  require(gridExtra)
  plots <- list(...)
  legend <- get_grob_element(plots[[1]])
  plots<-lapply(plots, function(x) x + theme(legend.position="none"))
  if (one_sub) {
    xlab_grob<-get_grob_element(plots[[1]], 'xlab')
    plots<-lapply(plots, function(x) x + xlab(NULL))
  }
  
  if (stack=='v'){
    lheight <- sum(legend$heights)
    p<-arrangeGrob(
      do.call(arrangeGrob, plots),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight))
  } else {
    lwidth <- sum(legend$widths)
    p<-arrangeGrob(
      do.call(arrangeGrob, c(plots, nrow=1)),
      legend,
      nrow = 1,
      widths = unit.c(unit(1, "npc") - lwidth, lwidth))
  }
  if (one_sub) {
    xlab_h<- unit(1.2,'lines') #sum(xlab_grob$heights)
    p<-arrangeGrob(p, xlab_grob, ncol = 1, heights = unit.c(unit(1, "npc") - xlab_h, xlab_h))
  }
  p
}
