
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

plot.pointrange <- function (..., pos=position_dodge(0.3), pointsize=I(3), linesize=I(1), pointfill=I('white'), within_subj=F, wid='uid', bars='ci', withinvars=NULL){
  if (within_subj){
    ellipses<-list(...)
    plot_f<-ellipses[[2]]
    withinvars<-c(withinvars, as.character(unlist(plot_f[names(plot_f)!='y'])))
    dv<-as.character(plot_f['y'][[1]])
    plot_data<-as.data.frame(ellipses[[1]])
    plot_data<-plot_data[!is.na(plot_data[,dv]),]
    aggr_data<-summarySEwithin(plot_data, measurevar=dv, withinvars = withinvars, idvar=wid, na.rm=T)
    aes_list<-modifyList(lapply(plot_f[names(plot_f)!='y'],as.character),list(y=dv, ymin=paste0(dv,'-', bars), ymax=paste0(dv,'+', bars )))
    ggplot(aggr_data, do.call(aes_string,aes_list))+ geom_linerange(size=linesize, position=pos)+geom_point(size=pointsize, position=pos, fill=pointfill)
  } else {
    ggplot(...)+ geom_linerange(stat='summary', size=linesize, fun.data=mean_cl_boot, position=pos)+geom_point(stat='summary', fun.y=mean, size=pointsize, position=pos, fill=pointfill)
  }
}
