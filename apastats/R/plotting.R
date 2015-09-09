#' Title
#'
#' @param x
#' @param addSegment
#'
#' @return scale_x_continuous with pretty breaks
#' @export
#'
#'
base.breaks.x <- function(x, addSegment=T){
  b <- pretty(x)

  b[1]<-min(x)
  b[length(b)]<-max(x)
  if (b[length(b)]-b[length(b)-1]<b[length(b)-1]-b[length(b)-2]){
    b<-b[c(1:(length(b)-2),length(b))]
  }

  d <- data.frame(y=-Inf, yend=-Inf, x=min(b), xend=max(b))
  if (addSegment)
  {list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=FALSE),
        scale_x_continuous(breaks=b))}
  else scale_x_continuous(breaks=b)
}

#' Title
#'
#' @param x
#' @param expand
#' @param ...
#'
#' @return scale_y_continuous with pretty breaks
#' @export
#'
#'
base.breaks.y <- function(x, expand = c(0,0),...){
  b <- pretty(x)
  #print(b)
  b[1]<-min(x)
  b[length(b)]<-max(x)

  d <- data.frame(x=-Inf, xend=-Inf, y=min(b), yend=max(b),...)
  list(geom_segment(data=d, aes(x=x, y=y, xend=xend, yend=yend), inherit.aes=F),
       scale_y_continuous(breaks=b, expand = expand))
}

#' Title
#'
#' @param ...
#' @param pos
#' @param pointsize
#' @param linesize
#' @param pointfill
#' @param within_subj
#' @param wid
#' @param bars
#' @param withinvars
#' @param betweenvars
#' @param x_as_numeric
#' @param add_h_line
#' @param connecting_line
#'
#' @return plot of pointrange
#' @export plot.pointrange
#' @method generic class
#'

plot.pointrange <- function (..., pos=position_dodge(0.3), pointsize=I(3), linesize=I(1), pointfill=I('white'), within_subj=F, wid='uid', bars='ci', withinvars=NULL, betweenvars=NULL, x_as_numeric=F, add_h_line=NULL, connecting_line=F, pretty_breaks_y=F, pretty_y_axis=F, exp_y=F, print_aggregated_data=F){
  ellipses<-list(...)
  plot_f<-ellipses[[2]]
  withinvars<-c(withinvars, as.character(unlist(plot_f[names(plot_f)!='y'])))
  dv<-as.character(plot_f['y'][[1]])
  plot_data<-as.data.frame(ellipses[[1]])
  plot_data<-plot_data[!is.na(plot_data[,dv]),]
  aes_list<-modifyList(lapply(plot_f[names(plot_f)!='y'],as.character),list(y=dv, ymin='ymin', ymax='ymax'))
  #print(aes_list)
  if (within_subj){
    aggr_data<-Rmisc::summarySEwithin(plot_data, measurevar=dv, withinvars = withinvars, betweenvars=betweenvars, idvar=wid, na.rm=T)
  } else {
    aggr_data<-Rmisc::summarySE(plot_data, measurevar=dv, groupvars = c(withinvars,betweenvars), na.rm=T)
  }
  if (x_as_numeric){
    aggr_data[, aes_list$x]<-as.numeric(as.character(aggr_data[, aes_list$x]))
  }
  aggr_data$ymin<-aggr_data[,dv]-aggr_data[,bars]
  aggr_data$ymax<-aggr_data[,dv]+aggr_data[,bars]
  if (exp_y){
    aggr_data$ymin<-exp(aggr_data$ymin)
    aggr_data$ymax<-exp(aggr_data$ymax)
    aggr_data[,dv]<-exp(aggr_data[,dv])
  }
  if (print_aggregated_data){
    print(aggr_data)
  }
  p<-ggplot(aggr_data, do.call(aes_string,aes_list))

  if (connecting_line)
    p<-p+geom_line(position=pos, size=linesize)
  p<-p+geom_linerange(size=linesize, position=pos)+geom_point(size=pointsize, position=pos, fill=pointfill)

  if (!is.null(add_h_line)){
    p<-p+add_h_line
  }

  if (pretty_breaks_y){
    y_range<-ggplot_build(p)$panel$ranges[[1]]$y.range
    breaks<-labeling::extended(y_range[1],y_range[2],5)
    limits<-range(c(breaks,y_range))
    p<-p+scale_y_continuous(breaks=breaks)+coord_cartesian(ylim=limits)
    if (pretty_y_axis){
      d <- data.frame(x=-Inf, xend=-Inf, y=min(breaks), yend=max(breaks))

      p<-p+geom_segment(data=data.frame(x=-Inf,xend=-Inf, y=min(breaks), yend=max(breaks)),aes(x=x,xend=xend, y=y, yend=yend),inherit.aes=F)
    }
  }
  p
}

#' Transform exponential scale back to normal
#'
#' @param digits
#'
#' @return scale
#' @export
#'
scale_y_exp<-function(digits=0){
  requireNamespace('scales')
  scale_y_continuous(breaks=trans_breaks('exp',function (x) log(x)), labels=trans_format('exp', function(x) as.character(f.round(x, digits=digits))))
}

#' Title
#' Not mine, found somewhere
#' @param myggplot
#' @param el
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

#' Arrange several plots with one legend and/or one x-axis title
#'
#' Based on a function from https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
#'
#' @param ... - two or more ggplot2 plots
#' @param stack - stack direction, "h" (horizontal) or "v" (vertical)
#' @param one_sub - one x-axis label for all plots? T/F
#' @param heights - a vector of heights for verticaly arrange plots
#' @param one_x_axis - one x-axis for all plots? T/F
#' @param legend position - "t" (top),"b" (bottom), "l" (left), or "r" (right)
#'
#' @return plots arranged with grid.arrange
#' @export
#'
#'
#'


grid_arrange_shared_legend<-function(..., stack = 'v', one_sub=F, heights=F, one_x_axis=F, legend_pos='b') {
  requireNamespace('gridExtra')
  plots <- list(...)
  legend <- get_grob_element(plots[[1]])
  plots<-lapply(plots, function(x) x + theme(legend.position="none"))
  if (one_sub) {
    x_lab_grob<-get_grob_element(plots[[1]], 'xlab')
    plots<-lapply(plots, function(x) x + xlab(NULL)+theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()))
  }
  if (one_x_axis){
    x_axis_grob<-get_grob_element(plots[[1]], 'axis-b')
    plots<-lapply(plots, function(x) x + theme(axis.text.x=element_blank(), axis.ticks.x=element_blank()))
  }
  if (!heights){
    heights=unit(rep_len(1, length(plots)), "null")
  }
  lheight <- sum(legend$heights)
  lwidth <- sum(legend$widths)

  if (stack=='v'){
    p<-do.call(gridExtra::arrangeGrob, append(plots, list(heights=heights)))
    if (one_x_axis){

      p<-gridExtra::arrangeGrob(p, x_axis_grob, ncol = 1, heights = unit.c(unit(1, "npc") - x_axis_grob$height, x_axis_grob$height))
    }
  } else {
    p<-do.call(gridExtra::arrangeGrob, c(plots, nrow=1))
  }

  if (legend_pos=='t')
    p<-gridExtra::arrangeGrob( legend, p, ncol = 1, heights = unit.c(lheight, unit(1, "npc") - lheight))
  else if (legend_pos=='b')
    p<-gridExtra::arrangeGrob(p, legend, ncol = 1, heights = unit.c(unit(1, "npc") - lheight, lheight))
  else if (legend_pos=='r')
    p<-gridExtra::arrangeGrob(p, legend, nrow = 1, widths = unit.c(unit(1, "npc") - lwidth, lwidth))
  else if (legend_pos=='l')
    p<-gridExtra::arrangeGrob(legend, p, nrow = 1, widths = unit.c(lwidth, unit(1, "npc") - lwidth))

  if (one_sub) {
    xlab_h<- unit(1.2,'lines') #sum(xlab_grob$heights)
    p<-gridExtra::arrangeGrob(p, x_lab_grob,  ncol = 1, heights = unit.c(unit(1, "npc") - xlab_h, xlab_h))
  }
  grid.arrange(p)
}

#' Internal function for palette of shapes
#'
#' @return palette of shapes
#' @export
#'
#'
ac_shape_pal<-function(){
  manual_pal(unname(c(21,22,24,25,23,1:5)))

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


  #рассчитываем высоту столбцов как кумулятивную пропорцию внутри каждого уровня field
  grants<-ddply(grants, .(field), transform, ymax=cumsum(n)/sum(n), ymin=(cumsum(n)-n)/sum(n), ymid=(cumsum(n)-n/2)/sum(n))

  #рассчитываем ширину столбцов как суммарное n для каждого уровня field
  grants<-ddply(grants, .(field), transform, xmax=sum(n))

  #добавляем в фрейм данные об остатках
  grants<-merge(grants, as.data.frame(chi$stdres, responseName='res'), by=c("field","npersons_bin") )

  #добавляем человекопонятные метки
  grants$field<-factor(grants$field, levels=c("soc","beh_cog","chem","physics"), labels=c("Обществ.","Бихевиор./когн.","Химия","Физика"))

  #задаем базовую структуру графика
  p<-ggplot(data=grants,aes(xmin=0, ymin=ymin, xmax=xmax, ymax=ymax, fill=res)) +
    #квадраты графика
    geom_rect(color=I("white"), show_guide=F, size=I(3)) +
    #подписи с частотами
    geom_text(aes(label=n, x=25, y=ymin+0.05), color=I("white"), size=3) +
    #используем facet для разделения на столбцы
    facet_grid(~field,scales="free_x",space="free_x")

  #применяем тему, убираем лишнее
  p<-p+ mytheme+theme(panel.margin = unit(0, "lines"),panel.grid=element_blank(),strip.background=element_blank(), line = element_blank(),
                      axis.text.x = element_blank(),  axis.line = element_blank(), axis.title = element_blank())+scale_x_continuous(expand=c(0,1))

  #добавляем метки для числа человек слева
  p<-p+scale_y_continuous(expand=c(0,0), breaks=grants[grants$field=="Обществ.","ymid"], labels=c("1 человек", "2-3 человека", "3 и более"))

  #раскрашиваем в зависимости от остатков
  p<-p+scale_fill_gradientn('Стандарт.\nостатки.',colours=c("#B2182B","#D6604D","#F4A582","#92C5DE","#4393C3","#2166AC"), values=rescale(c(min(grants$res), -3, -2,2,3,max(grants$res))),guide=guide_colorbar(nbin=4, raster=F, ticks=F, title.hjust =0,label.position="left",label.hjust =1))

  p
}
