#' make it easy to use ggplot2 for plot!
#'
#' @param data your data,data's fist column should be different type,and the second is value
#' @param plot choose a plot type between barplot and boxplot
#' @param xlab the label for x axis
#' @param ylab the label for y axis
#' @param rank rank the data
#' @param color fill with the color you like
#' @param angle the angle of xlab
#' @param savename save the plot to a file end with .jpg .png or .pdf
#' @param width image's size
#' @param height image's size
#' @param hjust position of axis.text.text
#' @param vjust position of axis.text.text
#' @param alpha level of significance test
#' @param method it can be 'more' when there are more than three replicates per experimental treatment;otherwise,it should be 'less'
#' @return p
#' @author Sidong-Li 2875620735@qq.com
#' @export easyplot
#' @import RColorBrewer
#' @import ggplot2
#' @examples easyplot(data=mydata,plot="barplot")
easyplot <- function(data,plot="barplot",xlab="type",ylab="mean",rank="none",
                     color=yanse,angle=58,hjust=0.99,vjust=0.9,savename="plot.jpg",
                     width=8,height=8,alpha=0.05,method="more"){
  theme_ld <- theme(legend.position = "none",
        axis.title = element_text(size=30,face = 'bold',color="black"),
        axis.text.y = element_text(size=15,color="black"),
        axis.text.x = element_text(size=25,angle = angle,hjust = hjust,vjust = vjust,color="balck"),
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.ticks.length.y = unit(0.2,"cm"))

  options(warn = -1)
  q <- data
  low <- rank=="none"
  if (low[1]) {
    if (plot=="barplot") {
      qw<- lsdd(q,alpha=alpha,method=method)
      q <- qw$data
      yanse <- RColorBrewer::brewer.pal(n = length(x = q$type),name = "Set3")
      p <- ggplot(q)+
        geom_bar(aes(x = type,y=mean,fill=type),
                 stat="identity",position=position_identity(),width=0.6)+
        geom_errorbar(aes(x = type,ymax=mean+sd,ymin=mean-sd),
                      position = position_identity(),width=0.2)+
        scale_y_continuous(limits = c(0,max(q$mean)*1.7),expand = c(0,0))+
        labs(y=ylab,x=xlab)+
        scale_fill_manual(values = color)+
        geom_text(mapping = aes(x = type,y =mean+sd+max(q$mean)*0.07,label=groups),size=10)+
        theme_bw()+
        theme_ld
      print(p)
      ggsave(filename = savename,plot = p,width = width,height = height)
      message(paste("The plot has been saved as",savename))
    }else if (plot=="boxplot"){
      qw<- lsdd(q,alpha=alpha,method=method)$data
      q<- lsdd(q,alpha=alpha,method=method)$process
      yanse <- RColorBrewer::brewer.pal(n = length(x = qw$type),name = "Set3")
      p <- ggplot(q)+
        geom_boxplot(aes(x = type,y=value,fill=type),position=position_identity(),width=0.6)+
        geom_jitter(mapping = aes(x=type,y = value,color=type),position =position_jitter(0.4),alpha=0.7,size=1.8)+
        scale_y_continuous(limits = c(0,max(q$value)*1.7),expand = c(0,0))+
        labs(y=ylab,x=xlab)+
        scale_fill_manual(values = color)+
        scale_color_manual(values = color)+
        geom_text(data=qw,mapping = aes(x = type,y =mean+sd+max(qw$mean)*0.2,label=groups),size=10)+
        theme_bw()+
        theme_ld
      print(p)
      ggsave(filename = savename,plot = p,width = width,height = height)
      message(paste("The plot has been saved as",savename))
    }else{
      message("plot must be 'barplot' or 'boxplot' in this version")
    }
    return(p=p)
  }
  else {
    if (plot=="barplot") {
      qw<- lsdd(q,alpha=alpha,method=method)
      q <- qw$data
      q$type <- factor(q$type,levels = rank)
      yanse <- RColorBrewer::brewer.pal(n = length(x = q$type),name = "Set3")
      p <- ggplot(q)+
        geom_bar(aes(x = type,y=mean,fill=type),
                 stat="identity",position=position_identity(),width=0.6)+
        geom_errorbar(aes(x = type,ymax=mean+sd,ymin=mean-sd),
                      position = position_identity(),width=0.2)+
        scale_y_continuous(limits = c(0,max(q$mean)*1.7),expand = c(0,0))+
        labs(y=ylab,x=xlab)+
        scale_fill_manual(values = color)+
        geom_text(mapping = aes(x = type,y =mean+sd+max(q$mean)*0.07,label=groups),size=10)+
        theme_bw()+
       theme_ld
      print(p)
      ggsave(filename = savename,plot = p,width = width,height = height)
      message(paste("The plot has been saved as",savename))
    }else if (plot=="boxplot"){
      qw<- lsdd(q,alpha=alpha,method=method)$data
      q<- lsdd(q,alpha=alpha,method=method)$process
      qw$type <- factor(qw$type,levels = rank)
      q$type <- factor(q$type,levels = rank)
      yanse <- RColorBrewer::brewer.pal(n = length(x = qw$type),name = "Set3")
      p <- ggplot(q)+
        geom_boxplot(aes(x = type,y=value,fill=type),position=position_identity(),width=0.6)+
        geom_jitter(mapping = aes(x=type,y = value,color=type),position =position_jitter(0.4),alpha=0.7,size=1.8)+
        scale_y_continuous(limits = c(0,max(q$value)*1.7),expand = c(0,0))+
        labs(y=ylab,x=xlab)+
        scale_fill_manual(values = color)+
        scale_color_manual(values = color)+
        geom_text(data=qw,mapping = aes(x = type,y =mean+sd+max(qw$mean)*0.2,label=groups),size=10)+
        theme_bw()+
       theme_ld
      print(p)
      ggsave(filename = savename,plot = p,width = width,height = height)
      message(paste("The plot has been saved as",savename))
    }else{
      message("plot must be 'barplot' or 'boxplot' in this version")
    }
    return(p=p)
  }
}
.onLoad <- function(libname,pkgname){
  message("Thank you for using this package!")
}
