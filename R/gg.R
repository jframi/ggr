
#' Title
#'
#' @param x
#' @param map
#' @param indiv
#' @param chrom
#' @param bw
#' @param ech
#' @param w
#' @param h
#' @param filepointsize
#' @param inter
#' @param xlim
#' @param lmarg
#' @param autowidth
#' @param palett not used at the moment
#' @param col
#' @param density
#' @param angle
#' @param ...
#'
#' @return
#' @export
#'
#' @examples

gg <- function(x,map,indiv=NA,chrom=NA,bw=F,ech=1,w=1000,h=900,filepointsize=16,inter=0.5,xlim=1,lmarg=0.2,autowidth=F,palett,col=c("red","green","yellow","blue"),density=NULL,angle=45,...) {


    # initialize chromlist if no user input
    if (is.na(chrom)) {
      chromlist<-1:length(names(map))
    }
    else chromlist<-chrom

    # initialize indiv if no user input
    if (is.na(indiv)) {
      indiv<-colnames(x[,-1])
    }
    nbindiv<-length(indiv)

    # calculate the width of each box
    largeurt<-(1-lmarg)/nbindiv
    # this is for the separation between each box
    largeur<-largeurt/(1+inter)

    #... but don't allow boxes wider than 0.04
    if (largeur>0.04) largeur<-0.04
    # the initial x position for each trait
    xinit<-seq(1+(100/w),1+(100/w)+largeurt*(nbindiv-1),by=largeurt)


    if (autowidth==T) {
      w<-(nbindiv)*1000/40
    }


    # construction of the color vector
    # if (bw) {
    #   palett<-rev(grey(seq(0,1,length=256))[1:256])}
    # else {
    #   palett<-rev(rainbow(256,start = 0, end = 2/3))}

    # what is the size of the longer chromosome of the map
    pluslongchrom<-max(unlist(lapply(map,max)))
    indivcoords<-rep(NA,length(indiv))
    names(indivcoords)<-indiv
    codes<-unique(do.call(c,apply(x[,-1],2,function(a) levels(as.factor(a)))))

    for (chrom in chromlist) {

      # aploter is the subset of x for the current chromosome
      chrmap<-map[chrom]
      nmk<-length(chrmap[[1]])

      aploter<-x[x[,1]%in%names(chrmap[[1]]),-1]
      maplot(chrmap,locusname=TRUE,interchr=0.5,newdev=FALSE,ech=1,xlim=xlim,ymax=pluslongchrom,...)
      rectx1<-1+sort(rep(seq(from=lmarg,to=1-largeurt,length.out = nbindiv),nmk))
      rectx2<-rectx1+largeur
      recty1<-c(0,head(chrmap[[1]],-1)+(chrmap[[1]][-1]-head(chrmap[[1]],-1))/2)
      recty2<-c(head(chrmap[[1]],-1)+(chrmap[[1]][-1]-head(chrmap[[1]],-1))/2,tail(chrmap[[1]],1))
      #cols<-c(apply(aploter,2,function(a) match(a,codes)))
      cols<-c(apply(aploter,2,function(a) match(a,names(col))))
      rect(xleft = rectx1,xright = rectx2,ytop = recty1,ybottom = recty2, lwd=0.5,border=NA ,col = col[cols], density=density, angle=angle, fg="black")
    }

  }

