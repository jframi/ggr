


#' The main function for graphical genotyping
#'
#' @param x a data.frame with genotypic data
#' @param map a map object as returned by as.map
#' @param bychrom boolean. Whether one plot per chromosome should be done (TRUE), or a single one for the whole genome (FALSE)
#' @param bwchrom space between chromosome when bychrom is FALSE
#' @param indiv vector of integer corresponding to individuals (columns) to plot
#' @param chrom vector of integer corresponding to chromosomes to plot
#' @param bw boolean. Balck and white (not used at the moment)
#' @param ech vertical scale
#' @param w width of plotting area. not used if autowidth=T
#' @param h height of plotting area
#' @param filepointsize not used at the moment
#' @param inter space between individuals
#' @param xlim see maplot doc
#' @param lmarg mrgin betwenn chromosome and first individual
#' @param autowidth boolean. compute w from number of individual
#' @param palett ,ot used
#' @param col vector of named colors. Names need to be genotyping codes
#' @param density parameter passed to the rect function for drawing genotyping blacks
#' @param angle parameter passed to the rect function for drawing genotyping blacks
#' @param indiv.name boolean. should individual names be plotted (deduced from c colnames)
#' @param ... additional parameters to be passed to maplot
#'
#' @return
#' @export
#'
#' @examples
gg <- function(x,map,bychrom=T,bwchrom=20,indiv=NA,chrom=NA,bw=F,ech=1,w=1000,h=900,filepointsize=16,inter=0.5,xlim=1,lmarg=0.2,autowidth=F,palett,col, density=NULL,angle=45,indiv.name=F,...) {


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
    pluslongchrom<-max(unlist(lapply(map[chromlist],max)))-min(map[chromlist][[which.max(unlist(lapply(map[chromlist],max)))]])
    indivcoords<-rep(NA,length(indiv))
    names(indivcoords)<-indiv
    codes<-unique(c(as.matrix(x[,-1])))

    if (bychrom){

      for (chrom in chromlist) {

        # aploter is the subset of x for the current chromosome
        chrmap<-map[chrom]
        chrmap[[1]]<-chrmap[[1]]-min(chrmap[[1]])
        nmk<-length(chrmap[[1]])

        aploter<-x[x[,1]%in%names(chrmap[[1]]),-1]
        maplot(chrmap,interchr=0.5,newdev=FALSE,ech=1,xlim=xlim,ymax=pluslongchrom,...)
        rectx1<-1+sort(rep(seq(from=lmarg,to=1-largeurt,length.out = nbindiv),nmk))
        rectx2<-rectx1+largeur
        recty1<-c(0,head(chrmap[[1]],-1)+(chrmap[[1]][-1]-head(chrmap[[1]],-1))/2)
        recty2<-c(head(chrmap[[1]],-1)+(chrmap[[1]][-1]-head(chrmap[[1]],-1))/2,tail(chrmap[[1]],1))
        #cols<-c(apply(aploter,2,function(a) match(a,codes)))
        cols<-c(apply(aploter,2,function(a) match(a,names(col))))
        rect(xleft = rectx1,xright = rectx2,ytop = recty1,ybottom = recty2, lwd=0.5,border=NA ,col = col[cols], density=density, angle=angle, fg="black")
        if (indiv.name){
          text(x = 1+seq(from=lmarg,to=1-largeurt,length.out = nbindiv),y=-max(chrmap[[1]])/100,labels = indiv,srt=90,pos=4, cex=0.5)
        }

      }
    } else{
      map<-map[chromlist]
      chromlist<-1:length(chromlist)
      addtochrs<-c(0,head(cumsum(unlist(lapply(map,max))+bwchrom),-1))
      mapcumu<-list(" "=unlist(sapply(1:length(map), function(a) map[[a]]+addtochrs[a])))
      maplot(mapcumu,interchr=0.5,newdev=FALSE,ech=1,xlim=xlim,...)
      for (chrom in chromlist) {
        # aploter is the subset of x for the current chromosome
        chrmap<-map[chrom]
        nmk<-length(chrmap[[1]])
        chrmap[[1]]<-chrmap[[1]]+addtochrs[chrom]
        aploter<-x[x[,1]%in%names(chrmap[[1]]),-1]
        rectx1<-1+sort(rep(seq(from=lmarg,to=1-largeurt,length.out = nbindiv),nmk))
        rectx2<-rectx1+largeur
        recty1<-c(min(chrmap[[1]]),head(chrmap[[1]],-1)+(chrmap[[1]][-1]-head(chrmap[[1]],-1))/2)
        recty2<-c(head(chrmap[[1]],-1)+(chrmap[[1]][-1]-head(chrmap[[1]],-1))/2,tail(chrmap[[1]],1))
        #cols<-c(apply(aploter,2,function(a) match(a,codes)))
        cols<-c(apply(aploter,2,function(a) match(a,names(col))))
        rect(xleft = rectx1,xright = rectx2,ytop = recty1,ybottom = recty2, lwd=0.5,border=NA ,col = col[cols], density=density, angle=angle, fg="black")
      }
      if (indiv.name){
        text(x = 1+seq(from=lmarg,to=1-largeurt,length.out = nbindiv),y=-max(mapcumu[[1]])/100,labels = colnames(x[,-1])[indiv],srt=90,pos=4, cex=0.5)
      }
      rect(xleft = 1, xright = max(rectx2),ytop = unlist(lapply(map,max))+addtochrs, ybottom= unlist(lapply(map,max))+addtochrs+bwchrom, col="white",border=NA  )
  }
  }

