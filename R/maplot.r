######################################################################
#
# maplot : alternative to plot.map including mk names and position
#          labelling
#
######################################################################

"maplot" <-
function(map,map2,locusname=TRUE,position=TRUE,interchr=0,ymax=NA,cex=0.6,decalcoef=0.02,out="screen",ech=1,newdev=TRUE,maih=1,xlim=1,first=0,filename=NULL,sw=rep(1,length(map)),locolors=NULL,boxw=500,main=NULL,...)
{
  #map <- x
  # figure out if the input is a cross (containing a map)
  #    or is the map itself
  if(!is.na(match("geno",names(map))))
    map <- pull.map(map)

if (length(locusname)==1){
locusname<-rep(locusname,length(map))
}
if (length(position)==1){
position<-rep(position,length(map))
}
if (is.null(locolors)){
locolors<-rep("black",sum(sapply(map,length)))
}
  sex.sp <- FALSE


    # determine whether a second map was given
    if(!missing(map2)) {
      if(is.logical(map2)) { # assume "map2" should be "horizontal"
        horizontal <- map2
        map2 <- NULL
        one.map <- TRUE
      }
      else { # determine if it is a cross object
        if(!is.na(match("geno",names(map2))))
          map2 <- pull.map(map2)
        one.map <- FALSE
      }
    }
    else one.map <- TRUE

    if (!out=="screen" & is.null(filename)) filename=paste("maplot",format(Sys.time(), "%y%m%d%H%M%S"),".",out,sep="")
    if (out=="wmf") {
    #win.metafile(filename = paste(file.choose(T),".wmf",sep=""),width=1000, height=900*ech)
    win.metafile(filename = filename)
    }
    if (out=="jpeg") {
    #jpeg(file=paste(file.choose(T),".jpeg",sep=""), bg="white",quality=100,width=1000, height=900)
    jpeg(file=filename, bg="white",quality=100,width=1000, height=900)
    }
    if (out=="png") {
    #png(file=paste(file.choose(T),".png",sep=""), bg="transparent",width=1000, height=900*ech)
    png(file=filename, bg="transparent",pointsize=20,width=1000, height=900*ech)
    }
    #if (out=="screen") if (newdev==TRUE) {
   # 	if (.Platform$OS.type=='unix') {
#		quartz(height=7*ech, pointsize=14)}
#		else {
#		windows(record=TRUE,height=7*ech)
#		}
#		}
		
  if(one.map) {
    n.chr <- length(map)
    boxw<-xlim*n.chr/boxw
    ticks.leng<-boxw+(xlim*n.chr/150)
    map <- lapply(map, function(a) a-min(a))
    if (is.na(ymax)) maxlen <- max(unlist(lapply(map,max))) else maxlen<-ymax

      old.xpd <- par("xpd")
      old.xaxt <- par("xaxt")
      old.las <- par("las")
      old.mai <- par("mai")
      par(xpd=NA,xaxt="n",las=1,mai=c(0,0,maih,0),xaxt="n",yaxt="n",bty="n")
      #on.exit(par(xpd=old.xpd,xaxt=old.xaxt,las=old.las,mai=old.mai))

      plot(0,0,type="n",ylim=c(maxlen,0),xlim=c(0.95,xlim*(n.chr+interchr+0.5)),xlab="",ylab="", main=main)

      a <- par("usr")
      curloc<-0
      for(i in 1:n.chr) {
              x<-i+first
        #lines(c(x,x), c(min(map[[i]]),max(map[[i]])))
        rect(x-boxw,max(map[[i]]),x+boxw,min(map[[i]]))
        # add chromosome label
        text(x,0,as.character(names(map)[i]),pos=3,offset=0.5)
        nmar <- length(map[[i]])
        decal<-0
        for(j in 1:nmar){
        		curloc<-curloc+1
		if (j>1) {
			if ((map[[i]][j]-map[[i]][j-1]-decal)<2*cex*maxlen*decalcoef/ech) {
				decal <- decal + ((2*cex*maxlen*decalcoef)/ech - (map[[i]][j]-map[[i]][j-1])) }
			else if (decal>0) decal<-0
		}
			
			
            lines(c(x-boxw,x+boxw),c(map[[i]][j],map[[i]][j]),col=locolors[[curloc]])
            #add locus name label
            if (locusname[[i]]){
            # add mark line
            lines(c(x+(sw[[i]]*ticks.leng),x+sw[[i]]*boxw),c(map[[i]][j]+decal,map[[i]][j]),col=locolors[[curloc]])
            #browser()
            text(x+(sw[[i]]*ticks.leng),map[[i]][j]+decal,names(map[[i]][j]),cex=cex,pos=ifelse(sw[[i]]==1,4,2),col=locolors[[curloc]])
            }
            #add position label
            if (position[[i]]){
            # add mark line
            lines(c(x-(sw[[i]]*ticks.leng),x-sw[[i]]*boxw),c(map[[i]][j]+decal,map[[i]][j]))
            text(x-(sw[[i]]*ticks.leng),map[[i]][j]+decal,round(map[[i]][j],digits=1),cex=cex,pos=ifelse(sw[[i]]==1,2,4))
            }
        }
      }

    #title(main="Genetic map")
  }
  else {
	print("This version of maplot does not yet implement map comparison")
  }
  invisible()
if (!out=="screen" & newdev==T) graphics.off()
}
