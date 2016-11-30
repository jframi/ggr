## ------------------------------------------------------------------------
library(ggr)
library(knitr)

## ------------------------------------------------------------------------
mymap<-read.table(system.file("extdata","Nguepjop.et.al.2016.map.txt",package="ggr"))
myg<-read.table(system.file("extdata","Nguepjop.et.al.2016.txt",package="ggr"))

## ---- echo=FALSE---------------------------------------------------------
kable(head(mymap))
kable(head(myg))

## ------------------------------------------------------------------------
mymap<-as.map(mymap)
mymap

## ------------------------------------------------------------------------
codes<-unique(c(as.matrix(myg[,-1])))
codes

## ------------------------------------------------------------------------
mycolors<-c("green","yellow","red","white")
names(mycolors)<-codes
mycolors

## ---- fig.width=10, fig.height=6-----------------------------------------
gg(x = myg,map = mymap,col =mycolors,lmarg = 0.06 ,inter=0.1,first = 0.05,position=F,decalcoef = 0.005, sw=-1)


