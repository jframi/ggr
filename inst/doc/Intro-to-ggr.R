## ------------------------------------------------------------------------
library(ggr)
library(knitr)

## ---- fig.show='hold'----------------------------------------------------
mymap<-read.table(system.file("extdata","Nguepjop.et.al.2016.map.txt",package="ggr"))
myg<-read.table(system.file("extdata","Nguepjop.et.al.2016.txt",package="ggr"))

## ------------------------------------------------------------------------
kable(head(mymap))
kable(head(myg))

## ------------------------------------------------------------------------
mymap<-as.map(mymap)
mymap

## ------------------------------------------------------------------------
codes<-unique(c(as.matrix(myg[,-1])))
mycolors<-c("green","yellow","red","white")
names(mycolors)<-codes

## ---- fig.width=10, fig.height=6-----------------------------------------
gg(x = myg,map = mymap,col =mycolors,lmarg = 0.06 ,inter=0.1,sw=-1,first = 0.05,position=F)


