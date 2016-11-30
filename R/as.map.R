#'Convert a map from a 3 columns data.frame to a map object
#'
#'Convert a map from a 3 columns data.frame to a map object
#'
#'@param x a 3 columns data.frame with marker names, chromosome, and position
#'@param order whether loci should be ordered by position in each chromosome. If FALSE, the order of original file is kept
#'@author J.-F. Rami \email{rami@@cirad.fr}
#'@export

as.map<-function(x,order=T){
  map<-x
  mnames <- map[,1]
  uchr <- unique(map[,2])
  n.chr <- length(uchr)
  geno <- vector("list",n.chr)
  names(geno) <- uchr
  min.mar <- 1
  for(i in 1:n.chr) {
    temp.map <- round(map[map[,2]==uchr[i],3],1)
    names(temp.map) <- mnames[map[,2]==uchr[i]]
    if (order){
      geno[[i]] <- temp.map[order(temp.map)]
    }else{
      geno[[i]] <- temp.map
    }
  }
  geno
}
