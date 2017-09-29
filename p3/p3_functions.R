#
fivequantile = function(x,col) {
  xxx <- quantile(x[,col], probs=c(0.2,0.4,0.6,0.8)) # same as probs=c(0,0.2,0.4,0.6,0.8,1))
  list(x[x[,col]<=xxx[1]],
       x[x[,col]>xxx[1] & x[,col]<=xxx[2]],
       x[x[,col]>xxx[2] & x[,col]<=xxx[3]],
       x[x[,col]>xxx[3] & x[,col]<=xxx[4]],
       x[x[,col]>xxx[4]])
}
