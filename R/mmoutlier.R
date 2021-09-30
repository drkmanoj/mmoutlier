#'Identifying outliers from univariate data and replace the outliers by max-min Method'
#'@export
#'@param x numeric variable
mmoutlier<-function(x)
{
  par(mfrow=c(1,2))
xsort<-sort(x)
y<-boxplot(x)$out
xy<-x[which(x %in% y)]
xy

xysort<-sort(xy)

l<-length(xysort)
#len<-1
for(k in seq_along(xysort))
{
for(i in seq_along(x))
{

  if(xysort[k]==x[i])
  {

  x[i]<-(xsort[length(x)-length(xysort)-l+1])-x[(l)]
 # len<-len-1
  l<-l-1
  }
}
}
print(x)
z<-boxplot(x)
}


