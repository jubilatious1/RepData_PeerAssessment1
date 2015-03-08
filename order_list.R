#begin example

iterations<-999

d<-list() #resampled data

f<-list() #fitted values

r<-list() #residuals

l<-list()

for (i in 1:iterations){

      iboot<-sample(1:nrow(cars),replace=TRUE)

      bootdata<-cars[iboot,]

      d[[i]]<-bootdata

      f[[i]]<-fitted(lm(bootdata$dist~bootdata$speed))

      r[[i]]<-resid(lm(bootdata$dist~bootdata$speed))

      t<-data.frame(d[i],f[i],r[i]);names(t)<-c("speed","dist","fitted","resid")

      l[[i]]<-t

} #end loop

#end example

# sortListByMean <- function(List) {
# List[order(vapply(List, mean, 0))]
# }
# sortedL <- sortListByMean(l)
