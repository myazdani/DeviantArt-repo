## pca_HSV.R
##
## 
library(data.table)
#####-------------------------------------
##### setup data
#####-------------------------------------
setwd("~/Documents/_2014 DeviantArt work/")
#dt = fread("~/Documents/wifire/processedData/features/output_SD_all.csv", header = FALSE)
load("./processedData/features/digital_trad_art_MATLABfeatures_sizes.Rda")
dt$date = as.Date(as.POSIXct(dt$uploaded_date, format="%Y-%m-%d  %H:%M:%S"))

#dt.reduced = subset(dt, Hue_Mean != 0)
dt.reduced = dt
dt.reduced$uploaded_date = NULL
#####-------------------------------------
##### PCA visualization 
#####-------------------------------------
dt.H = as.data.frame(dt.reduced[,c(which(grepl("date", names(dt.reduced))), which(grepl("category", names(dt.reduced))),which(grepl("HH", names(dt.reduced)))), with = FALSE])
dt.S = as.data.frame(dt.reduced[,c(which(grepl("date", names(dt.reduced))), which(grepl("category", names(dt.reduced))),which(grepl("SH", names(dt.reduced)))), with = FALSE])
dt.V = as.data.frame(dt.reduced[,c(which(grepl("date", names(dt.reduced))), which(grepl("category", names(dt.reduced))),which(grepl("VH", names(dt.reduced)))), with = FALSE])


return_PCA = function(df){
  pca.res = princomp(df)
  return(pca.res$scores[,c(1:4)])
}

pca.H = cbind(dt.H[,c(1,2)], return_PCA(dt.H[,-c(1,2)]))
ggplot(pca.H, aes(x = date, y = Comp.1, colour = category)) + geom_point(alpha = .1) + stat_smooth(method = "lm") + ggtitle("Hue") -> p
print(p)
pca.S = cbind(dt.S[,c(1,2)], return_PCA(dt.S[,-c(1,2)]))
ggplot(pca.S, aes(x = date, y = Comp.1, colour = category)) + geom_point(alpha = .3) + stat_smooth(method = "lm") -> p

pca.V = cbind(dt.V[,c(1,2)], return_PCA(dt.V[,-c(1,2)]))
ggplot(pca.V, aes(x = date, y = Comp.1, colour = category)) + geom_point(alpha = .3) + stat_smooth(method = "lm") -> p

#####-------------------------------------
##### distance from average....
#####-------------------------------------
avg.H = apply(dt.H[,-c(1,2)], 2, mean)
avg.S = apply(dt.S[,-c(1,2)], 2, mean)
avg.V = apply(dt.V[,-c(1,2)], 2, mean)

dt.H$distance.to.avg = apply(dt.H[,-c(1,2)], 1, FUN = function(x) as.numeric(dist(rbind(x, avg.H))))
dt.S$distance.to.avg = apply(dt.S[,-c(1,2)], 1, FUN = function(x) as.numeric(dist(rbind(x, avg.S))))
dt.V$distance.to.avg = apply(dt.V[,-c(1,2)], 1, FUN = function(x) as.numeric(dist(rbind(x, avg.V))))

ggplot(dt.H, aes(x = date, y = distance.to.avg, colour = category)) + geom_point(alpha = .1)
ggplot(dt.S, aes(x = date, y = distance.to.avg, colour = category)) + geom_point(alpha = .1)
ggplot(dt.V, aes(x = date, y = distance.to.avg, colour = category)) + geom_point(alpha = .1)
