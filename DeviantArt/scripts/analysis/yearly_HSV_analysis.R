## yearly_HSV_analysis.R
##
##

setwd("~/Documents/_2014 DeviantArt work/")

library(data.table)
# df = read.csv("./processedData/features/digital_art_8binHSV.c2.csv", header = TRUE, stringsAsFactors = FALSE)
# df = subset(df, cat_1 == "digitalart")
# df$X = NULL
# df$X.1 = NULL
# dt = as.data.table(df)

#dt = fread("./processedData/features/digital_trad_art_MATLABfeatures.csv", header = TRUE)
load("./processedData/features/digital_trad_art_MATLABfeatures.Rda")
res = as.data.frame(dt[,lapply(.SD, FUN = function(x) as.double(mean(x))), by = list(Year, category), .SDcols = 43:66])
res = subset(res, Year != 2000 & category != "Traditional Art") 


res.H = res[,c(1,which(grepl("HH", names(res))))]
res.S = res[,c(1,which(grepl("SH", names(res))))]
res.V = res[,c(1,which(grepl("VH", names(res))))]

library(ggplot2)
library(reshape)
res.H.m = melt(res.H)
res.S.m = melt(res.S)
res.V.m = melt(res.V)

ggplot(res.H.m, aes(x = variable, y = value)) + geom_point() + facet_wrap(~ Year, nrow = 3) 
ggplot(res.S.m, aes(x = variable, y = value)) + geom_point() + facet_wrap(~ Year, nrow = 3) 
ggplot(res.V.m, aes(x = variable, y = value)) + geom_point() + facet_wrap(~ Year, nrow = 3) 

#####-------------------------------------
#####
#####-------------------------------------
return_mds = function(data.matrix){
  row.names(data.matrix) = data.matrix$Year
  dist.matrix = dist(sqrt(data.matrix[,-1]), method = "euclidean")
  fit <- cmdscale(dist.matrix,eig=TRUE, k=2) # k is the number of dim  
  mds = data.frame(Year = row.names(fit$points), coord.1 = fit$points[,1], coord.2 = fit$points[,2])
  return(mds)
}

mds.H = return_mds(res.H)
ggplot(mds.H, aes(x = coord.1, y = coord.2, label = Year)) + geom_text() + ggtitle("MDS of Hue Distributions")
mds.S = return_mds(res.S)
ggplot(mds.S, aes(x = coord.1, y = coord.2, label = Year)) + geom_text() + ggtitle("MDS of Saturation Distributions")
mds.V = return_mds(res.V)
ggplot(mds.V, aes(x = coord.1, y = coord.2, label = Year)) + geom_text() + ggtitle("MDS of Value Distributions")


#####-------------------------------------
##### setup hour distance matrix
#####-------------------------------------
first.row = c(c(0:12), c(11:1))
dist.hours = toeplitz(first.row)
rownames(dist.hours) = res.S$hour[order(as.integer(res.S$hour))]
colnames(dist.hours) = res.S$hour[order(as.integer(res.S$hour))]

