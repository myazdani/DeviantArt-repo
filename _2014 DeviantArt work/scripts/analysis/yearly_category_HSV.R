## yearly_category_HSV.R
##
## 
library(data.table)
#####-------------------------------------
##### setup data
#####-------------------------------------
setwd("~/Documents/_2014 DeviantArt work/")
#dt = fread("~/Documents/wifire/processedData/features/output_SD_all.csv", header = FALSE)
load("./processedData/features/digital_trad_art_MATLABfeatures_sizes.Rda")

res = as.data.frame(dt[,lapply(.SD, FUN = function(x) as.double(mean(x)/sd(x))), by = list(Year, category), .SDcols = 45:68])
res = subset(res, Year != 2000)

res.H = res[,c(1,2,which(grepl("HH", names(res))))]
res.S = res[,c(1,2,which(grepl("SH", names(res))))]
res.V = res[,c(1,2,which(grepl("VH", names(res))))]
rm(res)

#####-------------------------------------
##### Distribution visualization
#####-------------------------------------
library(ggplot2)
library(reshape)
res.H.m = melt(res.H)
res.S.m = melt(res.S)
res.V.m = melt(res.V)
 
#ggplot(res.H.m, aes(x = variable, y = value, colour = category)) + geom_point(size = 5) + facet_wrap(~ Year, nrow = 3) 
#ggplot(res.S.m, aes(x = variable, y = value, colour = category)) + geom_point(size = 5) + facet_wrap(~ Year, nrow = 3) 
#ggplot(res.V.m, aes(x = variable, y = value, colour = category)) + geom_point(size = 5) + facet_wrap(~ Year, nrow = 3) 

#####-------------------------------------
##### PCA visualization 
#####-------------------------------------
dt.H = as.data.frame(dt[,c(which(grepl("Year", names(dt))), which(grepl("category", names(dt))),which(grepl("HH", names(dt)))), with = FALSE])
dt.S = as.data.frame(dt[,c(which(grepl("Year", names(dt))), which(grepl("category", names(dt))),which(grepl("SH", names(dt)))), with = FALSE])
dt.V = as.data.frame(dt[,c(which(grepl("Year", names(dt))), which(grepl("category", names(dt))),which(grepl("VH", names(dt)))), with = FALSE])


return_PCA = function(df){
  pca.res = princomp(df)
  return(pca.res$scores[,c(1:4)])
}

pca.H = cbind(dt.H[,c(1,2)], return_PCA(dt.H[,-c(1,2)]))
p = ggplot(pca.H, aes(x = Comp.1, y = Comp.2, colour = category, label = Year)) + geom_text() + geom_point(size = 1, alpha = .1) + ggtitle("PCA of Hue Distributions")
##ggsave(file = "./figures/Hue_MDS.pdf", p)
pca.S = cbind(dt.S[,c(1,2)], return_PCA(dt.S[,-c(1,2)]))
p = ggplot(pca.S, aes(x = Comp.1, y = Comp.2, label = Year, colour = category)) + geom_text() + ggtitle("PCA of Saturation Distributions")
#ggsave(file = "./figures/Saturation_MDS.pdf", p)
pca.V = cbind(dt.V[,c(1,2)], return_PCA(dt.V[,-c(1,2)]))
p = ggplot(pca.V, aes(x = Comp.1, y = Comp.2, label = Year, colour = category)) + geom_text() + ggtitle("PCA of Value Distributions")
#ggsave(file = "./figures/Value_MDS.pdf", p)



#####-------------------------------------
##### PCA visualization of aggregates
#####-------------------------------------

pca.H = cbind(res.H[,c(1,2)], return_PCA(res.H[,-c(1,2)]))
p = ggplot(pca.H, aes(x = Comp.1, y = Comp.2, label = Year, colour = category)) + geom_text() + ggtitle("PCA of Hue Distributions")
ggsave(file = "./figures/Hue_PCA.pdf", p)
pca.S = cbind(res.S[,c(1,2)], return_PCA(res.S[,-c(1,2)]))
p = ggplot(pca.S, aes(x = Comp.1, y = Comp.2, label = Year, colour = category)) + geom_text() + ggtitle("PCA of Saturation Distributions")
ggsave(file = "./figures/Saturation_PCA.pdf", p)
pca.V = cbind(res.V[,c(1,2)], return_PCA(res.V[,-c(1,2)]))
p = ggplot(pca.V, aes(x = Comp.1, y = Comp.2, label = Year, colour = category)) + geom_text() + ggtitle("PCA of Value Distributions")
ggsave(file = "./figures/Value_PCA.pdf", p)




#####-------------------------------------
##### MDS visualization
#####-------------------------------------
return_mds_per_category = function(df){
  categories = unique(df$category)
  categories.mds = data.frame(category = NA, Year = NA, coord.1 = NA, coord.2 = NA)
  for (i in c(1:length(categories))){
    df.temp = subset(df, category == categories[i])
    df.temp$category = NULL
    row.names(df.temp) = df.temp$Year
    dist.matrix = dist(df.temp[, -which(names(df.temp) == "Year")], method = "euclidean")
    fit <- cmdscale(dist.matrix,eig=TRUE, k=2) # k is the number of dim 
    mds.1 = fit$points[,1]
    mds.2 = fit$points[,2]
    
    if (max(mds.1) < max(abs(mds.1))) { mds.1 = -1*mds.1}
    #if (max(mds.2) < max(abs(mds.2))) { mds.2 = -1*mds.2}
    
    categories.mds = rbind(categories.mds, data.frame(category = categories[i], Year = row.names(fit$points), coord.1 = mds.1, coord.2 = mds.2))
  }
  return(categories.mds[-1,])
}

mds.H = return_mds_per_category(res.H)
#p = ggplot(mds.H, aes(x = coord.1, y = coord.2, label = Year)) + facet_wrap( ~ category, nrow = 2) + geom_text() + ggtitle("MDS of Hue Distributions")
p = ggplot(mds.H, aes(x = coord.1, y = coord.2, label = Year, colour = category)) + geom_text() + ggtitle("MDS of Hue Distributions")
##ggsave(file = "./figures/Hue_MDS.pdf", p)
mds.S = return_mds_per_category(res.S)
#p = ggplot(mds.S, aes(x = coord.1, y = coord.2, label = Year)) + facet_wrap( ~ category, nrow = 2) + geom_text() + ggtitle("MDS of Saturation Distributions")
p = ggplot(mds.S, aes(x = coord.1, y = coord.2, label = Year, colour = category)) + geom_text() + ggtitle("MDS of Saturation Distributions")
#ggsave(file = "./figures/Saturation_MDS.pdf", p)
mds.V = return_mds_per_category(res.V)
p = ggplot(mds.V, aes(x = coord.1, y = coord.2, label = Year, colour = category)) + geom_text() + ggtitle("MDS of Value Distributions")
#ggsave(file = "./figures/Value_MDS.pdf", p)

#####-------------------------------------
##### setup Year distance matrix
#####-------------------------------------
first.row = c(0:9)
dist.years = toeplitz(first.row)
unique.years = as.numeric(unique(res.S$Year))
rownames(dist.years) = unique.years[order(unique.years)]
colnames(dist.years) = unique.years[order(unique.years)]

#####-------------------------------------
##### setup hour distance matrix
#####-------------------------------------
get.dist.df = function(x){
  if (is.matrix(x)){ 
    # if it is a matrix already, then assume that it is a proper distance matrix
    z = x
  } 
  else{
    z = as.matrix(dist(x)) 
  }
  z[lower.tri(z,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
  z=as.data.frame(as.table(z))  #Turn into a 3-column table
  z=na.omit(z)  #Get rid of the junk we flagged above
  z=z[order(-abs(z$Freq)),] 
  
  z$Var1 = as.character(z$Var1)
  z$Var2 = as.character(z$Var2)
  ordered.pair = c()
  for (i in c(1:nrow(z))){
    if (z$Var1[i] > z$Var2[i]){
      ordered.pair = c(ordered.pair, 
                       paste(z$Var1[i], "-",z$Var2[i]))
    }
    else{
      ordered.pair = c(ordered.pair,
                       paste(z$Var2[i], "-",z$Var1[i]))
    }
  }
  z$pairs = ordered.pair
  z$Var1 = NULL
  z$Var2 = NULL
  return(z)
}

dist.years.df = get.dist.df(dist.years)
names(dist.years.df)[1] = "years.dist"

hours.HSV.list = list()
categories = unique(res.H$category)
library(plyr)
for (i in c(1:length(categories))){
  res.H.temp = subset(res.H, category == categories[i])
  res.H.temp$category = NULL
  res.S.temp = subset(res.S, category == categories[i])
  res.S.temp$category = NULL
  res.V.temp = subset(res.V, category == categories[i])
  res.V.temp$category = NULL
  
  ## get H distances
  row.names(res.H.temp) = res.H.temp[,1]
  H.dist = get.dist.df(res.H.temp[,-1])
  names(H.dist)[1] = "hue.dist"
  ## get S distances
  row.names(res.S.temp) = res.S.temp[,1]
  S.dist = get.dist.df(res.S.temp[,-1])
  names(S.dist)[1] = "saturation.dist"
  ## get V distancesobjects()
  row.names(res.V.temp) = res.V.temp[,1]
  V.dist = get.dist.df(res.V.temp[,-1])
  names(V.dist)[1] = "value.dist"
  hours.HSV = join_all(list(dist.years.df, H.dist, S.dist, V.dist))
  hours.HSV$category = categories[i]
  hours.HSV.list[[i]] = hours.HSV
}


hours.HSV.res = do.call(rbind, hours.HSV.list)

p = ggplot(hours.HSV.res, aes(x = years.dist, y = hue.dist, colour = category))  + geom_point(size = 3, alpha = .9) + stat_smooth(method = lm) + xlab("Difference in years") + ylab("Difference in Hue Distributions")
#ggsave(file = "./figures/Hue_time.pdf", p)
p = ggplot(hours.HSV.res, aes(x = years.dist, y = saturation.dist, colour = category))  + geom_point(size = 3, alpha = .9) + stat_smooth(method = lm) + xlab("Difference in years") + ylab("Difference in Saturation Distributions")
#ggsave(file = "./figures/Saturation_time.pdf", p)
p = ggplot(hours.HSV.res, aes(x = years.dist, y = value.dist, colour = category))  + geom_point(size = 3, alpha = .9) + stat_smooth(method = lm) + xlab("Difference in years") + ylab("Difference in Value Distributions")
#ggsave(file = "./figures/Value_time.pdf", p)
