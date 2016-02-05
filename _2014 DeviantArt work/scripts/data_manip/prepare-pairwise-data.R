## prepare-pairwise-data.R
##
## 
library(data.table)
library(plyr)
#####-------------------------------------
##### setup data
#####-------------------------------------
setwd("~/Documents/_2014 DeviantArt work/")


load("./processedData/features/digital_trad_art_MATLABfeatures_sizes.Rda")
res = as.data.frame(dt[,lapply(.SD, FUN = function(x) as.double(mean(x)/sd(x))), by = list(Year, category), .SDcols = 45:68])
res = subset(res, Year != 2000)

res.H = res[,c(1,2,which(grepl("HH", names(res))))]
res.S = res[,c(1,2,which(grepl("SH", names(res))))]
res.V = res[,c(1,2,which(grepl("VH", names(res))))]
rm(res)

# load("./processedData/features/digital_trad_art_MATLABfeatures_sizes_FULL_HUE_probs.Rda")
# res = as.data.frame(dt.merged[,lapply(.SD, FUN = function(x) as.double(mean(x))), by = list(Year, category), .SDcols = 72:251])
# res.H = subset(res, Year != 2000)
# load("./processedData/features/digital_trad_art_MATLABfeatures_sizes_FULL_SATURATION_probs.Rda")
# res = as.data.frame(dt.merged[,lapply(.SD, FUN = function(x) as.double(mean(x))), by = list(Year, category), .SDcols = 72:327])
# res.S = subset(res, Year != 2000)
# load("./processedData/features/digital_trad_art_MATLABfeatures_sizes_FULL_VALUE_probs.Rda")
# res = as.data.frame(dt.merged[,lapply(.SD, FUN = function(x) as.double(mean(x))), by = list(Year, category), .SDcols = 72:327])
# res.V = subset(res, Year != 2000)

pairwise.multiplications = function(df1){
  df1$category = NULL
  ind <- t(combn(nrow(df1),2))
  out <- apply(ind, 1, function(x) (df1[x[1], -1] - df1[x[2], -1])*(df1[x[1], -1] - df1[x[2], -1]))
  ids  <- apply(ind, 1, function(x) data.frame(id.1 = as.numeric(df1[x[1], 1]), id.2 = as.numeric(df1[x[2], 1])))
  df.features = do.call(rbind, out)
  df = cbind(do.call(rbind, ids), df.features)
  df$year.diff.squared = (df$id.1 - df$id.2)^2
  return(df)
}

pair.H = ddply(res.H, .(category), pairwise.multiplications)
pair.S = ddply(res.S, .(category), pairwise.multiplications)
pair.V = ddply(res.V, .(category), pairwise.multiplications)

write.csv(pair.H, file = "./processedData/pairs_features/H_full_hist_pairs.csv", row.names = FALSE, quote = FALSE)
write.csv(pair.S, file = "./processedData/pairs_features/S_full_hist_pairs.csv", row.names = FALSE, quote = FALSE)
write.csv(pair.V, file = "./processedData/pairs_features/V_full_hist_pairs.csv", row.names = FALSE, quote = FALSE)

pair.H$H.dist = apply(pair.H[,c(4:11)], 1, sum)
#pair.H$H.dist = apply(pair.H[,c(4:183)], 1, sum)

pair.H$id.1 = NULL
pair.H$id.2 = NULL
pair.H.m = melt(pair.H, id.vars = c("category", "year.diff.squared"))

ggplot(pair.H.m, aes(x = sqrt(year.diff.squared), y = sqrt(value), colour = category)) + geom_point() + facet_wrap(~variable, scales = "free_y") -> p

weights = c(3.11e-5, 1.66e3, 2.75e3, 6.96e-4, 7.28e-4, 8.05e-4, 8.19e-4, 2.14e-4)
#weights = read.csv("./processedData/weighting_full_hists_hue.csv", header = FALSE, stringsAsFactors = FALSE)
#weights = as.numeric(weights$V1)

#pair.H$weight.dist = apply(pair.H[,c(2:181)], 1, FUN = function(x) sum(x*weights))
pair.H$weight.dist = apply(pair.H[,c(2:9)], 1, FUN = function(x) sum(x*weights))
ggplot(pair.H, aes(x = sqrt(year.diff.squared), y = sqrt(H.dist), colour = category)) + geom_point() -> p
ggplot(pair.H, aes(x = sqrt(year.diff.squared), y = sqrt(weight.dist), colour = category)) + geom_point() -> p
