## random_samples.R
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
dt$filename = sapply(dt$filename, FUN = function(x) paste0("/Volumes/SWS06/DeviantART\ Project/Images/Categories/", x))
dt$Year = as.numeric(dt$Year)
dt$GIF = sapply(dt$filename, FUN = function(x) grepl(".gif", x))
dt = subset(dt, GIF == FALSE)

dt$cat.2 = sapply(dt$filename, FUN = function(x) paste0(strsplit(x, split = "/")[[1]][7], "/",strsplit(x, split = "/")[[1]][8]))
dt.reduced = subset(dt, cat.2 == "Traditional Art/Drawings" | cat.2 == "Digital Art/Photomanipulation" | cat.2 == "Digital Art/Drawings")
dt.H = as.data.frame(dt.reduced[,c(which(grepl("Year", names(dt.reduced))), which(grepl("cat.2", names(dt.reduced))),which(grepl("HH_8", names(dt.reduced)))), with = FALSE])
dt.S = as.data.frame(dt.reduced[,c(which(grepl("Year", names(dt.reduced))), which(grepl("cat.2", names(dt.reduced))),which(grepl("SH_8", names(dt.reduced)))), with = FALSE])
dt.V = as.data.frame(dt.reduced[,c(which(grepl("Year", names(dt.reduced))), which(grepl("cat.2", names(dt.reduced))),which(grepl("VH_8", names(dt.reduced)))), with = FALSE])

sum_bins= function(df) {
  temp = apply(df[,c(3:10)], 1, FUN = function(x) sum(x^4))
  return(mean(temp))
}

binary_bins = function(df){
  temp = apply(df[,c(3:10)], 1, FUN = function(x) length(which(x > .001)))
  return(mean(temp))
}

ddply(dt.H, .(Year, cat.2), binary_bins ) -> res.H
ggplot(res.H, aes(x = Year, y = V1, colour = cat.2)) + geom_point(size = 4)

ddply(dt.S, .(Year, cat.2), binary_bins ) -> res.S
ggplot(res.S, aes(x = Year, y = V1, colour = cat.2)) + geom_point(size = 4)

ddply(dt.V, .(Year, cat.2), binary_bins ) -> res.V
ggplot(res.V, aes(x = Year, y = V1, colour = cat.2)) + geom_point(size = 4)