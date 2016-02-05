## merge_meta_full_HSV_hists.R
##
## 
library(data.table)
#####-------------------------------------
##### setup data
#####-------------------------------------
setwd("~/Documents/_2014 DeviantArt work/")
#dt = fread("~/Documents/wifire/processedData/features/output_SD_all.csv", header = FALSE)
load("./processedData/features/digital_trad_art_MATLABfeatures_sizes.Rda")

normalized.sum = function(x){return(x/max(x))}

HSV.jpg = fread("./processedData/features/full_HSV_hists/Digital_Trad_HSV_JPG.csv", header = FALSE)
setnames(HSV.jpg, names(HSV.jpg), c("filename.path", paste0("H", c(1:180)), paste0("S", c(1:256)), paste0("V", c(1:256))))
res.H = HSV.jpg[,normalized.sum(.SD), by = filename.path, .SDcols = paste0("H", c(1:180))]
res.S = HSV.jpg[,normalized.sum(.SD), by = filename.path, .SDcols = paste0("S", c(1:256))]
res.V = HSV.jpg[,normalized.sum(.SD), by = filename.path, .SDcols = paste0("V", c(1:256))]

HSV.png = fread("./processedData/features/full_HSV_hists/Digital_Trad_HSV_PNG.csv", header = FALSE)
setnames(HSV.png, names(HSV.png), c("filename.path", paste0("H", c(1:180)), paste0("S", c(1:256)), paste0("V", c(1:256))))
res.H = HSV.png[,normalized.sum(.SD), by = filename.path, .SDcols = paste0("H", c(1:180))]
res.S = HSV.png[,normalized.sum(.SD), by = filename.path, .SDcols = paste0("S", c(1:256))]
res.V = HSV.png[,normalized.sum(.SD), by = filename.path, .SDcols = paste0("V", c(1:256))]

HSV = rbind(HSV.png, HSV.jpg)
rm(HSV.png)
rm(HSV.jpg)

HSV$filename = sapply(HSV$filename.path, FUN = function(x) strsplit(x, split = "/home/myazdani/Documents/DeviantArt/Images/Categories/")[[1]][2])
HSV$filename.path = NULL

dt.merged = merge(dt, HSV, by = "filename")

save(dt.merged, file = "./processedData/features/digital_trad_art_MATLABfeatures_sizes_FULL_HSV_hists.Rda")
