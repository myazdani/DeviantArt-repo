## random_samples.R
##
## 
library(data.table)
library(plotly)
library(plyr)
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

get.fine.cat = function(x) {
  splitted = strsplit(x, split = "/")[[1]]
  fine.cat = paste0(splitted[1], "/")
  for (i in c(2:(length(splitted)-1))){
    fine.cat = paste0(fine.cat, splitted[i], "/")
  }
  return(fine.cat)
}


dt$fine.cat = sapply(dt$filename, get.fine.cat)
table(dt$fine.cat) -> cat.lengths
cat.lengths = cat.lengths[order(cat.lengths, decreasing= TRUE)]
cat.dist = data.frame(cat.name = names(cat.lengths), num.images = cat.lengths, image.cum.percentage = 100*cumsum(cat.lengths)/sum(cat.lengths))
cat.dist$rank = c(1:nrow(cat.dist))
ggplot(cat.dist, aes(x = rank, y = image.cum.percentage, label = cat.name)) + geom_point() + geom_text()-> p

py <- plotly()
out <- py$ggplotly(p)
plotly_url <- out$response$url

get.random.samples = function(df) {
  num.samples = 2
  mysample = df[sample(1:nrow(df), num.samples,replace=FALSE),]
  mysample$year.cat = paste0(mysample$Year, ".", mysample$category)
  return(mysample)
}



top.fine.cats = as.character(cat.dist[which(cat.dist$image.cum.percentage < 80),"cat.name"])

res = ddply(subset(dt, fine.cat %in% top.fine.cats), .(fine.cat), get.random.samples)


write.table(res, file = "./processedData/montages/montage_data_files/top_20_percent_cats_2_images_each.txt", row.names = FALSE, quote = FALSE, sep = "\t")
