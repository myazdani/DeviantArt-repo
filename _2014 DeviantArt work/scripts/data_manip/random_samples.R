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


get.5k.samples = function(df) {
  num.samples = 5e3
  mysample = df[sample(1:nrow(df), num.samples,replace=FALSE),]
  mysample$year.cat = paste0(mysample$Year, ".", mysample$category)
  return(mysample)
}
get.2k.samples = function(df) {
  num.samples = 1e3
  mysample = df[sample(1:nrow(df), num.samples,replace=FALSE),]
  mysample$year.cat = paste0(mysample$Year, ".", mysample$cat.2)
  return(mysample)
}
dt$cat.2 = sapply(dt$filename, FUN = function(x) paste0(strsplit(x, split = "/")[[1]][7], "/",strsplit(x, split = "/")[[1]][8]))
dt.reduced = subset(dt, cat.2 == "Traditional Art/Drawings" | cat.2 == "Digital Art/Photomanipulation" | cat.2 == "Digital Art/Drawings")
library(plyr)

#res = ddply(subset(dt, Year > 2003), .(category, Year), get.5k.samples)
res = ddply(subset(dt.reduced, Year > 2003), .(cat.2, Year), get.2k.samples)

#ggplot(res, aes(x=Hue_Mean)) + geom_histogram(binwidth=.09) + facet_wrap(~year.cat)

res$username = NULL
res$title = NULL

sort.df = function(df){
  df.start = subset(df, Value_Mean < .25)
  df.end = subset(df, Value_Mean > .75)
  df.middle = subset(df, Value_Mean >= .25 & Value_Mean <= .75)
  df.middle = df.middle[order(df.middle$Hue_Mean), ]
  df.start = df.start[order(df.start$Hue_Mean), ]
  df.end = df.end[order(df.end$Hue_Mean), ]
  return(rbind(df.start, df.middle, df.end))
}

res.list = dlply(res, .(year.cat), sort.df )
res.df = ddply(res, .(year.cat), sort.df )
 
#write.table(res.list[["2004.Traditional Art"]], file = "./processedData/montage_data/trad_art_2004_5K_sample_separate_BW_sort.txt", row.names = FALSE, quote = FALSE, sep = "\t")
#write.table(res.list[["2010.Traditional Art"]], file = "./processedData/montage_data/trad_art_2010_5K_sample_separate_BW_sort.txt", row.names = FALSE, quote = FALSE, sep = "\t")
#write.table(res.list[["2004.Digital Art"]], file = "./processedData/montage_data/digital_art_2004_5K_sample_separate_BW_sort.txt", row.names = FALSE, quote = FALSE, sep = "\t")
#write.table(res.list[["2010.Digital Art"]], file = "./processedData/montage_data/digital_art_2010_5K_sample_separate_BW_sort.txt", row.names = FALSE, quote = FALSE, sep = "\t")

# write.table(res.list[["2004.Digital Art/Photomanipulation"]], file = "./processedData/montage_data/digital_art_photomanip_2004_2K_sample_separate_BW_sort.txt", row.names = FALSE, quote = FALSE, sep = "\t")
# write.table(res.list[["2010.Digital Art/Photomanipulation"]], file = "./processedData/montage_data/digital_art_photomanip_2010_2K_sample_separate_BW_sort.txt", row.names = FALSE, quote = FALSE, sep = "\t")
# write.table(res.list[["2004.Traditional Art/Drawings"]], file = "./processedData/montage_data/trad_art_drawings_2004_2K_sample_separate_BW_sort.txt", row.names = FALSE, quote = FALSE, sep = "\t")
# write.table(res.list[["2010.Traditional Art/Drawings"]], file = "./processedData/montage_data/trad_art_drawings_2010_2K_sample_separate_BW_sort.txt", row.names = FALSE, quote = FALSE, sep = "\t")

write.table(res.list[["2004.Digital Art/Drawings"]], file = "./processedData/montage_data/digital_art_drawings_2004_1K_sample_separate_BW_sort.txt", row.names = FALSE, quote = FALSE, sep = "\t")
write.table(res.list[["2010.Digital Art/Drawings"]], file = "./processedData/montage_data/digital_art_drawings_2010_1K_sample_separate_BW_sort.txt", row.names = FALSE, quote = FALSE, sep = "\t")
write.table(res.list[["2004.Traditional Art/Drawings"]], file = "./processedData/montage_data/trad_art_drawings_2004_1K_sample_separate_BW_sort.txt", row.names = FALSE, quote = FALSE, sep = "\t")
write.table(res.list[["2010.Traditional Art/Drawings"]], file = "./processedData/montage_data/trad_art_drawings_2010_1K_sample_separate_BW_sort.txt", row.names = FALSE, quote = FALSE, sep = "\t")
write.table(res.list[["2004.Digital Art/Photomanipulation"]], file = "./processedData/montage_data/digital_art_photomanip_2004_1K_sample_separate_BW_sort.txt", row.names = FALSE, quote = FALSE, sep = "\t")
write.table(res.list[["2010.Digital Art/Photomanipulation"]], file = "./processedData/montage_data/digital_art_photomanip_2010_1K_sample_separate_BW_sort.txt", row.names = FALSE, quote = FALSE, sep = "\t")

