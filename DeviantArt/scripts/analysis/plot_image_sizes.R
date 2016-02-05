## plot_image_sizes
##
setwd("~/Documents/_2014 DeviantArt work/")

load("./metadata/DeviantArt_digital_query.Rda")
load("./metadata/DeviantArt_trad_query.Rda")

df = rbind(digital_art_query, trad_art_query)