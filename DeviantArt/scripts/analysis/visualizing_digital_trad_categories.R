## visualizing_digital_trad__categories.R
##
setwd("~/Documents/_2014 DeviantArt work/")

load("./metadata/DeviantArt_digital_query.Rda")
load("./metadata/DeviantArt_trad_query.Rda")

df = rbind(digital_art_query, trad_art_query)
df$year = sapply(df$uploaded_date, FUN = function(x) substr(x, 1, 4))
df$category = sapply(df$catpath, FUN = function(x) paste0(strsplit(x, split = "/")[[1]][1], "/", strsplit(x, split = "/")[[1]][2]))

df.reduced = subset(df, category %in%  names((table(df$category)[table(df$category) > 5e3])))
df.reduced[which(df.reduced$category == "fractals/NA"),"category"] = "digitalart/fractals"
library(ggplot2)
library(reshape)

cat.year = as.data.frame.matrix(table(df.reduced$year, df.reduced$category))
cat.year$year = row.names(cat.year)
cat.m = melt(cat.year, id.vars="year")

ggplot(cat.m, aes(x = year, y = value, colour = variable, group = variable)) + geom_line(size=3) + scale_colour_brewer(palette="Set3") + theme_bw() -> p
print(p)

cat.m.subset = subset(cat.m, variable %in% c("digitalart/drawings", "traditional/drawings", "digitalart/paintings", "traditional/paintings", "digitalart/mixedmed", "traditional/mixedmedia"))

ggplot(cat.m.subset, aes(x = year, y = value)) + geom_point() + facet_wrap(~ variable, scales = "free_y", nrow = 2)
