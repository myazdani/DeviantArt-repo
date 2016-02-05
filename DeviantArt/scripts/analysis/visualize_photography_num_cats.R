## visualizing_num_categories.R
##
setwd("~/Documents/_2014 DeviantArt work/")

df.1 = read.delim("~/Dropbox/DeviantArt/data/metadata_part2_UPDATED.txt", header = TRUE, stringsAsFactors = FALSE)
df.1$year = sapply(df.1$uploaded_date, FUN = function(x) substr(x, 1, 4))
df.2 = read.csv("~/Dropbox/DeviantArt/data/metadata_deviations.txt", header = TRUE, stringsAsFactors = FALSE)
df.2$year = sapply(df.2$uploaded_date, FUN = function(x) substr(x, 7, 10))

df = rbind(df.1[,names(df.2)], df.2)

df$cat.0 = sapply(df$catpath, FUN = function(x) strsplit(x, split = "/")[[1]][1])
df$cat.1 = sapply(df$catpath, FUN = function(x) strsplit(x, split = "/")[[1]][2])
df$num.cats = sapply(df$catpath, FUN = function(x) length(strsplit(x, split = "/")[[1]]))

library(ggplot2)


ggplot(df, aes(factor(year), num.cats)) + geom_point()


df.reduced = subset(df, cat.0 == "photography")

cat.1.year = as.data.frame.matrix(table(df.reduced$year, df.reduced$cat.1))
cat.1.year$year = row.names(cat.1.year)

library(reshape)

cat.1.m = melt(cat.1.year, id.vars="year")

ggplot(cat.1.m, aes(x = year, y = value, colour = variable, group = variable)) + geom_point() + geom_line() -> p

library(plotly)
py = plotly()
py$ggplotly(p, kwargs=list(filename="DA-photography", fileopt="overwrite"))

