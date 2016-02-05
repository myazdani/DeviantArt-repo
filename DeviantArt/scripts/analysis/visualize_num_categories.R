## visualizing_num_categories.R
##
setwd("~/Documents/_2014 DeviantArt work/")

#df = read.delim("./old_work/1million_metadata.txt", stringsAsFactors = FALSE)
df = read.delim("~/Dropbox/DeviantArt/data/metadata_part2_UPDATED.txt", header = TRUE, stringsAsFactors = FALSE)



df$year = sapply(df$uploaded_date, FUN = function(x) substr(x, 7, 10))

df$cat.1 = sapply(df$catpath, FUN = function(x) strsplit(x, split = "/")[[1]][1])
df$num.cats = sapply(df$catpath, FUN = function(x) length(strsplit(x, split = "/")[[1]]))

library(ggplot2)


ggplot(df, aes(factor(year), num.cats)) + geom_point()

df =  df[-which(nchar(df$year) == 0),]

df.reduced = subset(df, cat.1 %in%  names((table(df$cat.1)[table(df$cat.1) > 30e3])))

cat.1.year = as.data.frame.matrix(table(df.reduced$year, df.reduced$cat.1))
cat.1.year$year = row.names(cat.1.year)

library(reshape)

cat.1.m = melt(cat.1.year, id.vars="year")

ggplot(cat.1.m, aes(x = year, y = value, colour = variable, group = variable)) + geom_point() + geom_line() -> p
