## text_analysis.R
##
## 
library(data.table)
library(ggplot2)
#####-------------------------------------
##### setup data
#####-------------------------------------
setwd("~/Documents/_2014 DeviantArt work/")
load("./processedData/features/digital_trad_art_MATLABfeatures_sizes.Rda")
dt$num.chars = sapply(dt$title, nchar)
dt$cat.2 = sapply(dt$filename, FUN = function(x) paste0(strsplit(x, split = "/")[[1]][1], "/",strsplit(x, split = "/")[[1]][2]))
dt.reduced = subset(dt, cat.2 %in%  names((table(dt$cat.2)[table(dt$cat.2) > 10e3])))
#res = as.data.frame(dt[,list(avg.chars = mean(num.chars), sd.chars = sd(num.chars), num.images = length(unique(filename))), by = list(Year)])

#ggplot(res, aes(x = Year, y = avg.chars)) + geom_point(size = 5)

#ggplot(res, aes(x = as.factor(Year), colour = category)) + geom_boxplot(aes(lower = avg.chars - sd.chars, upper = avg.chars + sd.chars, middle = avg.chars, ymin = avg.chars - 3*sd.chars, ymax = avg.chars + 3*sd.chars), stat = "identity")

top.number = 10

## get all tags from content column by looking for strings with #
get.all.words = function(x) {
  content.split = strsplit(x, split = " ")[[1]]
  return(content.split)
}


get.top.words = function(query.x) { 
  
  tags.list = unlist(lapply(query.x$title, get.all.words), recursive = FALSE, use.names = FALSE)
  
  #get the frequency of how often words are used
  as.data.frame(table(tags.list)) -> tag.freq
  tag.freq$tags.list = as.character(tag.freq$tags.list)
  #tag.freq = na.omit(tag.freq)
  
  # make probability
  tag.freq$Freq = tag.freq$Freq/sum(tag.freq$Freq)
  #select the top 100 tags as tags to use
  tags.to.use = tag.freq[order(tag.freq$Freq, decreasing=TRUE)[c(1:top.number)], ]
  
  #tags.to.use = tags.to.use[-which(sapply(tags.to.use, nchar) < 2)] #get rid of the tag found to be the single "#" (empty tag)
  #tags.to.use = unname(sapply(tags.to.use, FUN = function(x) strsplit(x, split = "#")[[1]][2]))
  #tags.to.use$date = query.x$date[1]
  return(tags.to.use)
}
res = ddply(dt, .(Year, category), get.top.words)

ggplot(subset(res, Year != 2000), aes(x = Year, y = exp(Freq), label = tags.list, colour = category)) + geom_text()
