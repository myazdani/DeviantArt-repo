## visualizing_global_features.R
##
setwd("~/Documents/_2014 DeviantArt work/")
library(ggplot2)
library(data.table)
library(reshape)
load("./processedData/features/digital_trad_art_MATLABfeatures_sizes.Rda")

##
## HSV STD vs. HSV AVG scatter plot (not using)
##
p = ggplot(dt, aes(x= Value_Mean, y = Value_Std, colour = category)) + geom_point(alpha = .9, size = .5) + theme_bw() 

##
## setup appropriate date conversion
##
dt$date = as.Date(as.POSIXct(dt$uploaded_date, format="%Y-%m-%d  %H:%M:%S"))


##
## HSV AVG vs time (date) scatter plot
##
p = ggplot(dt, aes(x= date, y = Value_Mean, colour = category)) + geom_point(alpha = 0.8, size = 0.7) + theme_bw() + xlab("") + ylab("")  + theme(legend.position="none", text = element_text(size=42))
ggsave("~/Dropbox/DeviantArt/plots/value_mean_per_image_over_dates.png", p)

##
## aggregating HSV features by year
##
res = as.data.frame(dt[,lapply(.SD, FUN = function(x) as.double(mean(x))), by = list(Year, category), .SDcols = c(12, 14, 19, 21, 26, 28)])
res = subset(res, Year != 2000)
res.m = melt(res, id.vars=c("Year", "category"))

#res.m$variable <- factor(res.m$variable, levels = c("Hue_Mean", "Sat_Mean", "Value_Mean", "Hue_Std", "Sat_Std" ,"Value_Std"))
res.m$variable <- factor(res.m$variable, levels = c("Hue Mean", "Hue Std", "Sat Mean", "Sat Std", "Value Mean" ,"Value Std"))

##
## change of HSV AVG aggregate vs years
##
ggplot(subset(res.m, variable %in% c("Hue Mean", "Sat Mean", "Value Mean", "Hue Std", "Sat Std" ,"Value Std")), aes(x = Year, y = value, colour = category, group = category)) + geom_line(size = 3) + 
  facet_wrap(~variable, scale = "free_y", nrow = 3) + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
                                                  strip.text.x = element_text(size = 46), axis.text.x = element_text(size = 46, angle = 90),
                                                  axis.text.y = element_text(size = 46), legend.direction = "horizontal", legend.position = "top",
                                                  legend.title=element_blank(), legend.text = element_text(colour="black", size = 46),
                                                  legend.background = element_rect()) + ylab("") + xlab("") -> p
ggsave("~/Dropbox/DeviantArt/plots/avg_HSV_per_year_V2.pdf", p)



res.date = as.data.frame(dt[,lapply(.SD, FUN = function(x) as.double(mean(x))), by = list(date, category), .SDcols = c(12, 14, 19, 21, 26, 28)])
res.date$date = as.character(res.date$date)



res.m = melt(res.date)

#res.m$variable <- factor(res.m$variable, levels = c("Hue_Mean", "Sat_Mean", "Value_Mean", "Hue_Std", "Sat_Std" ,"Value_Std"))
res.m$variable <- factor(res.m$variable, levels = c("Hue_Mean", "Hue_Std", "Sat_Mean", "Sat_Std", "Value_Mean" ,"Value_Std"))

##
## change of HSV AVG aggregate vs dates
##
ggplot(subset(res.m, variable %in% c("Hue_Mean", "Sat_Mean", "Value_Mean", "Hue_Std", "Sat_Std" ,"Value_Std")), aes(x = date, y = value, colour = category, group = category))  + geom_point() +
  facet_wrap(~variable, scale = "free_y", nrow = 3) + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
                                                            strip.text.x = element_text(size = 18), axis.text.x = element_text(size = 16, angle = 90),
                                                            axis.text.y = element_text(size = 18), legend.direction = "horizontal", legend.position = "top",
                                                            legend.title=element_blank(), legend.text = element_text(colour="black", size = 18),
                                                            legend.background = element_rect()) + ylab("") + xlab("")  + geom_smooth(method = "loess", size = 2) -> p
#ggsave("~/Dropbox/DeviantArt/plots/avg_HSV_per_date.pdf", p, width = 5, height = 10,units = "in", dpi = 300)

##
## splitting by second level categories
##
dt$cat.2 = sapply(dt$filename, FUN = function(x) paste0(strsplit(x, split = "/")[[1]][1], "/",strsplit(x, split = "/")[[1]][2]))

get.full.cat.name = function(x){
  cats =  strsplit(x, split = "/")[[1]]
  sub.cats = ""
  for (i in c(1:length(cats)-1)){
    sub.cats = paste0(sub.cats, cats[i], "/")
  }
  return(sub.cats)
}

dt$full.sub.cats = sapply(dt$filename, get.full.cat.name)
## pick only subset that has "signifcant" amount of examples for each sub-category
dt.reduced = subset(dt, cat.2 %in%  names((table(dt$cat.2)[table(dt$cat.2) > 5e3])))

p = ggplot(dt.reduced, aes(x= Value_Mean, y = Value_Std)) + geom_point(alpha = .1) + facet_wrap(~ cat.2)

res = as.data.frame(dt.reduced[,lapply(.SD, FUN = function(x) as.double(mean(x))), by = list(Year, cat.2), .SDcols = c(12, 14, 19, 21, 26, 28)])
res = subset(res, Year != 2000)
res.m = melt(res, id.vars=c("Year", "cat.2"))

res.m$variable <- factor(res.m$variable, levels = c("Hue_Mean", "Sat_Mean", "Value_Mean", "Hue_Std", "Sat_Std" ,"Value_Std"))

##
## change of HSV stats for sub-cat vs. years
##
p = ggplot(res.m, aes(x = Year, y = value, colour = cat.2, group = cat.2)) + geom_line(size = 1.5) + facet_wrap(~variable, scale = "free_y") + scale_colour_brewer(palette="Set3") #+ theme_bw() 


##
## comparing image dimensions scatter plot
##

ggplot(dt, aes(x = width, y = height, colour = category)) + geom_point(size = 0.5, alpha=0.05) + xlim(0, 1500) + ylim(0,1500) + theme(plot.background = element_blank(), panel.grid.major = element_blank(),
                                                                                                                                     axis.text.x = element_text(size = 8), axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10),
                                                                                                                                     axis.text.y = element_text(size = 9), legend.direction = "horizontal", legend.position = "top",
                                                                                                                                     legend.title=element_blank(), legend.text = element_text(colour="black", size = 9),
                                                                                                                                     legend.background = element_rect()) + guides(colour = guide_legend(override.aes = list(size = 3, alpha = 1)))-> p
#ggsave("~/Dropbox/DeviantArt/plots/width_height.png", p, width = 5, height = 5,units = "in", dpi = 300)


##
## comparing aggregate image dimensions and sizes vs. years
##
library(lubridate)
dt$month.year = paste0(year(dt$date), "-", month(dt$date))
res = dt[,list(num.cats = length(unique(cat.2)), mean.width = mean(width), mean.height = mean(height), mean.bytes = mean(bytes), normalized.size = mean(bytes/(width*height)), mean.area = mean(width*height)), by = list(Year,category)]
ggplot(res, aes(x = Year, y = mean.area, colour = category, group=category)) + geom_point(size=3) + geom_line(size=3)
ggplot(res, aes(x = Year, y = mean.bytes, colour = category, group=category)) + geom_point(size=3) + geom_line(size=3)
ggplot(subset(res, Year != "2000" & Year != "2010"), aes(x = Year, y = normalized.size, colour = category, group=category)) + geom_point(size=3) + geom_line(size=3)
ggplot(res, aes(x = Year, y = normalized.size, colour = category, group=category)) + geom_point(size=3) + geom_line(size=3)


res.date = dt[,list(num.cats = length(unique(cat.2)), mean.width = mean(width), mean.height = mean(height), mean.bytes = mean(bytes), normalized.size = mean(bytes/(width*height)), mean.area = mean(width*height), mean.ratio = mean(height/width)), by = list(date,category)]
ggplot(res.date, aes(x = date, y = mean.ratio, colour = category)) + geom_point(size =.5, alpha = .5) 
##
## comparing number of categories vs. years
##
res.date = dt.reduced[,list(num.sub.cats = length(unique(full.sub.cats)), num.images = length(unique(filename)), mean.width = mean(width)/sd(width), mean.height = mean(height)/sd(height), 
                    mean.bytes = mean(bytes)/sd(bytes), normalized.size = mean(bytes/(width*height))/sd(bytes/(width*height)), mean.area = mean(width*height)/sd(width*height)), by = list(date, cat.2)]

ggplot(res.date, aes(x = as.factor(num.sub.cats), y = num.images))  + geom_point()  + facet_wrap(~cat.2) -> p

get.lm.coeff = function(df){
  model = lm(num.images ~ num.sub.cats, data = df)
  return(model$coefficients)
}
genre.lm = ddply(res.date, .(cat.2), get.lm.coeff)


res.date = dt[,list(num.sub.cats = length(unique(full.sub.cats)), num.images = length(unique(filename)), mean.width = mean(width)/sd(width), mean.height = mean(height)/sd(height), 
                            mean.bytes = mean(bytes)/sd(bytes), normalized.size = mean(bytes/(width*height))/sd(bytes/(width*height)), mean.area = mean(width*height)/sd(width*height)), by = list(date, category)]
ggplot(res.date, aes(x = date, y = num.sub.cats, colour = category, group = category)) + geom_point(alpha=.5)  + geom_smooth(method = "loess", size = 2)
ggplot(res.date, aes(x = date, y = num.images, colour = category, group = category)) + geom_point(alpha=.5)  + geom_smooth(method = "loess", size = 2)
ggplot(res.date, aes(x = date, y = num.images/num.sub.cats, colour = category, group = category)) + geom_point(alpha=.5)  + geom_smooth(method = "loess", size = 2)

res.year = dt[,list(num.sub.cats = length(unique(full.sub.cats)), num.cats = length(unique(cat.2)), mean.width = mean(width)/sd(width), mean.height = mean(height)/sd(height), 
                    mean.bytes = mean(bytes)/sd(bytes), normalized.size = mean(bytes/(width*height))/sd(bytes/(width*height)), mean.area = mean(width*height)/sd(width*height)), by = list(Year, category)]

ggplot(res.year, aes(x = Year, y = num.sub.cats, colour = category, group = category)) + geom_line(size=3)

# str(resdt$cat.2.m = paste0(dt$category, "/", dt$cat.2) ## merge first level cat with sub-cat name
# cat.df = as.data.frame.matrix(table(dt$Year, dt$cat.2.m))
# cat.df$Year = row.names(cat.df)
# cat.m = melt(cat.df, id.vars= "Year")
# cat.m$variable = as.character(cat.m$variable)
# cat.m$category = sapply(cat.m$variable, FUN = function(x) strsplit(x, split = "/")[[1]][1])
# cat.m = subset(cat.m, value != 0)
# cat.m = as.data.table(cat.m)
# cat.m$seq = c(1:nrow(cat.m))
# cat.m$variable = sapply(c(1:nrow(cat.m)), FUN = function(i) strsplit(cat.m$variable[i][[1]], split = cat.m$category[i])[[1]][2])
# 
# ##
# ## scatter plot ofcategory labels vs. years
# ##
# ggplot(cat.m, aes(x = Year, y = seq, colour = category, label = variable)) + geom_text(size = 4)
