library(Hmisc)
library(party)
library(stats)
library(ggplot2)
file <- "Laptops 2013 - Sheet 1.csv"
setwd("F:/Downloads")
mydata = read.csv(file)
summary(mydata$Battery.Min)
str(mydata)
#describe(mydata)
#hs <- hist(mydata$Battery.Min)
laptop.ctree <- ctree(Model ~ Battery.Min + Weight 
                     + Price + Screen + Clock + Ram,
                     data=mydata)
print(laptop.ctree)
plot(laptop.ctree)

tmp <- mydata
tmp$Make <- NULL
tmp$Model <- NULL
tmp$Touch <- NULL
tmp$Battery.Hr <- NULL
tmp$Proc <- NULL
show(tmp)

hc <- hclust(dist(tmp), method="ave")
plot(hc, hang= -1, labels=mydata$Model)

kc <- kmeans(tmp, 3)
print(kc)
table(mydata$Model, kc$cluster)

#plot(mydata[c("Battery.Min", "Price", "Weight")], col=kc$cluster)






qplot(data=mydata, x=Battery.Min, y=Price, color=Make, facets= ~Make)

qplot(data=mydata, x=Make, y=Ram, geom="jitter")

points(kc$centers[,c("Battery.Min", "Price")], col=1:3, pch=8, cex=2)

plotmatrix(with(mydata, data.frame(Battery.Min, Price, Weight)))

pairs(mydata)

names(mydata)
str(mydata)





