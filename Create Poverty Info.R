## poverty allocation data - create and merge in all data 
setwd("~/Documents/LSC Data/Original GAR Export (April 2016)")


pov.all.1<-read.xlsx("Poverty Population Estimates.xlsx", header=T, stringsAsFactors=F,
                 sheetIndex=1)
pov.all.2<-read.xlsx("Poverty Population Estimates.xlsx", header=T,stringsAsFactors=F,
                   sheetIndex=2)
pov.all.1<-pov.all.1[,c("X2015", "X2014", "NA.")]
library(plyr)
pov.all.1<-rename(pov.all.1, c("X2015"="pov_2015", "X2014"= "pov_2014",
"NA."="serv_area"))

pov.all.2<-pov.all.2[,c( "BF",  "X2000.2012",
                       "X2013")]

pov.all.2<-rename(pov.all.2, c("X2000.2012"="pov_2000_2012", "X2013"= "pov_2013",
                               "BF"="serv_area"))
pov.1.complete<-pov.all.1[which(!is.na(pov.all.1$serv_area)),]
pov.2.complete<-pov.all.2[which(!is.na(pov.all.2$serv_area)),]

poverty<-merge(pov.1.complete, pov.2.complete,
by=c("serv_area"),
all=T)

poverty<-poverty[-137,]
poverty$pov_2013<-round(poverty$pov_2013, digits=0)
poverty$pov_2008<-poverty$pov_2000_2012
poverty$pov_2009<-poverty$pov_2000_2012
poverty$pov_2010<-poverty$pov_2000_2012
poverty$pov_2011<-poverty$pov_2000_2012
poverty$pov_2012<-poverty$pov_2000_2012
poverty$pov_2000_2012<-NULL

library(reshape2)
poverty<-melt(poverty)
poverty$year<-gsub("_","",poverty$variable)
poverty$year<-gsub("pov","",poverty$year)
table(poverty$year, poverty$variable)
poverty$variable<-NULL
poverty<-rename(poverty,c("value"="pov_rate_100"))
poverty$year<-as.numeric(poverty$year)
str(poverty)

write.csv(poverty, "poverty_by_year_100.csv")