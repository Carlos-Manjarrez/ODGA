list.files("/LSC Data/Original GAR Export (April 2016)")
LSC_SA<-read.csv("Master_LSC_Admin.csv", header=T, stringsAsFactors=F)

setwd("~/Documents/LSC Data/Original GAR Export (April 2016)")
library(xlsx)
addresses<-read.xlsx("Salesforce-Grantee names-addresses.xlsx", sheetIndex=1, stringsAsFactors=F)

## subset master file to just have the IDs and the variables of interest
LSC_SA.demos<-read.csv("LSC_SA.demos.csv", stringsAsFactors=F, header=T)

names(LSC_SA.demos)

## rename the column names so they are in ( firstpart_second part format)

colnames(LSC_SA.demos) <- gsub("nat_amr", "native", colnames(LSC_SA.demos))
colnames(LSC_SA.demos) <- gsub("0_17", "017", colnames(LSC_SA.demos))
colnames(LSC_SA.demos) <- gsub("18_59", "1859", colnames(LSC_SA.demos))
colnames(LSC_SA.demos) <- gsub("60_ovr", "60over", colnames(LSC_SA.demos))
colnames(LSC_SA.demos) <- gsub("36_59", "3659", colnames(LSC_SA.demos))
colnames(LSC_SA.demos) <- gsub("18_35", "1835", colnames(LSC_SA.demos))

vars <- names(LSC_SA.demos)[9:38]

head(LSC_SA.demos)
LSC_SA.demos[, vars]
library(reshape)
long.demos <- melt(LSC_SA.demos[, c("recipient_id", "wfta_year", "serv_area" , vars)], measure.vars = vars)

long.demos$first.half <- sapply(strsplit(as.character(long.demos$variable), "_"), function(x) x[1])
long.demos$second.half <- sapply(strsplit(as.character(long.demos$variable), "_"), function(x) x[2])

head(long.demos)

### then merge in address data and serv area type (from addresses and master file)
# write csv when done 