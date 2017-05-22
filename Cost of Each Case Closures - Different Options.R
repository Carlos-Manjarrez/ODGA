setwd("~/Documents/LSC Data/Original GAR Export (April 2016)")

master<-read.csv("Master_GAR_SA_level_2008_2015.csv", stringsAsFactors=F, header=T)

colnames(master)[grepl("limited", colnames(master),ignore.case=T)]

cost.vars<-master[,c("Basic_Fld_LSC", "total_cc_s" ,"total_limited_s", "total_extended_s",
                     "year", "Organization_Name", "recipient_id", "total_cc", "total_limited", "total_extended")]
library(plyr)

cost.variables<- ddply(cost.vars, c("recipient_id", "year", "Organization_Name"), summarise,
                   total_Basic_Fld_LSC = sum(Basic_Fld_LSC, na.rm=T),
                   total_cc = sum(total_cc, na.rm=T),
                   total_cc_s =sum(total_cc_s, na.rm=T),
                   total_limited = sum(total_limited, na.rm=T), 
                   total_extended=sum(total_extended, na.rm=T), 
                   total_limited_s =sum(total_limited_s, na.rm=T),
                   total_extended_s = sum(total_extended_s, na.rm=T))

# weighted total 1 - average case somewhere between an extended and limited
cost.variables$weight_1_total <- (.5)*cost.variables$total_limited_s + (1.5)*cost.variables$total_extended_s
cost.variables$cost_per_weight_1 <- cost.variables$total_Basic_Fld_LSC / cost.variables$weight_1_total

# weighted total 2 - weight everything in terms of extended
cost.variables$weight_2_total <- (1/3)*cost.variables$total_limited_s + cost.variables$total_extended_s
cost.variables$cost_per_weight2 <- cost.variables$total_Basic_Fld_LSC / cost.variables$weight_2_total

# weighted total 3 - use both staff and pai closures, average case somewhere between extended and limited
cost.variables$weight_3_total <- (.5)*cost.variables$total_limited + (1.5)*cost.variables$total_extended
cost.variables$cost_per_weight_3 <- cost.variables$total_Basic_Fld_LSC / cost.variables$weight_3_total

cost.variables$unweighted_cost_per_cc_s <-cost.variables$total_Basic_Fld_LSC/ cost.variables$total_cc_s
cost.variables$unweighted_cost_per_cc <-cost.variables$total_Basic_Fld_LSC/ cost.variables$total_cc


write.csv(cost.variables, "cost_per_case_calculations.csv")

