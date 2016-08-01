
###regression analysis 
### looks at recipient/year level
### weight staffing variables by FTE 

setwd("~/Documents/LSC Data/Original GAR Export (April 2016)")


library(readr)
staff<-read_csv("E1ab_Staffing_1999_2015.csv", 
                na = c("", NA, "NULL"),
                col_types = cols(HoursPerWeek = col_double(),
                                 annl_salary = col_double(),
                                 yrs_exp_w_lsc = col_double()) )

staff<-staff[staff$year>=2008,]

staff[which(staff$HoursPerWeek>100), "HoursPerWeek"] <- NA
staff[which(staff$yrs_exp_curr>80), "yrs_exp_curr"] <- NA
staff[which(staff$yrs_exp_w_lsc>49), "yrs_exp_w_lsc"] <- NA
staff[which(staff$yrs_exp_w_lsc>49), "yrs_exp_w_lsc"] <- NA


staff$job_description<-gsub('[[:digit:]]+[[:space:]]+-+[[:space:]]', '', staff$job_description)
staff<-as.data.frame(staff)


colnames(staff)[1]<-"recipient_id"

staff$recip_year<- paste(staff$recipient_id, staff$year)

staff$pct_of_fte<-NA
staff$pct_of_fte<- staff$HoursPerWeek/35
staff$pct_of_fte[staff$pct_of_fte>=1]<- 1

# dataframe with count of FTE job equivalents per recip year 
str(staff)
staff.cases<-tapply((staff$pct_of_fte),
                    list(staff$job_description, staff$recip_year),
                    sum, na.rm=T)

staff.cases<-t(staff.cases)
jobs <- as.data.frame.matrix(staff.cases)
jobs$recip_year<- rownames(jobs)
jobs$year<-gsub('[[:digit:]]+[[:space:]]', '', jobs$recip_year)
jobs$recipient_id<-gsub('[[:blank:]].*$', '' , jobs$recip_year)

str(jobs)
jobs$total_jobs<-rowSums(jobs[, !colnames(jobs) %in%
                                c("year", "recip_year", "recipient_id")], na.rm=T)

jobs$attorneys<-rowSums(jobs[, colnames(jobs) %in%
                               c("Supervising Attorney",
                                 "Staff Attorney", "Managing Attorney", 
                                 "Director of Litigation")], na.rm=T)

jobs$casesupport<-rowSums(jobs[, colnames(jobs) %in%
                                 c("Law Clerk","Paralegal")], na.rm=T)


jobs$casesupport_and_attorneys<-rowSums(jobs[, colnames(jobs) %in%
                                               c("Law Clerk","Paralegal", "Supervising Attorney",
                                                 "Staff Attorney", "Managing Attorney",
                                                 "Director of Litigation")], na.rm=T)

jobs$leadership<-rowSums(jobs[, colnames(jobs) %in%
                                c("Deputy Director",
                                  "Development Director",
                                  "Executive Director")], na.rm=T)

jobs<-rename(jobs, replace=c("PAI Coordinator"="pai.coordinator"))

jobs$office_support<-rowSums(jobs[, colnames(jobs) %in%
                                    c("Administrative Assistant",
                                      "Financial Professional",
                                      "Grants Manager",
                                      "Information Technology Staff",
                                      "Management Professional",
                                      "Other Position",
                                      "pai.coordinator",
                                      "Secretarial/Clerical",
                                      "Senior Aide",
                                      "Training Responsible Person")], na.rm=T)

## bilingual

table(staff$lsc_lang_id)
staff$bilingual<-NA
staff$bilingual[!staff$lsc_lang_id==0]<-1
staff$bilingual[staff$lsc_lang_id==0]<-0

staff.bilingual<-tapply((staff$bilingual),
                        (staff$recip_year),
                        sum, na.rm=T)
write.csv(staff.bilingual, "bilingual.csv")
bilingual<-read.csv("bilingual.csv", stringsAsFactors=F, header=T)
str(bilingual)
bilingual$year<-gsub('[[:digit:]]+[[:space:]]', '', bilingual$X)
bilingual$recipient_id<-gsub('[[:blank:]].*$', '' , bilingual$X)
str(bilingual)
library(plyr)
bilingual<-rename(bilingual, replace=c("x"="bilingual"))
bilingual$X<-NULL

## recipient id, year, want job description,  yrs exp curr, yrs exp w lsc, pct_full_time, 

# label job description type

staff.small<-staff[,c("recipient_id", "year", "yrs_exp_curr", "yrs_exp_prof", 
                      "yrs_exp_w_lsc", "job_description", "pct_full_time", 
                      "pct_of_fte")]

staff.small$type<-NA
staff.small$type[staff.small$job_description=="Paralegal"]<- "Case Support"
staff.small$type[staff.small$job_description=="Law Clerk"]<- "Case Support"


staff.small$type[staff.small$job_description=="Managing Attorney"]<- "Attorney"
staff.small$type[staff.small$job_description=="Supervising Attorney"]<- "Attorney"
staff.small$type[staff.small$job_description=="Staff Attorney"]<- "Attorney"
staff.small$type[staff.small$job_description=="Director of Litigation"]<- "Attorney"

staff.small$type[staff.small$job_description=="Executive Director"]<- "Leadership"
staff.small$type[staff.small$job_description=="Development Director"]<- "Leadership"
staff.small$type[staff.small$job_description=="Deputy Director"]<- "Leadership"

staff.small$type[staff.small$job_description=="Administrative Assistant"]<- "Office Support"
staff.small$type[staff.small$job_description=="Financial Professional"]<- "Office Support"
staff.small$type[staff.small$job_description=="Grants Manager"]<- "Office Support"
staff.small$type[staff.small$job_description=="Information Technology Staff"]<- "Office Support"
staff.small$type[staff.small$job_description=="Management Professional"]<- "Office Support"
staff.small$type[staff.small$job_description=="Other Position"]<- "Office Support"
staff.small$type[staff.small$job_description=="PAI Coordinator"]<- "Office Support"
staff.small$type[staff.small$job_description=="Secretarial/Clerical"]<- "Office Support"
staff.small$type[staff.small$job_description=="Senior Aide"]<- "Office Support"
staff.small$type[staff.small$job_description=="Training Responsible Person"]<- "Office Support"

staff.small$type[is.na(staff.small$type)] <- "Other"


# create mean years of experience variables (weighted for FTE)

staff.small$fte_yrs_exp_curr = (staff.small$pct_of_fte)* (staff.small$yrs_exp_curr)
staff.small$fte_yrs_exp_prof = (staff.small$pct_of_fte)* (staff.small$yrs_exp_prof)
staff.small$fte_yrs_exp_w_lsc = (staff.small$pct_of_fte)* (staff.small$yrs_exp_w_lsc)

staff.small<-staff.small[, c("year", "pct_of_fte", "fte_yrs_exp_curr", 
                             "fte_yrs_exp_prof", 
                             "fte_yrs_exp_w_lsc", "recipient_id", "type"
)]

str(staff.small)

sum.vars<-aggregate(staff.small[, c("fte_yrs_exp_curr",
                                    "pct_of_fte", 
                                    "fte_yrs_exp_w_lsc",
                                    "fte_yrs_exp_prof", 
                                    "fte_yrs_exp_curr")], by=list(staff.small$recipient_id, staff.small$year,
                                                                  staff.small$type), FUN=sum, na.rm=TRUE)
str(sum.vars)


sum.vars$mean_fte_yrs_exp_curr <- (sum.vars$fte_yrs_exp_curr)/(sum.vars$pct_of_fte)
sum.vars$mean_fte_yrs_exp_w_lsc <- (sum.vars$fte_yrs_exp_w_lsc)/(sum.vars$pct_of_fte)
sum.vars$mean_fte_yrs_exp_prof <- (sum.vars$fte_yrs_exp_prof)/(sum.vars$pct_of_fte)

sum.vars<-sum.vars[,c("Group.1","Group.2", "Group.3",
                      "pct_of_fte", "mean_fte_yrs_exp_curr", 
                      "mean_fte_yrs_exp_w_lsc",
                      "mean_fte_yrs_exp_prof", "fte_yrs_exp_prof" )]

sum.vars <- rename(sum.vars, replace = c("Group.1" = "recipient_id",
                                         "Group.2"="year",
                                         "Group.3" = "job_type", "fte_yrs_exp_prof"="sum_yrs_exp"))

### create years professional experience mean variable
names(sum.vars)
years.exp<-sum.vars[,c("recipient_id", "job_type", "year", "mean_fte_yrs_exp_prof")]
str(years.exp)
library(reshape2)
years.exp <- dcast(years.exp, recipient_id+year ~ job_type)
str(years.exp)

years.exp <- rename(years.exp, replace = c("Attorney"="attorney_prof_exp", 
                                           "Case Support"= "case_support_prof_exp", 
                                           "Leadership"="leadership_prof_exp",
                                           "Office Support"="office_support_prof_exp"))


### create years professional experience sum variable
names(sum.vars)
sum.exp<-sum.vars[,c("recipient_id", "job_type", "year", "sum_yrs_exp")]
str(sum.exp)
library(reshape2)
sum.exp <- dcast(sum.exp, recipient_id+year ~ job_type)
str(sum.exp)

sum.exp <- rename(sum.exp, replace = c("Attorney"="attorney_sum_exp", 
                                       "Case Support"= "case_support_sum_exp", 
                                       "Leadership"="leadership_sum_exp",
                                       "Office Support"="office_support_sum_exp"))

#### years experience with LSC sum variable
years.lsc<-sum.vars[,c("recipient_id", "job_type", "year", "mean_fte_yrs_exp_w_lsc")]
str(years.lsc)
library(reshape2)

years.lsc <- dcast(years.lsc, recipient_id+year ~ job_type)

years.lsc <- rename(years.lsc, replace = c("Attorney"="attorney_lsc_exp", 
                                           "Case Support"= "case_support_lsc_exp", 
                                           "Leadership"="leadership_lsc_exp",
                                           "Office Support"="office_support_lsc_exp"))


### everything we need from LSC Master on recipient and year level 


library(plyr)

LSC_SA<-read.csv("Master_GAR_SA_level_2008_2015.csv", stringsAsFactors=F, header=T)


LSC_SA$total_pai_aap<-rowSums(LSC_SA[, colnames(LSC_SA) %in%
                                       c("Pro_Bono_AAP"   ,    "Pro_Bono_S_AAP"   ,  "Judicare_AAP"  ,
                                         "Contr_Vol_AAP"   ,  "Contr_Indv_AAP"   ,
                                         "Co_Council_AAP"  ,   "LRS_AAP"      ,      "Other_AAP" ,
                                         "CoCounsel_PB_AAP"  , "CoCounsel_Comp_AAP",
                                         "Other_PB_AAP"     ,  "Other_Comp_AAP"  )], na.rm=T)

LSC_SA$total_pai_aar<-rowSums(LSC_SA[, colnames(LSC_SA) %in%
                                       c("Pro_Bono_AAR"   ,    "Pro_Bono_S_AAR"   ,  "Judicare_AAR"  ,
                                         "Contr_Vol_AAR"   ,  "Contr_Indv_AAR"   ,
                                         "Co_Council_AAR"  ,   "LRS_AAR"      ,      "Other_AAR" ,
                                         "CoCounsel_PB_AAR"  , "CoCounsel_Comp_AAR",
                                         "Other_PB_AAR"     ,  "Other_Comp_AAR"  )], na.rm=T)


names(LSC_SA)
recipient.level <- ddply(LSC_SA, c("recipient_id", "year"), summarise,
                         total_funding_LSC= sum(total_funding_LSC, na.rm=T),
                         total_pai_aar= sum(total_pai_aar, na.rm=T),
                         total_pai_aap = sum(total_pai_aap, na.rm=T), 
                         total_funding_NLSC = sum(total_funding_NLSC, na.rm=T),
                         total_limited_p = sum(total_limited_p, na.rm=T),
                         total_limited_s = sum(total_limited_s, na.rm=T),
                         total_extended_s = sum(total_extended_s, na.rm=T),
                         total_extended_p = sum(total_extended_p, na.rm=T),
                         total_cc = sum(total_cc, na.rm=T),
                         total_cc_s =  sum(total_cc_s, na.rm=T),
                         total_cc_p =  sum(total_cc_p, na.rm=T),
                         total_contested =sum(total_contested, na.rm=T), 
                         total_appeals =sum(total_Ic, na.rm=T),
                         total_contested_s=sum( total_contested_s, na.rm=T),
                         total_contested_p=sum(total_contested_p, na.rm=T),
                         total_appeals_s=sum(total_Ic_s, na.rm=T), 
                         total_appeals_p=sum(total_Ic_p, na.rm=T),
                         total_Traing_Staff =sum(total_Traing_Staff, na.rm=T), 
                         total_Lawyers_expenses =sum(total_Lawyers, na.rm=T), 
                         total_Personnel_expenses =sum(total_Personnel, na.rm=T), 
                         total_Empl_Benefits =sum(total_Empl_Benefits, na.rm=T),
                         groups= sum(groups, na.rm=T))

str(recipient.level)

### merge together separate data points into one file 
str(jobs)
jobs<-jobs[, c("recipient_id", "year", "total_jobs", "attorneys", "casesupport", 
               "casesupport_and_attorneys", "leadership", "PAI Coordinator",
               "office_support", "Paralegal") ]
str(jobs)
regression.data<-merge(recipient.level, jobs, by.x=c("recipient_id", "year"),
                       by.y=c("recipient_id", "year"), all.x=T)

regression.data<-merge(regression.data, years.exp, by.x=c("recipient_id", "year"),
                       by.y=c("recipient_id", "year"), all.x=T)
regression.data<-merge(regression.data, sum.exp, by.x=c("recipient_id", "year"),
                       by.y=c("recipient_id", "year"), all.x=T)

regression.data<-merge(regression.data, years.lsc, by.x=c("recipient_id", "year"),
                       by.y=c("recipient_id", "year"), all.x=T)

library(xlsx)
addresses<-read.xlsx("Salesforce-Grantee names-addresses.xlsx", sheetIndex=1, stringsAsFactors=F)

regression.data<-merge(regression.data, addresses, 
                       by.x=c("recipient_id"), by.y=c("Recipient.ID"), 
                       all.x=T)

regression.data<-merge(regression.data, bilingual, 
                       by.x=c("recipient_id", "year"), 
                       by.y=c("recipient_id", "year"), 
                       all.x=T)


### standardize with complete cases across models to better compare models
#varlist <- c("total.staff.cc", "funding", "attorney_lsc_exp", "attorneys",
# "Primary.State.Province", "recipient_id", "wfta_year")
#regression.data <- regression.data[complete.cases(regression.data[, varlist]), ]

str(regression.data)

regression.data$total_funding <- rowSums(regression.data[, colnames(regression.data) %in%
                                                           c("total_funding_LSC", "total_funding_NLSC")], na.rm=T)

regression.data$prop_lsc<- (regression.data$total_funding_LSC/ regression.data$total_funding)


### geographic information 

# look at regions

regions<-read.csv("stateregioncodes.csv", stringsAsFactors=F, header=T)
states<-read.csv("statenames.csv", stringsAsFactors=F, header=T)
str(states)
str(regions)

state.data<-merge(regions, states, by.x=c("State"), 
                  by.y=c("State"), all=T)
regression.data<-merge(regression.data, state.data, 
                       by.x=c("Primary.State.Province"), 
                       by.y=c("Abbreviation"), all.x=T)


#### geocoded info 

#geocodes<-read.csv("geocode.csv", stringsAsFactors=F, header=T)
#str(geocodes)
#geocodes<-geocodes[,c("ID", "MatchScore", "Latitude", "Longitude", "CensusCountyFips", "Address", "City",
#                     "State", "Zip", "CensusBlock", "CensusBlockGroup", "CensusTract")]
#

#library("RJSONIO")
#library("RCurl")

# FCC's Census Block Conversions API
# http://www.fcc.gov/developers/census-block-conversions-api

latlong2fips <- function(latitude, longitude) {
  url <- "http://data.fcc.gov/api/block/find?format=json&latitude=%f&longitude=%f"
  url <- sprintf(url, latitude, longitude)
  json <- RCurl::getURL(url)
  json <- RJSONIO::fromJSON(json)
  as.character(json$County['FIPS'])
}

# Orange County
#latlong2fips(latitude=28.35975, longitude=-81.421988)

#geocodes$fips<-NA
#for (i in 1:nrow(geocodes)){
# fips<-latlong2fips(latitude=geocodes$Latitude[i], longitude=geocodes$Longitude[i])
# geocodes$fips[i]<-fips
# print(fips)
#}

#table(is.na(geocodes$fips))

#list.files()
#library(xlsx)
#urban<-read.xlsx("ruralurbancodes2013 (1).xls", sheetIndex=1, header=T, stringsAsFactors=F)
#str(urban)

#urban<-urban[,c("FIPS", "County_Name", "Population_2010", "RUCC_2013", "Description")]

#geocodes<-merge(geocodes, urban, by.x=c("fips"), by.y=c("FIPS"), all.x=T)
#str(geocodes)

#write.csv(geocodes, "geocodes_w_fips.csv")

geocodes<- read.csv("geocodes_w_fips.csv", stringsAsFactors=F, header=T)

geocodes<-geocodes[,c("ID", "fips", "RUCC_2013", "County_Name")]


regression.data<-merge(regression.data, geocodes, 
                       by.x=c("recipient_id"), by.y=c("ID"), all.x=T)

str(regression.data)

#str(geocodes)

library(nlme)
library(multilevel)

### add in additional information 

additional<-read.csv("AdditionalInfo.csv",stringsAsFactors=F, header=T)
str(additional)
table(additional$wfta_year)

additional$total_served<- additional$adults_in_households + additional$children_in_households

regression.data<- merge(regression.data, additional, by.x=c("recipient_id", "year"), 
                        by.y=c("recipient_id", "wfta_year"), all.x=T)
str(regression.data)

### real NSLC CASES
regression.data$real_nlsc_cases<- regression.data$nlsc_cases
regression.data$real_nlsc_cases[!regression.data$figure=="Actual Count"]<- NA

### cases per attorney
regression.data$cc.per.attorney <- regression.data$total_cc_s/regression.data$attorneys
regression.data$nlsc.per.attorney <- regression.data$real_nlsc_cases/regression.data$attorneys
regression.data$contested.per.attorney <- (regression.data$total_contested_s)/(regression.data$attorneys)

### recip ID as factor
regression.data$recipient_id<-as.factor(regression.data$recipient_id)


### add in race data
list.files()
race<-read.xlsx("race_for_2015.xlsx"  , stringsAsFactors=F, header=T, sheetIndex=1)

### Regression Analysis

regression_data_2015<-subset(regression.data, regression.data$year==2015)

regression_data_2015<-merge(regression_data_2015, race, by.x=c("recipient_id"),
                            by.y=c("recipient_id"), all.x=T)

regression_data_2015$prop_minority<- (regression_data_2015$black+ regression_data_2015$asian+
                                        regression_data_2015$hisp)/regression_data_2015$total

regression_data_2015$prop_native<- (regression_data_2015$native)/regression_data_2015$total
table(is.na(race$recipient_id))



model1<-lm(log(total_cc_s+1)~ log(total_funding_LSC+1), data=regression_data_2015)
summary(model1)


model2<-lm(log(total_cc_s+1)~ log(total_funding_LSC+1) +log(attorneys+1)+
             attorney_lsc_exp+ log(attorneys+1), data=regression_data_2015)

summary(model2)


### updated model - used in write up of regression analysis
names(regression_data_2015)

model3<-lm(log(total_cc_s+1)~ log(total_funding_LSC+1) +log(attorneys+1)+ 
             log(casesupport +1) + log(attorney_prof_exp+1)+ log( case_support_prof_exp+1)+
             +RUCC_2013+ log(prop_lsc+1) + 
             log(real_nlsc_cases +1), 
           data=regression_data_2015) 

summary(model3)


model3.1<-lm(log(total_cc_s+1)~ log(total_funding_LSC+1) +log(attorneys+1)+ 
               log(casesupport +1) + log(attorney_prof_exp+1)+ log( case_support_prof_exp+1)+
               +RUCC_2013+ log(prop_lsc+1) + log(prop_minority+1) +
               log(real_nlsc_cases +1) + log(prop_native+1), 
             data=regression_data_2015) 

summary(model3.1)



### weighted set as 3 times
regression_data_2015$weighted_cases <- (.5)*regression_data_2015$total_limited_s + (1.5)*regression_data_2015$total_extended_s



model4<-lm(log(weighted_cases+1)~ log(total_funding_LSC+1) +log(attorneys+1)+ 
             log(casesupport +1) + log(attorney_prof_exp+1)+ log( case_support_prof_exp+1)+
             +RUCC_2013+ log(prop_lsc+1) + log(prop_minority +1) + log(prop_native +1)+ 
             log(real_nlsc_cases +1), 
           data=regression_data_2015) 

summary(model4)




### correlation matrix variables: 

correlation_data <-regression_data_2015[, c("total_cc_s", "total_funding_LSC", "attorneys",
                                            "attorney_prof_exp" , "casesupport",
                                            "case_support_prof_exp",
                                            "RUCC_2013" ,"prop_lsc", "real_nlsc_cases", "prop_minority",
                                            "prop_native"
)]


correlation_data$total_cc_s<-log(correlation_data$total_cc_s+1)
correlation_data$total_funding_LSC<-log(correlation_data$total_funding_LSC+1)
correlation_data$attorneys<-log(correlation_data$attorneys+1)
correlation_data$casesupport<-log(correlation_data$casesupport+1)
correlation_data$attorney_prof_exp<-log(correlation_data$attorney_prof_exp+1)
correlation_data$case_support_prof_exp<-log(correlation_data$case_support_prof_exp+1)
correlation_data$prop_lsc<-log(correlation_data$prop_lsc+1)
correlation_data$real_nlsc_cases<-log(correlation_data$real_nlsc_cases+1)
correlation_data$prop_native<-log(correlation_data$prop_native+1)
correlation_data$prop_minority<-log(correlation_data$prop_minority+1)





str(correlation_data)



corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

table<-corstarsl(correlation_data)

corstarsl(regression_data_2015)


write.csv(table, "table.csv")








