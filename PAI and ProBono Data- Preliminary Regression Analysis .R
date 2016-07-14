#### PAI ANALYSIS
setwd("~/Documents/LSC Data/Original GAR Export (April 2016)")

library(xlsx)
addresses<-read.xlsx("Salesforce-Grantee names-addresses.xlsx", sheetIndex=1, stringsAsFactors=F)

#### create dataframe with info. on whether orgs. have a PAI Coordinator or not

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

staff$FTE_ratio<-NA
staff$FTE_ratio<- staff$HoursPerWeek/35
staff$FTE_ratio[staff$FTE_ratio>=1]<- 1


staff$pai_cord_attorney<-NA
staff$pai_cord_attorney[staff$job_description=="PAI Coordinator" & staff$attorney==1]<-1
staff$pai_cord_attorney[is.na(staff$pai_cord_attorney)]<-0

staff$has_pai<-NA
staff$has_pai[staff$job_description=="PAI Coordinator"] <- 1
staff$has_pai[is.na(staff$has_pai)]<-0


staff.pai <- ddply(staff, c("recipient_id", "year"), summarise,

                         pai.count= sum(has_pai, na.rm=T),
                        pai.attorney =sum(pai_cord_attorney, na.rm=T))

staff.pai$has_pai<-NA
staff.pai$has_pai[staff.pai$pai.count>=1] <-1
staff.pai$has_pai[is.na(staff.pai$has_pai)]<-0


staff.pai$has_pai_attorney<-NA
staff.pai$has_pai_attorney[staff.pai$pai.attorney>=1] <-1
staff.pai$has_pai_attorney[is.na(staff.pai$has_pai_attorney)]<-0
str(staff.pai)

View(sample(staff.pai))

### subset to just the variables for has pai, has pai attorney

staff.pai<-staff.pai[,c("recipient_id", "year", "has_pai", "has_pai_attorney")]

master<-read.csv("Master_GAR_SA_level_2008_2015.csv" , stringsAsFactors=F, header=T)


master$total_probono_cc<-rowSums(master[, colnames(master) %in%
                               c("Pro_Bono_CC" , "Pro_Bono_S_CC", "CoCounsel_PB_CC", "Other_PB_CC")], na.rm=T)

master$total_probono_aap<-rowSums(master[, colnames(master) %in%
                    c("Pro_Bono_AAP" , "Pro_Bono_S_AAP", "CoCounsel_PB_AAP", "Other_PB_AAP")], na.rm=T)


master$total_probono_aar<-rowSums(master[, colnames(master) %in%
                                           c("Pro_Bono_AAR" , "Pro_Bono_S_AAR", "CoCounsel_PB_AAR", "Other_PB_AAR")], na.rm=T)

master$total_probono_cr<-rowSums(master[, colnames(master) %in%
                                           c("Pro_Bono_CR" , "Pro_Bono_S_CR", "CoCounsel_PB_CR", "Other_PB_CR")], na.rm=T)


master$total_compensated_cc<-rowSums(master[, colnames(master) %in%
                                                    c("Judicare_CC", "Contr_Vol_CC","Contr_Indv_CC",
                                                      "CoCounsel_Comp_CC","Other_Comp_CC")], na.rm=T)


library(plyr)


master_bf<-subset(master, master$serv_area_type=="BF")
master_rl <- ddply(master_bf, c("recipient_id", "year"), summarise,
                   total_compensated_cc =sum(total_compensated_cc, na.rm=T),
                    total_probono_cc= sum(total_probono_cc, na.rm=T),
                    total_probono_aap =sum(total_probono_aap, na.rm=T),
                    total_probono_aar =sum(total_probono_aar, na.rm=T),
                    total_contested_p = sum(total_contested_p, na.rm=T),
                    total_contested_s = sum(total_contested_s, na.rm=T),
                    total_probono_cr =sum(total_probono_cr, na.rm=T),
                    total_extended_s =sum(total_extended_s, na.rm=T),
                    total_extended_p =sum(total_extended_p, na.rm=T),
                    total_limited_s =sum(total_limited_s, na.rm=T),
                    total_limited_p =sum(total_limited_p, na.rm=T),
                    total_cc_p =sum(total_cc_p, na.rm=T),
                    total_cc_s =sum(total_cc_s, na.rm=T),
                    total_cc =sum(total_cc, na.rm=T),
                    total_funding=sum(total_funding, na.rm=T),
                    total_funding_LSC=sum(total_funding_LSC, na.rm=T),
                    total_funding_NLSC=sum(total_funding_NLSC, na.rm=T),
                    Bar_Grants_NLSC=sum(Bar_Grants_NLSC, na.rm=T),
                    Basic_Fld_LSC =sum(Basic_Fld_LSC, na.rm=T),
                    Corp_Indiv_Contributions_NLSC=sum(Corp_Indiv_Contributions_NLSC, na.rm=T),
                    groups=sum(groups, na.rm=T))
                    

probono.data<-merge(master_rl, staff.pai, by.x=c("recipient_id", "year"),
                    by.y=c("recipient_id", "year"), all.x=T)


str(probono.data)


probono.data<-merge(probono.data, addresses, 
                       by.x=c("recipient_id"), by.y=c("Recipient.ID"), 
                       all.x=T)

probono.data$pai_funding<- probono.data$Basic_Fld_LSC* (.125)

### Subset Data - Just 2015

probono.data_2015<-subset(probono.data, probono.data$year==2015,)

### Correlation Calculations
cor(probono.data$total_probono_aar,  probono.data$total_probono_cc, use = "pairwise.complete.obs",
    method = c("pearson") )

# Regression Analysis

library(lmerTest)

### 2015 Analysis Using probono.data_2015 

### Used natural log of case closures, funding etc. because of skew. 
names(probono.data_2015)
hist(probono.data_2015$groups)

## Basic Model

model1_2015<-lm(log(total_probono_cc+1)~ log(total_funding+1)+log(Bar_Grants_NLSC +1)+
                  log(Corp_Indiv_Contributions_NLSC+1)+ 
             log(total_probono_aap+1)+ as.factor(has_pai) +
                log(total_extended_s+1), data=probono.data_2015)


summary(model1_2015)

### look at PAI Coordinator info

table(has_pai = probono.data_2015$has_pai, has_pai_attorney = probono.data_2015$has_pai_attorney)

tapply(probono.data_2015$total_probono_cc, probono.data_2015$has_pai_attorney, mean, na.rm=T)

#### MULTILEVEL MODEL - Data for Years 2008-2015


multilevel_1<-lmer(log(total_probono_cc+1)~ log(total_funding+1)+log(Bar_Grants_NLSC +1)+
                  log(Corp_Indiv_Contributions_NLSC+1)+ 
                  log(total_probono_aap+1)+ as.factor(has_pai) + as.factor(has_pai_attorney)+
                  log(total_extended_s+1)+  (1|recipient_id) +year, data=probono.data)

summary(multilevel_1)




