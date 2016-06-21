setwd("~/Documents/LSC Data/Original GAR Export (April 2016)")
#rm(list = ls())


## Read all data sets
demos<-read.csv("ClientDemographics.csv", stringsAsFactors=F, header=T)
CSR049<-read.csv("CSR_2008_2014_0_49.csv",stringsAsFactors=F, header=T)
CSR5099<-read.csv("CSR_2008_2014_50_99.csv",stringsAsFactors=F, header=T)
G3<-read.csv("G3_0_99_all_years.csv", header=T, stringsAsFactors=F)
PAI<-read.csv("PAIComponents_ActualOnly.csv", header=T, stringsAsFactors=F)
offices<-read.csv("Offices.csv", stringsAsFactors=F, header=T)
opencases<-read.csv("OpenCases.csv",stringsAsFactors=F, header=T)
revenue<-read.csv("Revenues.csv",stringsAsFactors=F, header=T)
selfinspection<-read.csv("SelfInspection.csv", stringsAsFactors=F,header=T)
expenses<-read.csv("Expenses.csv", stringsAsFactors=F,header=T)
otherservices<-read.csv("OtherServices.csv", stringsAsFactors=F,header=T)

## just 2008+ data (check to make sure wfta year and year are same)
demos<-subset(demos, demos$wfta_year>=2008)
offices<-subset(offices, offices$year>=2008)
opencases<-subset(opencases, opencases$year>=2008)
PAI<-subset(PAI,PAI$wfta_year>=2008)
revenue<-subset(revenue, revenue$wfta_year>=2007)
selfinspection<-subset(selfinspection, selfinspection$year>=2008)
expenses<-subset(expenses, expenses$wfta_year>=2008)
G3<-subset(G3, G3$year>=2008)


## change NULL to NA, convert all but first 3-4 rows to numbers
clean<-function(x){
  
  x[x=='NULL']<-NA
  x[-1:-3] <- sapply(x[-1:-3], as.numeric)
  return(x)
}

PAI<-clean(PAI)
CSR049<-clean(CSR049)
CSR5099<-clean(CSR5099)
demos<-clean(demos)
opencases<-clean(opencases)
#PAI<-clean(PAI)

revenue[revenue=='NULL']<-NA
revenue[-1:-4] <- sapply(revenue[-1:-4], as.numeric)
offices[offices=="NULL"]<-NA


## need to trim recipient_id, remove spaces
trim <- function( x ) {
  gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}

PAI$serv_area<-trim(PAI$serv_area)
CSR049$serv_area<-trim(CSR049$serv_area)
CSR5099$serv_area<-trim(CSR5099$serv_area)
demos$serv_area<-trim(demos$serv_area)
opencases$serv_area<-trim(opencases$serv_area)
revenue$serv_area<-trim(revenue$serv_area)
G3$serv_area<-trim(G3$serv_area)
expenses$serv_area<-trim(expenses$serv_area)

### note- year in CSR049, CSR5099, opencases
### wfta_year in demos, PAI, revenue

LSC_SA<-merge(demos, revenue, by=c("recipient_id",
                                      "wfta_year","serv_area"),all=T)

LSC_SA<-merge(LSC_SA, opencases, 
              by.x=c("recipient_id",
                   "wfta_year","serv_area"),
              by.y=c("recipient_id",
               "year","serv_area"),all=T)


LSC_SA<-merge(LSC_SA, PAI, 
      by.x=c("recipient_id",
       "wfta_year","serv_area"),
      by.y=c("recipient_id",
       "wfta_year","serv_area"),all=T)

All.CSR<-merge(CSR049, CSR5099, 
      by.x=c("recipient_id",
                  "year","serv_area"),
          by.y=c("recipient_id",
               "year","serv_area"),all=T)

colnames(LSC_SA)[colnames(LSC_SA)=="total"] <- "total_funding"

## Correct an error in the GAR reporting - one value should have been 9 not 11
## note - need to check that these extra two should not have been added to male
which(LSC_SA$recipient_id=="805240")
LSC_SA[1386, c("groups")] = 9

## read in recipient address data

library(xlsx)
addresses<-read.xlsx("Salesforce-Grantee names-addresses.xlsx", sheetIndex=1, stringsAsFactors=F)

LSC_SA<-merge(LSC_SA, addresses, 
              by.x=c("recipient_id"
                     ),
              by.y=c("Recipient.ID"
                     ),all.x=T)

##add in variables for total case closures 


original.var<-colnames(G3)[colnames(G3) %in% colnames(All.CSR)]

subsetvars<-c(original.var, "CounselAdvice_Total",
              "LimitedAction_Total",
              "NegotSetl_wo_Lit_Total",
              "NegotSetl_with_Lit_Total",
              "AgencyDecision_Total",
              "CourtDec_Uncontested_Total",
              "CourtDec_Contested_Total",
              "CourtDec_Appeals_Total",
              "Other_Closure_Total",
              "Extensive_Services_Total")
addedcc<-G3[, subsetvars]
addedcc[addedcc=="NULL"]<-NA

LSC_SA<-merge(LSC_SA, addedcc, 
          by.x=c("recipient_id", "wfta_year", "serv_area"
            ),
       by.y=c("recipient_id", "year", "serv_area"
         ),all.x=T)


### See "Create Poverty Info" R Script that organizes the Poverty data"


poverty<-read.csv("poverty_by_year_100.csv", stringsAsFactors=F)

LSC_SA<-merge(LSC_SA, poverty,
              by.x=c("serv_area", "wfta_year"), 
              by.y=c("serv_area", "year"),
              all.x=T)


### get rid of NA in service area types, check they're all labeled accurately
### from now on, we can use serv_area_type to subset by BF, migrant etc. 
#colnames(LSC_SA)[grepl("type", colnames(LSC_SA),ignore.case=T)]


LSC_SA$serv_area_type[is.na(LSC_SA$serv_area_type)] <-  "native"
table(LSC_SA$serv_area_type)

#View(LSC_SA[LSC_SA$serv_area_type=="MW",c("serv_area_type", "serv_area")])

LSC_SA[LSC_SA$serv_area=="AZ-2",c("serv_area_type")]= "BF"
LSC_SA[LSC_SA$serv_area=="MWY",c("serv_area_type")]= "MW"
LSC_SA[LSC_SA$serv_area=="WY-4",c("serv_area_type")]= "BF"
LSC_SA[LSC_SA$serv_area=="NM-1",c("serv_area_type")]= "BF"

### expenses 
expenses$serv_area_type<-NULL
expenses[-1:-4] <- sapply(expenses[-1:-4], as.numeric)

library(plyr)
colnames(expenses)[(colnames(expenses) %in% colnames(LSC_SA))]
expenses<-rename(expenses,c( "Other"="expenses.Other", "Other_Total"="expenses.Other_Total",
                             "Grand_Total"="expenses.Grand_Total", "Lawyers"="expenses.Lawyers",
                             "Paralegals"= "expenses.Paralegals", "Other_Staff"="expenses.Other_Staff",
                             "LSC_Total"="expenses.LSC_Total"
))

LSC_SA<-merge(LSC_SA, expenses, by.x=c("wfta_year", "serv_area", "recipient_id"),
              by.y=c("wfta_year", "serv_area", "recipient_id"), all=T)

table(duplicated(LSC_SA[, c("recipient_id", "wfta_year", "serv_area")]))


## Look for column names
colnames(LSC_SA)[grepl("serv_area_type", colnames(LSC_SA),ignore.case=T)]


LSC_SA$contested<-rowSums(LSC_SA[,colnames(LSC_SA) %in%
                                           c("NegotSetl_with_Lit_Total",
                                             "AgencyDecision_Total",
                                             "CourtDec_Contested_Total",
                                             "CourtDec_Appeals_Total")], na.rm=T)
LSC_SA$extended<- rowSums(LSC_SA[,colnames(LSC_SA) %in%
                                           c("NegotSetl_wo_Lit_Total",
                                             "NegotSetl_with_Lit_Total",
                                             "AgencyDecision_Total",
                                             "CourtDec_Uncontested_Total",
                                             "CourtDec_Contested_Total",
                                             "CourtDec_Appeals_Total",
                                             "Other_Closure_Total",
                                             "Extensive_Services_Total")],na.rm=T)

LSC_SA$total_cc_calc<- rowSums(LSC_SA[,colnames(LSC_SA) %in%
                                                c("CounselAdvice_Total",
                                                  "LimitedAction_Total",
                                                  "NegotSetl_wo_Lit_Total",
                                                  "NegotSetl_with_Lit_Total",
                                                  "AgencyDecision_Total",
                                                  "CourtDec_Uncontested_Total",
                                                  "CourtDec_Contested_Total",
                                                  "CourtDec_Appeals_Total",
                                                  "Other_Closure_Total",
                                                  "Extensive_Services_Total")],na.rm=T)


colnames(revenue)[grepl("ot", colnames(revenue),ignore.case=T)]

## Aggregate vaiables


LSC_SA$total_education_cc<- rowSums(LSC_SA[,
                                   grepl(".*_1[[:digit:]]",
                                         colnames(LSC_SA),ignore.case=T) 
                                   ],na.rm=T)

LSC_SA$total_employment_cc<- rowSums(LSC_SA[,
                                    grepl(".*_2[[:digit:]]",
                                          colnames(LSC_SA),ignore.case=T) 
                                    ],na.rm=T)
LSC_SA$total_family_cc<- rowSums(LSC_SA[,
                                grepl(".*_3[[:digit:]]",
                                      colnames(LSC_SA),ignore.case=T) 
                                ],na.rm=T)
LSC_SA$total_juvenile_cc<- rowSums(LSC_SA[,
                                  grepl(".*_4[[:digit:]]",
                                        colnames(LSC_SA),ignore.case=T) 
                                  ],na.rm=T)
LSC_SA$total_health_cc<- rowSums(LSC_SA[,
                                grepl(".*_5[[:digit:]]",
                                      colnames(LSC_SA),ignore.case=T) 
                                ],na.rm=T)

LSC_SA$total_housing_cc<- rowSums(LSC_SA[,
                                 grepl(".*_6[[:digit:]]",
                                       colnames(LSC_SA),ignore.case=T) 
                                 ],na.rm=T)
LSC_SA$total_income_cc<- rowSums(LSC_SA[,
                                grepl(".*_7[[:digit:]]",
                                      colnames(LSC_SA),ignore.case=T) 
                                ],na.rm=T)
LSC_SA$total_individual_cc<- rowSums(LSC_SA[,
                                    grepl(".*_8[[:digit:]]",
                                          colnames(LSC_SA),ignore.case=T) 
                                    ],na.rm=T)
LSC_SA$total_misc_cc<- rowSums(LSC_SA[,
                              grepl(".*_9[[:digit:]]",
                                    colnames(LSC_SA),ignore.case=T) 
                              ],na.rm=T)

LSC_SA$total_consumer<- rowSums(LSC_SA[,
                               grepl("[[:alpha:]]+_[[:digit:]][[:alpha:]]",
                                     colnames(LSC_SA),ignore.case=T) 
                               ],na.rm=T)


LSC_SA$domviol_custody_cc<- rowSums(LSC_SA[,
                      grepl("[[:alpha:]]+_31|[[:alpha:]]+_37",
                            colnames(LSC_SA),ignore.case=T) 
                      ],na.rm=T)


LSC_SA$trafficking_all_staff<- rowSums(LSC_SA[,
                                      grepl(".*_86s",
                                            colnames(LSC_SA),ignore.case=T) 
                                      ],na.rm=T)
LSC_SA$trafficking_all_pai<- rowSums(LSC_SA[,
                                    grepl(".*_86p",
                                          colnames(LSC_SA),ignore.case=T) 
                                    ],na.rm=T)

LSC_SA$trafficking_all<- rowSums(LSC_SA[,colnames(LSC_SA) %in%
                                  c("trafficking_all_pai",
                                    "trafficking_all_staff"
                                  )],na.rm=T)


## create total cc, contested, limited for staff and PAI

LSC_SA$all_limited<- rowSums(LSC_SA[,colnames(LSC_SA) %in%
                                      c("LimitedAction_Total",
                                        "CounselAdvice_Total")],na.rm=T)

LSC_SA$counsel.advice_staff<- rowSums(LSC_SA[,
                                             grepl("^A_[[:digit:]]+s",
                                                   colnames(LSC_SA),ignore.case=T) 
                                             ],na.rm=T)
LSC_SA$counsel.advice_pai<- rowSums(LSC_SA[,
                                           grepl("^A_[[:digit:]]+p",
                                                 colnames(LSC_SA),ignore.case=T) 
                                           ],na.rm=T)

LSC_SA$limited.action_staff<- rowSums(LSC_SA[,
                                             grepl("^B_[[:digit:]]+s",
                                                   colnames(LSC_SA),ignore.case=T) 
                                             ],na.rm=T)
LSC_SA$limited.action_pai<- rowSums(LSC_SA[,
                                           grepl("^B_[[:digit:]]+p",
                                                 colnames(LSC_SA),ignore.case=T) 
                                           ],na.rm=T)

# F closing code

LSC_SA$nego.sett.wo.lit_staff<- rowSums(LSC_SA[,
                                               grepl("^F_[[:digit:]]+s",
                                                     colnames(LSC_SA),ignore.case=T) 
                                               ],na.rm=T)
LSC_SA$nego.sett.wo.lit_pai<- rowSums(LSC_SA[,
                                             grepl("^F_[[:digit:]]+p",
                                                   colnames(LSC_SA),ignore.case=T) 
                                             ],na.rm=T)

#G closing code 

LSC_SA$nego.sett.w.lit_staff<- rowSums(LSC_SA[,
                                              grepl("^G_[[:digit:]]+s",
                                                    colnames(LSC_SA),ignore.case=T) 
                                              ],na.rm=T)
LSC_SA$nego.sett.w.lit_pai<- rowSums(LSC_SA[,
                                            grepl("^G_[[:digit:]]+p",
                                                  colnames(LSC_SA),ignore.case=T) 
                                            ],na.rm=T)

# H closing code 

LSC_SA$agency.decision_staff<- rowSums(LSC_SA[,
                                              grepl("^H_[[:digit:]]+s",
                                                    colnames(LSC_SA),ignore.case=T) 
                                              ],na.rm=T)

LSC_SA$agency.decision_pai<- rowSums(LSC_SA[,
                                            grepl("^H_[[:digit:]]+p",
                                                  colnames(LSC_SA),ignore.case=T) 
                                            ],na.rm=T)

# I a closing code 

LSC_SA$court.decision.uncontested_staff<- rowSums(LSC_SA[,
                                                         grepl("^Ia_[[:digit:]]+s",
                                                               colnames(LSC_SA),ignore.case=T) 
                                                         ],na.rm=T)
LSC_SA$court.decision.uncontested_pai<- rowSums(LSC_SA[,
                                                       grepl("^Ia_[[:digit:]]+p",
                                                             colnames(LSC_SA),ignore.case=T) 
                                                       ],na.rm=T)
#Ib closing code

LSC_SA$court.decision.contested_staff<- rowSums(LSC_SA[,
                                                       grepl("^Ib_[[:digit:]]+s",
                                                             colnames(LSC_SA),ignore.case=T) 
                                                       ],na.rm=T)
LSC_SA$court.decision.contested_pai<- rowSums(LSC_SA[,
                                                     grepl("^Ib_[[:digit:]]+p",
                                                           colnames(LSC_SA),ignore.case=T) 
                                                     ],na.rm=T)

#Ic closing code 

LSC_SA$court.decision.appeal_staff<- rowSums(LSC_SA[,
                                                    grepl("^Ic_[[:digit:]]+s",
                                                          colnames(LSC_SA),ignore.case=T) 
                                                    ],na.rm=T)
LSC_SA$court.decision.appeal_pai<- rowSums(LSC_SA[,
                                                  grepl("^Ic_[[:digit:]]+p",
                                                        colnames(LSC_SA),ignore.case=T) 
                                                  ],na.rm=T)

#Other Closure K 

LSC_SA$other.closure_staff<- rowSums(LSC_SA[,
                                            grepl("^K_[[:digit:]]+s",
                                                  colnames(LSC_SA),ignore.case=T) 
                                            ],na.rm=T)
LSC_SA$other.closure_pai<- rowSums(LSC_SA[,
                                          grepl("^K_[[:digit:]]+p",
                                                colnames(LSC_SA),ignore.case=T) 
                                          ],na.rm=T)

LSC_SA$extensive.services_staff<- rowSums(LSC_SA[,
                                                 grepl("^L_[[:digit:]]+s",
                                                       colnames(LSC_SA),ignore.case=T) 
                                                 ],na.rm=T)
LSC_SA$extensive.services_pai<- rowSums(LSC_SA[,
                                               grepl("^L_[[:digit:]]+p",
                                                     colnames(LSC_SA),ignore.case=T) 
                                               ],na.rm=T)


names(LSC_SA)

LSC_SA$total_cc_staff<- rowSums(LSC_SA[,colnames(LSC_SA) %in%
                                         c("counsel.advice_staff",
                                           "limited.action_staff",
                                           "nego.sett.wo.lit_staff",
                                           "nego.sett.w.lit_staff",
                                           "agency.decision_staff",
                                           "court.decision.uncontested_staff",
                                           "court.decision.contested_staff",
                                           "court.decision.appeal_staff",
                                           "other.closure_staff",
                                           "extensive.services_staff")],na.rm=T)

LSC_SA$total_cc_pai<- rowSums(LSC_SA[,colnames(LSC_SA) %in%
                                       c("counsel.advice_pai",
                                         "limited.action_pai",
                                         "nego.sett.wo.lit_pai",
                                         "nego.sett.w.lit_pai",
                                         "agency.decision_pai",
                                         "court.decision.uncontested_pai",
                                         "court.decision.contested_pai",
                                         "court.decision.appeal_pai",
                                         "other.closure_pai",
                                         "extensive.services_pai")],na.rm=T)


LSC_SA$all.limited_staff<- rowSums(LSC_SA[,colnames(LSC_SA) %in%
                                            c("counsel.advice_staff",
                                              "limited.action_staff")],na.rm=T)

LSC_SA$all.limited_pai<- rowSums(LSC_SA[,colnames(LSC_SA) %in%
                                          c("counsel.advice_pai",
                                            "limited.action_pai")],na.rm=T)


LSC_SA$all.extended_staff<- rowSums(LSC_SA[,colnames(LSC_SA) %in%
                                             c("nego.sett.wo.lit_staff",
                                               "nego.sett.w.lit_staff",
                                               "agency.decision_staff",
                                               "court.decision.uncontested_staff",
                                               "court.decision.contested_staff",
                                               "court.decision.appeal_staff",
                                               "other.closure_staff",
                                               "extensive.services_staff")],na.rm=T)

LSC_SA$all.extended_pai<- rowSums(LSC_SA[,colnames(LSC_SA) %in%
                                           c("nego.sett.wo.lit_pai",
                                             "nego.sett.w.lit_pai",
                                             "agency.decision_pai",
                                             "court.decision.uncontested_pai",
                                             "court.decision.contested_pai",
                                             "court.decision.appeal_pai",
                                             "other.closure_pai",
                                             "extensive.services_pai")],na.rm=T)

LSC_SA$all.contested_staff<- rowSums(LSC_SA[,colnames(LSC_SA) %in%
                                              c("nego.sett.w.lit_staff",
                                                "agency.decision_staff",
                                                "court.decision.contested_staff",
                                                "court.decision.appeal_staff")],na.rm=T)


LSC_SA$all.contested_pai<- rowSums(LSC_SA[,colnames(LSC_SA) %in%
                                            c("nego.sett.w.lit_pai",
                                              "agency.decision_pai",
                                              "court.decision.contested_pai",
                                              "court.decision.appeal_pai")],na.rm=T)


LSC_SA$consumer.finance_oc= rowSums(LSC_SA[,colnames(LSC_SA) %in% c("con_fin_s", "con_fin_p1", "con_fin_p2", "con_fin_p3")], na.rm=T)
LSC_SA$education_oc= rowSums(LSC_SA[,colnames(LSC_SA) %in% c("education_s", "education_p1", "education_p2", "education_p3")], na.rm=T)
LSC_SA$employment_oc= rowSums(LSC_SA[,colnames(LSC_SA) %in% c("employment_s", "employment_p1", "employment_p2", "employment_p3")], na.rm=T)
LSC_SA$family_oc= rowSums(LSC_SA[,colnames(LSC_SA) %in% c("family_s", "family_p1", "family_p2", "family_p3")], na.rm=T)
LSC_SA$juvenile_oc= rowSums(LSC_SA[,colnames(LSC_SA) %in% c("juvenile_s", "juvenile_p1", "juvenile_p2", "juvenile_p3")], na.rm=T)
LSC_SA$health_oc= rowSums(LSC_SA[,colnames(LSC_SA) %in% c("health_s", "health_p1", "health_p2", "health_p3")], na.rm=T)
LSC_SA$housing_oc= rowSums(LSC_SA[,colnames(LSC_SA) %in% c("housing_s", "housing_p1", "housing_p2", "housing_p3")], na.rm=T)
LSC_SA$income_oc= rowSums(LSC_SA[,colnames(LSC_SA) %in% c("income_s", "income_p1", "income_p2", "income_p3")], na.rm=T)
LSC_SA$individual_oc= rowSums(LSC_SA[,colnames(LSC_SA) %in% c("individual_s", "individual_p1", "individual_p2", "individual_p3")], na.rm=T)
LSC_SA$miscellaneous_oc= rowSums(LSC_SA[,colnames(LSC_SA) %in% c("misc_s", "misc_p1", "misc_p2", "misc_p3")], na.rm=T)


### Final Clean 
names(LSC_SA)
#LSC_SA$X<-NULL
LSC_SA$X <-NULL
LSC_SA$year.1 <-NULL
# Check duplicated records and write csv
table(duplicated(LSC_SA[, c("recipient_id", "wfta_year", "serv_area")]))

write.csv(LSC_SA, "Master_LSC_Admin.csv")


## create codebook 

names<- names(LSC_SA)
codebook<-data.frame(names,stringsAsFactors=F)

#create class variable 
codebook$class <- NA
  for (i in names(LSC_SA)) {
    codebook$class[codebook$name==i] <-  class(LSC_SA[, i])
  }

#create number NA values
codebook$number.of.NAs <-NA
  for (i in names(LSC_SA)){
    codebook$number.of.NAs[codebook$name==i] <- sum(is.na(LSC_SA[,i]))
  }

#create number unique
codebook$unique.values <-NA
for (i in names(LSC_SA)){
  codebook$unique.values[codebook$name==i] <- length(unique(LSC_SA[,i]))
}

#create min
codebook$min.value <-NA
for (i in names(LSC_SA)){
  codebook$min.value[codebook$name==i] <- min((LSC_SA[,i]), na.rm=T)
}

#create max value
codebook$max.value <-NA
for (i in names(LSC_SA)){
  codebook$max.value[codebook$name==i] <- max((LSC_SA[,i]), na.rm=T)
}

#create median value
codebook$median <-NA
for (i in names(LSC_SA)){
  codebook$median[codebook$name==i] <- median((LSC_SA[,i]), na.rm=T)
}

#form of origin
codebook$original.file.name <-NA
for (i in names(LSC_SA)){
 
  if (i %in% c("wfta_year", "recipient_id", "serv_area")){
    codebook$original.file.name[codebook$name==i] <- "identifiers"
  } 
  
  else if (i %in% names(expenses)){
    codebook$original.file.name[codebook$name==i] <- "D1-expenses"
  }
  
  else if (i %in% colnames(LSC_SA)[grepl("expenses.", colnames(LSC_SA),ignore.case=T)] ){
    codebook$original.file.name[codebook$name==i] <- "D1-expenses"
  }
   
  else if (i %in% names(G3)){
    codebook$original.file.name[codebook$name==i] <- "G3-case closures"
   }
  
  else if (i %in% names(demos)){
    codebook$original.file.name[codebook$name==i] <- "G4-client demographics"
  }
  
  else if (i %in% names(opencases)){
    codebook$original.file.name[codebook$name==i] <- "G5-opencases"
  }
  
  else if (i %in% names(PAI)){
    codebook$original.file.name[codebook$name==i] <- "J1-PAI"
  }
  
  else if (i %in% names(revenue)){
    codebook$original.file.name[codebook$name==i] <- "D3-revenue"
  }
  
  else if (i %in% names(addresses)){
    codebook$original.file.name[codebook$name==i] <- "salesforce address data"
  }
  
  
  else {
  codebook$original.file.name[codebook$name==i] <- "Constructed Variable" 
  }    
}

View(codebook)
write.csv(codebook, "master_codebook.csv")


### renaming variable names

LSC_SA<-read.csv("Master_LSC_Admin.csv", stringsAsFactors=F, header=T)

book<-read.csv("codebookwmods.csv", stringsAsFactors=F, header=T)
str(book)

updates <- book$updated.variable.name[match(colnames(LSC_SA), book$original.name)]
str(updates)
updates[is.na(updates)] <- colnames(LSC_SA)[is.na(updates)]
updates[updates==""] <- colnames(LSC_SA)[updates==""]
colnames(LSC_SA) <- updates
str(LSC_SA)

write.csv(LSC_SA,"Master_GAR_SA_level_2008_2015.csv")


### Reading Staff Correctly 
### note - this method resolves all problems except the start time variable

library(readr)
staff<-read_csv("E1ab_Staffing_1999_2015.csv", 
                na = c("", NA, "NULL"),
                col_types = cols(HoursPerWeek = col_double(),
                                 annl_salary = col_double(),
                                 yrs_exp_w_lsc = col_double()) )





