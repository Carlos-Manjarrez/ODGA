
### note - this script exports XLSX files for salary data by states.
### we have not cleaned to check for duplicate records etc. 
### is based on headcount, not FTE

setwd()

library(readr)
staff<-read_csv("E1ab_Staffing_1999_2015.csv", 
                na = c("", NA, "NULL"),
                col_types = cols(HoursPerWeek = col_double(),
                                 annl_salary = col_double(),
                                 yrs_exp_w_lsc = col_double()) )
### note - parsing errors for hrs/week

office<-read.csv("originaloffice.csv",stringsAsFactors=F, header=T)
state<-office[,c("state", "lsc_office_id")]
staff<-merge(staffing,state, by.x=c("lsc_office_id"), 
               by.y=c("lsc_office_id"), all.x=T)

library(xlsx)
grant.type<-read.xlsx("2016 Funding Decisions Chart Final updated by RJH to show MI-13 decision (1).xls",
                      header=T, sheetIndex=1, stringsAsFactors=F)

grant.type<-grant.type[, c("recipient_ID", "Organization.Name", "serv_area", "grant_type")]
grant.type<-grant.type[complete.cases(grant.type),]

### delete anyone with salary less than 10k
staff<-staff[staff$annl_salary>=10000,]
##delete anyone with crazy years of experience
staff<-staff[ staff$yrs_exp_prof<=70,]
staff<-staff[ staff$year>=2008,]

# create pct time all 

staff$pct_time_all<- staff$pct_time_1+staff$pct_time_2+staff$pct_time_3+staff$pct_time_4+staff$pct_time_5

#create bins
staff$prof_exp_bins <-NA
staff$prof_exp_bins[staff$yrs_exp_prof %in% 0:1] <- "0-1 years"
staff$prof_exp_bins[staff$yrs_exp_prof %in% 2] <- "2 years"
staff$prof_exp_bins[staff$yrs_exp_prof %in% 3] <- "3 years"
staff$prof_exp_bins[staff$yrs_exp_prof %in% 4] <- "4 years"
staff$prof_exp_bins[staff$yrs_exp_prof %in% 5] <- "5 years"
staff$prof_exp_bins[staff$yrs_exp_prof %in% 6:7] <- "6-7 years"
staff$prof_exp_bins[staff$yrs_exp_prof %in% 8:9] <- "8-9 years"
staff$prof_exp_bins[staff$yrs_exp_prof %in% 10:14] <- "10-14 years"
staff$prof_exp_bins[staff$yrs_exp_prof %in% 15:19] <- "15-19 years"
staff$prof_exp_bins[staff$yrs_exp_prof %in% 20:24] <- "20-24 years"
staff$prof_exp_bins[staff$yrs_exp_prof %in% 25:29] <- "25-29 years"
staff$prof_exp_bins[staff$yrs_exp_prof %in% 30:70] <- "30 or more years"

#create alternative, less granular, bins
staff$prof_exp_bins_2 <-NA
staff$prof_exp_bins_2[staff$yrs_exp_prof %in% 0:5] <- "0-5 years"
staff$prof_exp_bins_2[staff$yrs_exp_prof %in% 6:10] <- "6-10 years"
staff$prof_exp_bins_2[staff$yrs_exp_prof %in% 11:20] <- "11-20 years"
staff$prof_exp_bins_2[staff$yrs_exp_prof %in% 20:70] <- "20 or more years"

staff$cleanpos<-gsub('[[:digit:]]+[[:space:]]+-+[[:space:]]', '', staff$job_description)
staff$job_description<-staff$cleanpos

staff$prof_exp_bins<-as.factor(staff$prof_exp_bins)
staff$prof_exp_bins <- factor(staff$prof_exp_bins,
  levels=c("0-1 years","2 years","3 years","4 years",
          "5 years","6-7 years","8-9 years","10-14 years","15-19 years",
          "20-24 years","25-29 years","30 or more years"))

staff$prof_exp_bins_2<-as.factor(staff$prof_exp_bins_2)
staff$prof_exp_bins_2 <- factor(staff$prof_exp_bins_2,
             levels=c("0-5 years", "6-10 years", "11-20 years",
                      "20 or more years"))
## look at service areas missing from our overall list 

unique(staff$serv_area_5[!staff$serv_area_5 %in% grant.type$serv_area])

##  create data frame of missing SA

serv_area<-c("NA", "NULL" , "LA-1"  ,"MTX"  , "MAL" ,  "MAR" ,  "MKS" ,
             "OH-17" ,"LA-12", "OH-5" ,"MWY", 
             "MNV"  , "MTX"   ,"MAR" ,  "MKY"  , "MLA"  , "MKS"  , "MWV"  , "OH-5" ,
             "LA-1" , "OH-17", "LA-12", "MNV" ,
             "MAR" , "MKY" , "MLA" , "MMS" , "MWY" , "MAL" ,
             "MAR" , "MMS" , "MLA" , "MTN" , "MKY", "MTN" , "MMS" , "MAR",  "MKY")
grant_type<-c("NA", "NA", "Basic Field", "Migrant Worker", "Migrant Worker",
              "Migrant Worker", "Migrant Worker", "Basic Field", "Basic Field",
              "Basic Field", "Migrant Worker",
              "Migrant Worker", "Migrant Worker", "Migrant Worker",
              "Migrant Worker","Migrant Worker","Migrant Worker","Migrant Worker",
              "Basic Field","Basic Field","Basic Field","Basic Field", "Migrant Worker",
              "Migrant Worker","Migrant Worker","Migrant Worker","Migrant Worker",
              "Migrant Worker","Migrant Worker","Migrant Worker","Migrant Worker"
              ,"Migrant Worker","Migrant Worker","Migrant Worker", 
              "Migrant Worker","Migrant Worker","Migrant Worker","Migrant Worker")


extras<-data.frame(serv_area, grant_type)
extras <- extras[!duplicated(extras), ]

## append extras to grant.type

grant.type <- grant.type[!duplicated(grant.type), ]
grant.type<-grant.type[, c("serv_area", "grant_type")]
grant.type<-rbind(grant.type, extras)

write.csv(grant.type, "fieldtypes.csv")

unique(grant.type$grant_type)
basic<-grant.type[grant.type$grant_type=="Basic Field", c("serv_area")]
native<-grant.type[grant.type$grant_type=="Native American", c("serv_area")]
migrant<-grant.type[grant.type$grant_type=="Migrant Worker", c("serv_area")]

## create variables for migrant, Native, and basic 
staff$basic <- staff$serv_area_1 %in% basic | staff$serv_area_2 %in% basic | 
  staff$serv_area_3 %in% basic| staff$serv_area_4 %in% basic |
  staff$serv_area_5 %in% basic

staff$native <- staff$serv_area_1 %in% native | staff$serv_area_2 %in% native|
  staff$serv_area_3 %in% native| staff$serv_area_4 %in% native |
  staff$serv_area_5 %in% native

staff$migrant <- staff$serv_area_1 %in% migrant | staff$serv_area_2 %in% migrant| 
  staff$serv_area_3 %in% migrant| staff$serv_area_4 %in% migrant |
  staff$serv_area_5 %in% migrant


### subset to just basic or migrant

basicstaff<-staff[staff$basic==T,]
str(basicstaff)
basicstaff2015<-basicstaff[basicstaff$year==2015,]

migrantstaff<-staff[staff$migrant==T,]
migrantstaff2015<-migrantstaff[migrantstaff$year==2015,]

nativestaff<-staff[staff$native==T,]
nativestaff2015<-nativestaff[nativestaff$year==2015,]


### creates all staff table for 2015 (can subset by diff years)
staff2015<-staff[staff$year==2015,]

#rm(list = ls()[!ls() %in% c("staff", "staff2015")])
  
subset.state <-function(state, staff2015, staff,prof_exp_bins){
  
  staff2015.state<-staff2015[staff2015$state==state,]
  
  all.staff.table<- tapply(staff2015.state$annl_salary,
                           list(staff2015.state$job_description, 
                                staff2015.state[, prof_exp_bins])
                           , median, na.rm = TRUE)
colnames(all.staff.table) <- paste0("exp_", colnames(all.staff.table))
colnames(all.staff.table) <- gsub(" |-", "_", colnames(all.staff.table))  
  
  
  staff.cases<-tapply(staff2015.state$annl_salary,
                      list(staff2015.state$job_description, 
                           staff2015.state[,prof_exp_bins]
                          )
                      , length)
  
all.staff.table[staff.cases<3] <- NA

# if instead just want total numbers to be above or below certain #s
#all.staff.table[rowSums(staff.cases, na.rm = TRUE) < 3, ] <- NA

  #### Create aggregate table - all states
  
staff$annl_salary<-as.numeric(staff$annl_salary)
  
agg.staff <- aggregate(staff$annl_salary, 
                         by = list(staff$job_description
                                   , staff$year, staff$state)
                         , median, na.rm = TRUE)
  
  library(plyr)
  agg.staff<-rename(agg.staff, c("Group.1"="job_description", "Group.2"="year",
                                 "Group.3"="state", "x"="median_salary"))
  library("reshape2")
  agg.staff$year<-as.character(agg.staff$year)
  
  agg.cast<-dcast(agg.staff, job_description+state~year)
  
  agg.cast$percent_change_from_2014 <- (agg.cast$'2015' - agg.cast$'2014')/
    (agg.cast$'2014')
  agg.cast$percent_change_from_2010 <- (agg.cast$'2015' - agg.cast$'2010')/
    (agg.cast$'2010')
  
  agg.cast$percent_change_from_2014<- 
    round(agg.cast$percent_change_from_2014, digits = 2)
  
  agg.cast$percent_change_from_2010<- 
    round(agg.cast$percent_change_from_2010, digits = 2)
  
  
  keeps <- c("state","job_description", "2015","percent_change_from_2014",
             "percent_change_from_2010")
  aggregate<-agg.cast[keeps]
aggregate <- aggregate[aggregate$state==state, ]
rownames(aggregate)<-NULL

## print all states
  
  library(xlsx)
  
  #install.packages("devtools")
  #devtools::install_github("kassambara/r2excel")
  library(r2excel)  
  
  filename <- paste(state, "2015", "salaries.xlsx")
  wb <- createWorkbook(type="xlsx")
  sheet1 <- createSheet(wb, sheetName = "Median Salary by Experience")
  sheet2 <- createSheet(wb, sheetName = "Aggregate 2015 Salary Data")
  
  xlsx.addHeader(wb, sheet1, value="Median Salary by Experience",level=1, 
                 color="black", underline=1)
  xlsx.addLineBreak(sheet1, 1)
  
  xlsx.addHeader(wb, sheet2, value="2015 Median Aggregate Salary",level=1, 
                 color="black", underline=1)
  xlsx.addLineBreak(sheet2, 1)
  
  info=paste("Based on years experience in the profession and annualized salaries as
reported by LSC Grantees through the 2015 Grant Activity Reports.
Excludes staff with annualized salaries of less than $10,000, and
does not show data without sufficient aggregation for anonymity", sep="")

info2=paste("This chart shows median 2015 salary for each position
in the state. The percent change in median salary from 2014 and
2010 is also shown. All values in nominal dollars.", sep="")
  
xlsx.addParagraph(wb, sheet1,value=info, isItalic=TRUE, colSpan=6, 
                    rowSpan=6, fontColor="darkgray", fontSize=14)

xlsx.addParagraph(wb, sheet2,value=info2, isItalic=TRUE, colSpan=6, 
                  rowSpan=6, fontColor="darkgray", fontSize=14)

  xlsx.addTable(wb, sheet1, all.staff.table)
  xlsx.addLineBreak(sheet1, 2)
  
  xlsx.addTable(wb, sheet2, aggregate)
  xlsx.addLineBreak(sheet2, 2)
  
  saveWorkbook(wb, filename)
  return(NULL)
}

all.states <- unique(basicstaff2015$state)

subset.state(state="CA", staff2015=staff2015, staff=staff,
             prof_exp_bins="prof_exp_bins")


lapply(all.states, subset.state,
       staff = basicstaff, staff2015 =basicstaff2015, prof_exp_bins="prof_exp_bins_2")



