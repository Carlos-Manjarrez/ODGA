
### Overall Calculations 
count.unique<-function(x){
  length(unique(x))
}

tapply(LSC_SA$total_cc_calc, LSC_SA$wfta_year, sum, na.rm=T)
tapply(LSC_SA$total_cc_pai, LSC_SA$wfta_year, sum, na.rm=T)
tapply(LSC_SA$total_cc_staff, LSC_SA$wfta_year, sum, na.rm=T)

tapply(LSC_SA$women, LSC_SA$wfta_year, sum, na.rm=T)

names(additional)
additional<-read.csv("AdditionalInfo.csv", stringsAsFactors=F, header=T)

additional$total_household<- rowSums(additional[,colnames(additional) %in%
                                       c("adults_in_households", "children_in_households")], na.rm=T)

tapply(additional$total_household, additional$wfta_year, sum, na.rm=T)
tapply(additional$adults_in_households, additional$wfta_year, sum, na.rm=T)

tapply(additional$domestic_violence_cases, additional$wfta_year, sum, na.rm=T)



### Number of Offices 
tapply(LSC_SA$recipient_id, LSC_SA$wfta_year, count.unique)
str(offices)
small.offices<-subset(offices, offices$off_type=="B" | offices$off_type=="M")

tapply(offices$lsc_office_id, offices$year, length)
tapply(small.offices$lsc_office_id, small.offices$year, length)


table(offices$lsc_office_id)

str(offices)
table(duplicated(offices[, c("lsc_office_id", "year")]))

###read file
staff<-read_csv("E1ab_Staffing_1999_2015.csv", 
                na = c("", NA, "NULL"),
                col_types = cols(HoursPerWeek = col_double(),
                                 annl_salary = col_double(),
                                 yrs_exp_w_lsc = col_double()) )

all.probs<-problems(staff)
table(all.probs$col)
sapply(staff, function(x) sum(is.na(x), na.rm = T))

staff$job_description<-gsub('[[:digit:]]+[[:space:]]+-+[[:space:]]', '', staff$job_description)

###### do calculations 

#staff$exp_buckets[staff$exp_buckets == NA]<- 0

staff$addition<-1

staff$FTE_count<-NA
staff$FTE_count<- staff$HoursPerWeek/35
staff$FTE_count[staff$FTE_count>=1]<- 1
table(staff$FTE_count)

tapply(staff$FTE_count, staff$year, sum, na.rm=T)

Paralegals<-staff[staff$job_description=="Paralegal",]
tapply(Paralegals$FTE_count, Paralegals$year, sum, na.rm=T)

tapply(staff$attorney, staff$year, sum, na.rm=T)

staff$attorney_jobs<-NA
staff$attorney_jobs[staff$job_description=="Managing Attorney"]<- "Attorney"
staff$attorney_jobs[staff$job_description=="Supervising Attorney"]<- "Attorney"
staff$attorney_jobs[staff$job_description=="Staff Attorney"]<- "Attorney"
staff$attorney_jobs[staff$job_description=="Director of Litigation"]<- "Attorney"

Attorneys<-staff[staff$attorney_jobs=="Attorney",]
table(Attorneys$job_description)


tapply(Attorneys$FTE_count, Attorneys$year, sum, na.rm=T)



tapply(staff$addition, staff$year, sum, na.rm=T)
tapply(staff$FTE_count, staff$year, sum, na.rm=T)




names(master)



