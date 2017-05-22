setwd()
LSC_SA.demos<-read.csv("LSC_SA.demos.csv", stringsAsFactors=F, header=T)

names(LSC_SA.demos)

colnames(LSC_SA.demos) <- gsub("nat_amr", "native", colnames(LSC_SA.demos))
colnames(LSC_SA.demos) <- gsub("0_17", "017", colnames(LSC_SA.demos))
colnames(LSC_SA.demos) <- gsub("18_59", "1859", colnames(LSC_SA.demos))
colnames(LSC_SA.demos) <- gsub("60_ovr", "60over", colnames(LSC_SA.demos))
colnames(LSC_SA.demos) <- gsub("36_59", "3659", colnames(LSC_SA.demos))
colnames(LSC_SA.demos) <- gsub("18_35", "1835", colnames(LSC_SA.demos))

vars <- names(LSC_SA.demos)[9:38]

subset <- LSC_SA.demos[, vars]


results <- matrix(NA, ncol = length(unique(vars)), nrow = nrow(subset))
colnames(results) <- unique(vars)
for(i in unique(vars)) {
  print(i)
  results[, i] <- rowSums(subset[, which(vars==i), drop = FALSE], na.rm=T)
}
head(results)
#LSC_SA <- data.frame(LSC_SA, results, stringsAsFactors = FALSE)

countLetterNum <- function(df) {
  counts <- colSums(df[, vars], na.rm = TRUE)
  overall <- data.frame(counts, race = sapply(strsplit(vars, "_"), function(x) x[2]),
                        age = sapply(strsplit(vars, "_"), function(x) x[1]))
  counts.by.race.age <- tapply(overall$counts, 
                                 list(as.numeric(overall$age), overall$race), 
                                 sum, na.rm = TRUE)
  return(counts.by.letter.num)
}

list(LSC_SA.demos$recipient_id, LSC_SA.demos$serv_area, LSC_SA.demos$wfta_year)

library(reshape)
results <- data.frame(recipient_id = LSC_SA.demos$recipient_id, 
                      serv_area = LSC_SA.demos$serv_area, 
                      wfta_year = LSC_SA.demos$wfta_year,
                      results)

long.results <- melt(results, id.vars = c("recipient_id", "serv_area", "wfta_year"))
long.results$variable <- as.character(long.results$variable)
long.results$race <- sapply(strsplit(long.results$variable, split = "_"), function(x) x[1])
long.results$age <- sapply(strsplit(long.results$variable, split = "_"), function(x) x[2])

head(long.results)
table(long.results$age)
vars

long.results$age[long.results$age == "017"] <- "under 18"
long.results$age[long.results$age == "1835"] <- "18-35"
long.results$age[long.results$age == "1859"] <- "18-59"
long.results$age[long.results$age == "3659"] <- "36-59"
long.results$age[long.results$age == "60over"] <- "60 and over"

long.results.demos<-long.results

long.results.demos<-merge(long.results.demos, addresses, 
                    by.x=c("recipient_id"
                    ),
                    by.y=c("Recipient.ID"
                    ),all.x=T)

LSC.merge<- LSC_SA[,c( "recipient_id", "wfta_year" ,"serv_area",
                       "serv_area_type")]

long.results.demos<-merge(long.results.demos, LSC.merge, 
                    by.x=c("serv_area", "recipient_id", "wfta_year"),
                    by.y=c("serv_area", "recipient_id", "wfta_year"), all.x=T)


str(long.results.demos)
write.csv(long.results.demos, "long.results.demos.csv")

head(long.results.demos)

head(LSC_SA.demos)
long.results.demos.check[1:50, c("serv_area","recipient_id","wfta_year","variable", 
                             "value", "race")]

long.results.demos.check<-long.results.demos[
  long.results.demos$recipient_id==107000,]

### additional information

tapply(LSC_SA$Corp_Indiv_Contributions, LSC_SA$wfta_year, sum, na.rm=T)
additional<-read.csv("AdditionalInfo.csv", stringsAsFactors=F, header=T)
str(additional)

additional<-merge(additional, addresses, 
                          by.x=c("recipient_id"
                          ),
                          by.y=c("Recipient.ID"
                          ),all.x=T)

write.csv(additional, "additionalfortableau.csv")

## languages

language<-read.csv("ClientLanguage.csv", stringsAsFactors=F, header=T)
str(language)

language<-merge(language, addresses, 
                  by.x=c("recipient_id"
                  ),
                  by.y=c("Recipient.ID"
                  ),all.x=T)

language<-merge(language, LSC.merge, 
                          by.x=c("serv_area", "recipient_id", "wfta_year"),
                          by.y=c("serv_area", "recipient_id", "wfta_year"), all.x=T)

str(language)

write.csv(language, "languagetableau.csv")

tapply(language$serv_area_type, language$wfta_year, sum, na.rm=T)

