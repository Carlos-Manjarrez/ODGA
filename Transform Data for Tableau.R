### This code can be used to shape a file in an ideal format for Tableau
### which is long format

### set your working directory, which has your files. 

### read in your file
master<-read.csv("Master_GAR_SA_level_2008_2015.csv", stringsAsFactors=F, header=T)

### install and load the required R packages. If you do not have these libraries installed
#### run "install.packages("plyr")" and "install.packages("reshape2")"
#### to download and install on your computer
 
library(plyr)
library(reshape2)

### The next step is getting the data at the desired unit level
### our master file is at the level of recipient, year, service area
#### if we just want to work with a recipient-level file in Tableau, 
### we can "summarize" the data in R to show it only at the recipient level

### here is the sample code below, "master" is the name of the file you are using,
### the variables in the c() are the identifying variables for the level of data
### you are using."Summarise" is a function that summarizes at the level of interest
## on the left hand side of the equal sign is the name of the variable in your new data set
### on the right, you specify what is done to the original data set


master_rl_salaries <- ddply(master, c("recipient_id", "year", "Organization_Name"), summarise,
                            total_Lawyers = sum(total_Lawyers, na.rm=T), 
                            total_Paralegals = sum(total_Paralegals, na.rm=T),
                            total_Other_Staff = sum(total_Other_Staff, na.rm=T))

# now you have a data file called "master_rl_salaries" which is at the recipient level
## Tableau works best (for percentages etc) if your file is in long format. This means your data
# set will include separate columns for ID variables, and then two additional columns
## one that has the variable name and another that has the value of the variable

### I recommend creating a separate file for each category of variable i.e. funding, age etc.

### you use the "melt" function in R to get this file format. The variables in c()
### are the ID variables you want to maintain. 


master_rl_salaries <-melt(master_rl_salaries, id=c("recipient_id", "year", "Organization_Name"))

## if you want to check out the structure of your file type "str(master_rl_salaries" to view
### or you can type View(master_rl_salaries) to view a table of the data in R

## the last step is writing the file to csv, where it can then be uploaded into Tableau
write.csv(master_rl_salaries, "master_rl_salaries.csv")

