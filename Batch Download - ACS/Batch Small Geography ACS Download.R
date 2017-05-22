### BATCH ACS DOWNLOAD CODE: 

# this code can be used to do a batch national-level download of ACS data for a particular
# table name and geography type (i.e. tract or county level). 

## this  code is a good option when you know the names of the tables you want to download, 
## and want a batch national file. For state level data, and if you don't know the names
## of the tables, try using the ACS or acslite14 packages in R to find relevant data and table names. 

# In order to use this code, make sure "fips_codes.csv" is downloaded in your
# working directory (available on ODGA github)

### install relevant R packages, and load as needed

library(acs14lite) # used to fetch ACS data, 5 year, 2010-2014
## note, there is also an ACS packages in R that lets you select other census vintages
### the code below can be modified slightly if you're using the ACS package instead of acs14lite
### both packages have great documentation online
library(dplyr) # used to reformat the ACS data
library(tigris) # used to fetch TIGER data (shapefiles), if needed
library(sp) # for working with spatial data objects
library(rgdal) # for importing and exporting spatial data in various formats

### STEP 1 - INSTALL API KEY

### you need to get an API key from Census http://api.census.gov/data/key_signup.html
### once you have the key, uncomment and run the following 2 lines, with your api
### key included: 


### api.key.install("INSERT KEY HERE")
### set_api_key("INSERT KEY HERE")

### you will only have to install the API key once. You will need to set it
# each time you work with census data. 


## STEP 2 - CREATE LIST OF STATES FOR LOOP 

### ACS data at smaller geographies (counties, tracts etc.) is only available to download at the state 
## level. In order to create a national file of all counties or tracts in all states, you'll
### need to loop and bind together data from all the states.

### in this step, you upload a file with all of the state names/state FIPS ids. 

geo_ids<- read.csv( "fips_codes.csv", stringsAsFactors=F, header=T)
str(geo_ids)

state_ids<-geo_ids[,c("State.Abbreviation", "State.FIPS.Code")]
state_ids<-distinct(state_ids)

### STEP 3 - SET UP THE FUNCTION

## run the following code to set up the function. 

get_national_data<- function(tables, geography){
  output <- list()
  for(i in 1:nrow(state_ids)){ 
    print(i)
    
    output[[i]] <- acs14(geography = geography , state = state_ids$State.FIPS.Code[i], 
                         variable = tables)   
    
  } 
  acs_national <- do.call(rbind, output)
  return(acs_national)
}

## STEP 4 - RUN THE FUNCTION FOR YOUR ACS TABLES OF INTEREST

# create a character vector (in this case poverty_tables), with the names of the 
# acs tables you're interested in downloading.  

poverty_tables<- c('B17021_001E', 'B17021_002E')

## Run the function for the variables you're interested in, and specify the geography

national_file <- get_national_data(poverty_tables, geography = "tract")

### Check Data - make sure it looks ok
View(national_file[sample(1:nrow(national_file), 100), ])

### write to the csv name of your choice

write.csv(national_file, "national_file.csv")

### THAT IS ALL YOU NEED TO DO if you're just interested in a file
# with datpoints for each county, tract etc. The following notes describe how to merge 
## with geographic identifiers: 

### MERGING INTERNAL LATITUDE, LONGITUDE

### OPTION 1 - Gazetter Files, Internal Lat/Long

### You can merge the data you created with Census Gazetter Files that show
### internal longitude and latitude points, if you're using this data in a 
## GIS or mapping software. Gazetter files for the appropriate vintage can be downloaded here, and merged based on FIPS codes:
# https://www.census.gov/geo/maps-data/data/gazetteer.html
## I found that the easiest way to merge geographic data onto the file. 

### OPTION 2 - Tigris Shapefiles (More tricky)

### You can also merge the data with TIGRIS Shapefiles. Like ACS data, though
## TIGRIS shapefiles are available first at the state level, and then at sub-levels
### merging would therefore require looping through all the shapefiles

## Note, ArcGIS Online cannot handle national-level shapefiles. 


