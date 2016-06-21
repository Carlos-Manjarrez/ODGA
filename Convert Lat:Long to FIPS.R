### Convert Lat/Long to FIPS

library("RJSONIO")
library("RCurl")

# FCC's Census Block Conversions API
# http://www.fcc.gov/developers/census-block-conversions-api

latlong2fips <- function(latitude, longitude) {
  url <- "http://data.fcc.gov/api/block/find?format=json&latitude=%f&longitude=%f"
  url <- sprintf(url, latitude, longitude)
  json <- RCurl::getURL(url)
  json <- RJSONIO::fromJSON(json)
  as.character(json$County['FIPS'])
}

# For example - Orange County
latlong2fips(latitude=28.35975, longitude=-81.421988)

## add FIPS to a dataset that already has lat and long included in it. 
geocodes$fips<-NA
for (i in 1:nrow(geocodes)){
  fips<-latlong2fips(latitude=geocodes$Latitude[i], longitude=geocodes$Longitude[i])
  geocodes$fips[i]<-fips
  print(fips)
}

