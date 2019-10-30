#apr11

library(dplyr)
countryCodes
infMortality
popData
latLong

countryData <- inner_join(countryCodes, popData, by = c("cia" = "countryCode"))
countryData <- inner_join(countryData, infMortality, by = c("cia" = "countryCode"))
countryData <- inner_join(countryData, latLong, by = c("iso3166" = "iso3166"))

#Deliverable 4
#All countries in factbook without a iso3166 are not in final table
notIncluded <- countryCodes$country[which(countryCodes$iso3166 == "-")]
which(countryData$country %in% notIncluded)

notIncluded <- setdiff(countryCodes$country, countryData$country)

#Deliverable 5
#Find the mean mortality rate for all countries with population less than 10 million, and for those countries with population greater than 50 million.
m1 <- mean(countryData$mortality[which(countryData$population < 10000000)])
m2 <- mean(countryData$mortality[which(countryData$population > 50000000)])
