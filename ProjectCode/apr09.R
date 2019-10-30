#apr09

library(dplyr)

coordsCSV <- read.csv('countries_codes_and_coordinates.csv', stringsAsFactors = FALSE)
head(coordsCSV)

coordsCSV$Alpha.2.code <- sapply(coordsCSV$Alpha.2.code, function(x) gsub("[ ]", "", x))


#which countries appear twice
repeats <- names(which(table(coordsCSV$Alpha.2.code)>=2))

for (x in repeats) {
  ind <- which(coordsCSV$Alpha.2.code == x)
  coordsCSV <- coordsCSV[-ind[2:length(ind)],]
}

latLong <- data.frame(iso3166 = coordsCSV$Alpha.2.code,
                      latitude = coordsCSV$Latitude..average.,
                      longitude = coordsCSV$Longitude..average.)
nrow(latLong)