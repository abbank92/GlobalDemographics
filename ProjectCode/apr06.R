#apr06

countryTable <- getNodeSet(xmlObject, '//appendix[@name="cross-reference list of country data codes"]/table/row')
country <- sapply(countryTable, function(row) xmlGetAttr(row[[1]], "content"))
cia <- sapply(countryTable, function(row) xmlGetAttr(row[[1]], "country"))
iso3166 <- sapply(countryTable, function(row) xmlGetAttr(row[[3]], "content"))

countryCodes <- data.frame(country = country, cia = cia, iso3166 = iso3166, stringsAsFactors = FALSE)
head(countryCodes)
countryCodes[which(countryCodes$country== "Tajikistan"), ]
