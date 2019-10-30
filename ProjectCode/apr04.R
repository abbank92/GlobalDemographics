#apr04

setwd("~/Documents/stat359/GlobalDemographics")
library(XML)

system("open -a 'Google Chrome' factbook.xml")
xmlObject <- xmlParse('factbook.xml')
root <- xmlRoot(xmlObject)
root

infants <- getNodeSet(xmlObject, '//field[@name="Infant mortality rate"]/rank')
infants
countryCode <- numeric(length(infants)); mortality <- numeric(length(infants))
for (i in 1:length(infants)) {
  countryCode[i] <- xmlGetAttr(infants[[i]], "country")
  mortality[i] <- as.numeric(xmlGetAttr(infants[[i]], "number"))
}
infmort <- data.frame(countryCode = countryCode, mortality = mortality, stringsAsFactors = FALSE)
head(infmort)
hist(infmort$mortality)




pops <- getNodeSet(xmlObject, '//field[@id="f2119"]/rank')
countryCode <- numeric(length(pops)); pop <- numeric(length(pops))
for (i in 1:length(pops)) {
  countryCode[i] <- xmlGetAttr(pops[[i]], "country")
  pop[i] <- as.numeric(xmlGetAttr(pops[[i]], "number"))
}
popData <- data.frame(countryCode = countryCode, population = pop, stringsAsFactors = FALSE)







