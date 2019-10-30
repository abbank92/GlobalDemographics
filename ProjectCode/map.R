#getting Extension Data
cityData <- read.csv("extensionData/1790-2010_MASTER.csv",
                     stringsAsFactors = FALSE)
countyAndMSA <- read.csv('extensionData/county_msa.csv',
                         stringsAsFactors = FALSE)
msaAndGDP <- read.csv('extensionData/gdp_metro0917.csv',
                      stringsAsFactors = FALSE)
#Get rid of cities with no population in 2010
cityData <- cityData[-which(cityData$X2010 == 0),]
#First clean the counties by adding the state
cityData$CountyName2 <- paste(cityData$County_Name,
                              cityData$ST,
                              sep = ", ")
#Left join of cities with their MSA by county
citiesWithMSA <- left_join(cityData,
                           countyAndMSA,
                           by = c("CountyName2" = "County"))
#Fix some shit
msaAndGDP$U.S..metropolitan.areas <- gsub(', .*$', '',
                                          msaAndGDP$U.S..metropolitan.areas)
citiesWithMSA$MSA <- gsub(', .*$', '',
                          citiesWithMSA$MSA)
#Left join of new data set with GDP by metro area
data <- left_join(citiesWithMSA,
                  msaAndGDP,
                  by = c("MSA" = "U.S..metropolitan.areas"))

#map

m <- leaflet() %>% addTiles()

maxrad <- 30
palette <- brewer.pal(5, "Blues")
yearsToGraph <- c('X1790', 'X1850', 'X1900', 'X1950', 'X2010')
for (y in yearsToGraph) {
  i <- which(names(data) == y)
  
  nonzero <- data[which(data[,i]!=0),]
  nonzero <- nonzero[which(!is.na(nonzero$LAT)),]
  
  tiers <- quantile(nonzero[,i], probs = seq(0,1,.2))
  tiers[1] <- tiers[1] - .0005
  
  cityPopGroup <- cut(nonzero[,i], tiers)
  cityPopGroupColors <- palette[cityPopGroup]
  
  s <- sqrt(nonzero[,i])
  k <- maxrad/max(s)
  
  m <- m %>% addCircleMarkers(radius = s*k,
                              lng = nonzero$LON, lat = nonzero$LAT,
                              color = cityPopGroupColors, fillOpacity = 0.2,
                              group = gsub('X', '', y))
}

metros <- unique(data$MSA)
metros <- metros[!is.na(metros)]
flashyCities <- numeric(length(metros))
for (i in 1:length(metros)) {
  tro <- metros[i]
  oneMetro <- data[which(data$MSA == tro),]
  flashyCities[i] <- oneMetro$City[which.max(oneMetro$X2010)]
}
dataForGroups <- data[which(data$City %in% flashyCities), ]
dataForGroups <- dataForGroups[which(!is.na(dataForGroups$LON)),]
dataForGroups <- dataForGroups[which(!is.na(dataForGroups$X2015)),]
X <- dataForGroups[, c('X2010','X2015')]
X <- as.matrix(X)
assignments <- kmeans(X, 5)
groupPalette <- c('red', 'green', 'orange', 'purple', 'blue')
dataForGroups$grolors <- groupPalette[assignments]

icons <- awesomeIcons(icon = 'ion-cash',
                      iconColor = 'black', library = 'ion',
                      markerColor = dataForGroups$grolors)

m <- m %>% addAwesomeMarkers(lng = dataForGroups$LON, lat = dataForGroups$LAT,
                             icon = icons, label = dataForGroups$City,
                             group = 'Econ and Pop Group')

m <- m %>% addLayersControl(baseGroups = c('1790', '1850', '1900', '1950', '2010'),
                            overlayGroups = c('Econ and Pop Group'),
                            options = layersControlOptions(collapsed = FALSE))
m <- m %>% hideGroup('Econ and Pop Group')
m
