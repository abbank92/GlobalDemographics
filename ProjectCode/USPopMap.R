#shinyApp
library(shiny)
library(leaflet)
library(RColorBrewer)

setwd("~/Documents/stat359/GlobalDemographics")
data <- read.csv('checkData.csv', stringsAsFactors = FALSE)

kmeans <- function(X, k) {
  X <- scale(X)
  centroids <- list()
  centroids <- X[sample(1:nrow(X), k), ]
  oldGroups <- numeric(nrow(X))
  newGroups <- 1:nrow(X)
  
  while(!identical(oldGroups, newGroups)) {
    oldGroups <- newGroups
    #make assignments based on distance
    newGroups <- sapply(1:nrow(X), function(i){
      row <- X[i, ]
      return(bestCentroid(row, centroids))
    })
    #update centroids
    for (c in 1:k) {
      group <- X[which(newGroups == c),]
      centroids[c,] <- betterCentroid(group)
    }
  }
  return(newGroups)
}

bestCentroid <- function(row, centroids) {
  distancia <- sapply(1:nrow(centroids), function(i) {
    return(dist(rbind(row, centroids[i,])))
  })
  return(which.min(distancia))
}

betterCentroid <- function(group) {
  toReturn <- numeric(ncol(group))
  toReturn <- sapply(seq_along(toReturn), function(i) mean(group[ ,i]))
  return(toReturn)
}

set.seed(1)
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
  print(oneMetro$City)
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

ui <- fluidPage(
  titlePanel('Population Shifts over Time'),
  sidebarLayout(position = 'right',
                sidebarPanel(
                  h2('Using the Map', align = 'center'),
                  p("This map can be used to look at population changes over the history of the United States. The darker the blue and the larger the circle, the greater the population of its city.",
                    style = "font-family: 'times'; font-si16pt"),
                  p('Toggling the "Econ and Pop Group" button will display a whole bunch of markers. Each marker denotes a city with the highest population in its Metropolitan Area. The color of each marker denotes its group. Using',
                    em('kmeans'),
                    'clustering, these cities are grouped based on their population and economic output.',
                    style = "font-family: 'times'; font-si16pt"
                  )
                ),
                mainPanel(
                  leafletOutput("map")
                )
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet(m)
}

shinyApp(ui, server)