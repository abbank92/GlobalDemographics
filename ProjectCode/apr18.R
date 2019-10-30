#apr18

regionalMap <- function(k) {
  groupos <- kmeans(threeVars, k)
  colores <- rainbow(k, alpha = 0.2)
  
  map("world", fill = TRUE, col = "grey")
  
  
  for (i in 1:k) {
    y <- threeVars[which(groupos == i),1]
    x <- threeVars[which(groupos == i),2]
    inds <- chull(x, y)
    polygon(x[inds], y[inds], col = colores[i])
  }
  
  symbols(countryData$longitude, countryData$latitude,
          add = TRUE, inches = FALSE, circles = radii,
          fg = mortColors, bg = mortColors)
}

regionalMap(5)
