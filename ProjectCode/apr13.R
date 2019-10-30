#apr13

library(maps)
library(RColorBrewer)
display.brewer.all()

cortar <- quantile(countryData$mortality, probs = seq(0,1,0.125))
cortar[1] <- cortar[1]-0.0005
mortBreaks <- cut(countryData$mortality, cortar)
colores <- brewer.pal(8, "Reds")
mortColors <- colores[mortBreaks]

#deliverable 6
table(mortBreaks)


map("world", fill = TRUE, col = "grey")
symbols(countryData$longitude, countryData$latitude,
        add = TRUE, inches = FALSE, circles = rep.int(2, length(countryData$latitude)),
        fg = mortColors, bg = mortColors)
legend("topleft", legend = levels(mortBreaks), col = colores, pch = 19, cex = .5, title = 'Levels')
