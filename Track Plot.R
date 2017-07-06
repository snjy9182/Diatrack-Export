library(plotly)
packageVersion('plotly')

Sys.setenv("plotly_username"= "snjy9182")
Sys.setenv("plotly_api_key"= "qdIKaQCensVFAa3P2gCq")

trackll <- readDiaSessions()

linkedtrackll <- linkSkippedFrames(trackll, tolerance = 5, maxSkip = 10)

outputRowWise(linkedtrackll)

file = file.choose()

data = read.csv(file)

#######

p <- plot_ly(x = ~data$x, y = ~data$y, type = "scatter")
p

#######
x = data$x
y = data$y
s <- subplot(
    plot_ly(x = x, type = "histogram"),
    plotly_empty(),
    plot_ly(x = x, y = y, type = "histogram2dcontour"),
    plot_ly(y = y, type = "histogram"),
    nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2), margin = 0,
    shareX = TRUE, shareY = TRUE, titleX = FALSE, titleY = FALSE
)
s

#######

p <- plot_ly(x = ~linkedtrackll[[1]][[1]], y = ~linkedtrackll[[1]][[2]], name = 1, type = 'scatter', mode = 'lines', evaluation = TRUE)
for (i in 2:5)
    p <- add_trace(p, x = ~linkedtrackll[[i]][[1]], y = ~linkedtrackll[[i]][[2]], name = i, type = 'scatter', mode = 'lines', evaluation = TRUE)
p