library(plotly)
packageVersion('plotly')

Sys.setenv("plotly_username"= "snjy9182")
Sys.setenv("plotly_api_key"= "qdIKaQCensVFAa3P2gCq")

p <- plot_ly(x = ~linkedtrackll[[1]][[1]], y = ~linkedtrackll[[1]][[2]], name = 1, type = 'scatter', mode = 'lines')

for (i in 2:6){
    addLine(p, i);
}


addLine = function(p, i){
    p <- add_trace(p, x = ~linkedtrackll[[i]][[1]], y = ~linkedtrackll[[i]][[2]], name = i, type = 'scatter', mode = 'lines')
}
q
# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = plotly_POST(p, filename="line/basic")
chart_link