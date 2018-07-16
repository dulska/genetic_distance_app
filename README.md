# genetic_distance_app
Shiny app to appoint the genetic distance and shortest pathway between different individuals using Dijkstra alghoritm.

Required packages:
```
list.of.packages <- c("DT","data.table", "stringr", "stringi", "dplyr", "ggplot2","shiny", "openxlsx", "d3heatmap", "shinyjs", "reshape2", "plyr", "scales", "V8", "geosphere", "network", "igraph", "tidyr", "plotly")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
```

To run a Shiny app type:
```
library(shiny)
runGitHub("genetic_distance_app", "dulska")
```
