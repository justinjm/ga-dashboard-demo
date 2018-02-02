packages_list <- c("devtools",
                   "stringr",
                   "xts",
                   "zoo",
                   "tidyverse",
                   "lubridate",
                   "scales",
                   "shiny",
                   "googleAuthR",
                   "googleCloudStorageR",
                   "bigQueryR",
                   "googleAnalyticsR",
                   "slackr",
                   "shiny",
                   "DT",
                   "shinydashboard",
                   "flexdashboard",
                   "ggplot2",
                   "dygraphs",
                   "d3heatmap",
                   "highcharter",
                   "CausalImpact",
                   "forecast")

new_packages <- packages_list[!(packages_list %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

devtools::install_github("twitter/AnomalyDetection")
library(AnomalyDetection)
