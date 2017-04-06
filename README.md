# topicApp: A Simple Shiny App for Topic Modeling

## Download and Running

To install and the run the app, open R/R Studio and run the following code:

```{r}
toInstall <- c("devtools","shiny","quanteda","shinydashboard","RColorBrewer","DT","treemap","visNetwork","Matrix",
              "igraph","wordcloud","scatterD3","reshape","grid","tidyverse","shinyjs","shinyBS","stm")
install.packages(toInstall)
devtools::install_github("wesslen/topicApp")
topicApp::runApp()
```
