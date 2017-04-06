# topicApp: A Simple Shiny App for Topic Modeling

## Download and Running

To install and the run the app, open R/R Studio and run the following code:

```{r}
toInstall <- c("devtools","shiny","shinyBS","shinyjs")
install.packages("devtools")
devtools::install_github("wesslen/topicApp")
shiny::runApp('topicApp')
```
