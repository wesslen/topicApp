# topicApp: A Simple Shiny App for Topic Modeling

![Screen Shot 1](img/screen-shot-1.png)

![Screen Shot 2](img/screen-shot-2.png)

![Screen Shot 3](img/screen-shot-3.png)


## Download and Running

To install and the run the app, open R/R Studio and run the following code:

```{r}
toInstall <- c("devtools","shiny","quanteda","shinydashboard","RColorBrewer","DT","treemap","visNetwork",
                "Matrix","igraph","wordcloud","scatterD3","reshape","grid","tidyverse",
                "shinyjs","shinyBS","stm")
install.packages(toInstall)
devtools::install_github("wesslen/topicApp")
topicApp::runApp()
```

## FAQ/Help

1.  I'm getting an error message when trying to install the `slam` package (a dependency for several other packages).

Try to run this command in your R (or R Studio) Console:

```{r}
slam_url <- "https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz" 
devtools::install_url(slam_url)
```

This should manually install the `slam` package.

2.  I want to use more features of the `stm` packages (e.g., include covariates).

This app has been created for only simple analyses (e.g., small datasets, no covariates, limited functionality). If you're interested in more functionality of the `stm` package, you should considering either running the code individually or use Dan Zangri's `stmgui` package (see [stmGUI GitHub](https://github.com/dzangri/stmGUI))
