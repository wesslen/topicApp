
#packages
packages <- c("shiny","quanteda","shinydashboard","RColorBrewer","DT","treemap","visNetwork",
              "igraph","wordcloud","scatterD3","reshape","grid","tidyverse","shinyjs","shinyBS","stm")

#for (i in packages){install.packages(i)}

lapply(packages,library,character.only = TRUE)
source('directoryInput.R')
source('functions.R')

#source("./inst/app/functions.R")

exp.stop <- c("also")

###################################################
##############       UI       #####################
###################################################

############### Header content ####################

header <- dashboardHeader(title = "Shiny Topic Model")

############### Sidebar content ###################

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Model Parameters", tabName = "model", icon = icon("tasks")),
    menuItem("Topics", tabName = "topics", icon = icon("th")),
    #menuItem("Document-Level", tabName = "companies", icon = icon("users")),
    menuItem("Validation", tabName = "validation", icon = icon("check"))
  )
)

############### Body content ######################

body <-   dashboardBody(
  tabItems(
    # Topic Modeling Tab
    
    tabItem(tabName = "model",
            fluidRow(
              box(title = "Step 1: Load Dataset",
                  column(9,
                         fileInput("dataFileToUpload", "Choose Data File To Upload")
                  ),
                  hr(),
                  # Code below was from stmGUI: https://github.com/dzangri/stmGUI
                  actionButton("submitDataForUpload", "Submit"),
                  hr(),
                  #a(id = "toggleAdvDataUpload", "Show/hide advanced options"),
                    div(id = "advUploadOptions",
                        checkboxInput("headerPresent", "Header Row Present", TRUE),
                        radioButtons("columnSeparator",
                                     "Separator",
                                     c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                                     inline = TRUE,
                                     ","),
                        radioButtons("quoteAroundData", "Quotes Around Text",
                                     c(None = "",
                                       "Double Quote" = "\"",
                                       "Single Quote" = "'"),
                                     inline = TRUE,
                                     "\"")
                    )
                  ),
              box(title = "Step 2: Pre-processing",
                  selectInput("tpDocs",
                              "Select Text Column",
                              c()),
                  bsTooltip("tpDocs", "Select which column contains the vector of text.",
                            "left", options = list(container = "body")),
                  textInput("stopwords", label = "Stop Words", 
                            value = paste(exp.stop, collapse = ", ")),
                  bsTooltip("stopwords", "Include additional stop words to remove:",
                            "left", options = list(container = "body")),
                  sliderInput("minDoc",
                              "Minimum # of Documents (for Terms):",
                              min = 0,  max = 100,  value = 10, step = 5),
                  bsTooltip("minDoc", "Remove sparse terms:",
                            "left", options = list(container = "body")),
                  box(checkboxInput("stemming", label = "Stemming", value = FALSE),
                  radioButtons("ngrams", label = "n-grams",
                               choices = list("Unigrams" = 1, "Bigrams" = 2), selected = 1)),
                  box(actionButton("dfm.update", "Create DFM"))
                  )
              ),
              fluidRow(
              box(title = "Step 3: Topic Model",
                  column(1.5,
                    sliderInput("num.topics",
                                "Number of Topics:",
                                min = 0,  max = 100,  value = 10, step = 5),
                    bsTooltip("num.topics", "Set to zero to auto-detect topics.",
                              "left", options = list(container = "body"))),
                  column(1.5,
                    sliderInput("iter",
                                "Maximum Number of Iterations:",
                                min = 20,  max = 200,  value = 100, step = 20),
                    bsTooltip("iter", "Adjust higher if the algorithm is not converging.",
                              "left", options = list(container = "body"))),
                    hr(),
                    actionButton("topic.update", "Run Model")
              ),
              box(title = "Step 4: Topic Network Settings",
                  sliderInput("parm",
                              "Minimum Correlation",
                              min = 0,  max = 0.2,  value = 0.1, step = 0.01),
                  bsTooltip("parm", "Higher threshold means less edges, Lower means more edges.",
                            "left", options = list(container = "body")),
                  hr(),
                  actionButton("network.update", "Create Network")
                ),
              box(title = "Save results",
                  
                  directoryInput('directory', label = 'Selected Directory', value = '~'),
                  bsTooltip("directory", "Select the directory  to save the results.",
                            "left", options = list(container = "body")),
                  
                  actionButton("save.results","Save Model")
                  )
              )
            )
    ,
    # Topics Tab
    tabItem(tabName = "topics",
            fluidRow(
              box(title = "Topic Network", 
                  visNetworkOutput("topic.network", height = "800px"), width = 12, collapsible = T)
                    ),
            fluidRow(
              box(title = "Topic Word Cloud", 
                  plotOutput("topic.wordcloud"),
                  sliderInput("max.words",
                              "Maximum Number of Words:",
                              min = 20,  max = 200,  value = 100, step = 20)),
              box(title = "Representative Documents",
                  dataTableOutput("doc.table")
                  #scatterD3Output("company.plot", height = "450px"),
                  )
                )
            )
    ,
    # Company Tab 
    tabItem(tabName = "companies",
            fluidRow(
              box(
                title = "Company Attributes",
                #selectInput("company", "Choose a company:", choices = cmpyData$Company),
                dataTableOutput("company.attribute"),
                height = 400),
              box(
                title = "Company Topics", 
                plotOutput("company.treemap")),
              height = 400,
              collapsible = T
            )
            ,
            fluidRow(    
              box(title = "Company's Webpages", dataTableOutput("company.webpage"), width = 12, collapsible = T)
            )
    ),
    # Validation tab
    tabItem(tabName = "validation",
            fluidRow(
              box(
                title = "Topic Validation",
                checkboxGroupInput("k.validation", "K Topics to Run:",
                                   c("5" = 1,
                                     "10" = 2,
                                     "20" = 3,
                                     "30" = 4,
                                     "50" = 5,
                                     "75" = 6,
                                     "100" = 7), selected = c(1,2,3,4,5,6,7), inline = TRUE),
                numericInput("search.seed", label = "Seed", value = 1),
                bsTooltip("search.seed", "Random seed used to partition dataset for Cross-Validation",
                          "left", options = list(container = "body")),
                hr(),
                actionButton("run.validation", "Run Validation")
              )
            ),
            fluidRow(
              box(
                plotOutput('valid.plot'), width = 12
                )
              )
            )
    
    )
)


############### Dashboard page ####################

ui <- dashboardPage(header, sidebar, body)

##############    SERVER       #####################

server <- function(input, output, session) {

  # reactive object that stores intermediate results
  storedData <- reactiveValues()
  
  storedData$data <- NULL
  # storedData$textprocess <- NULL
  # storedData$prepdocs <- NULL
  # storedData$stmresult <- NULL
  # storedData$stmformula <- NULL
  # storedData$esteffect <- NULL
  # 
  # storedData$summaryPlotArgs <- NULL
  # storedData$labelsPlotArgs <- NULL
  # storedData$perspectivesPlotArgs <- NULL
  # storedData$histPlotArgs <- NULL
  # storedData$estEffPlotArgs <- NULL
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch
        
        path = choose.dir(default = readDirectoryInput(session, 'directory'))
        updateDirectoryInput(session, 'directory', value = path)
      }
    }
  )
  
  output$directory = renderText({
    readDirectoryInput(session, 'directory')
  })
  

  
  
  
  shinyjs::onclick("toggleAdvDataUpload",
                   shinyjs::toggle(id = "advUploadOptions",
                                   anim = TRUE))
  observe({
    shinyjs::toggleState("submitDataForUpload",
                         !is.null(input$dataFileToUpload))
  })
  observe({
    shinyjs::toggleState("dataInputTitle-nextStep",
                         !is.null(storedData$data))
  })
  
  observeEvent(input$submitDataForUpload, ({
    shinyjs::html("dataInputTextResult", "")
    
    userData <- input$dataFileToUpload
    
    withProgress(message = "Loading data, please wait...", {
      setProgress(0.5)
      
      readDataArgs <- list(userData$datapath, header = input$headerPresent, sep = input$columnSeparator,
                           quote = input$quoteAroundData)
      
      shinyjs::toggleState("moveFromStep1To2")
      
      tryCatch({
        storedData$data <- do.call(read.csv, readDataArgs)
      }, error = function(e) {
        funName <- deparse(substitute(read.csv))
        shinyjs::html("dataInputTextResult",
                      paste("ERROR: Error while running '",
                            funName, "':\n",
                            e,
                            sep = ""))
        storedData$data <- NULL
        return(NULL)
      }, warning = function(w) {
        shinyjs::html("dataInputTextResult",
                      paste("WARNING: Warning while reading data:\n",
                            w,
                            sep = "\n"))
        storedData$data <- NULL
        return(NULL)
      }, finally = {
      })
      
      setProgress(1)
    })

  }))
  
  observe({
    userData <- storedData$data
    if (!is.null(userData)) {
      shinyjs::enable("tpDocs")
      dataColumnNames <- colnames(userData)
      updateSelectInput(session, "tpDocs", choices = dataColumnNames)
    } else {
      shinyjs::disable("tpDocs")
    }
  })
  
  
    # Topic
  z <- reactiveValues(Corpus = NULL, dtm = NULL, dfm = NULL)

  observeEvent(input$dfm.update, {
    
    MyCorpus <- corpus(as.character(storedData$data[,input$tpDocs]))
    #docvars(MyCorpus, "Company") <- data$Company
    
    stp <- unlist(strsplit(input$stopwords,","))
    stp <- trimws(stp)
    
    ngram <- ifelse(input$ngrams==1,1L, 1L:2L)
    
    Dfm <- dfm(MyCorpus, remove = c(stopwords("english"), stp), removeNumbers = TRUE, removePunct = TRUE,
                  stem = input$stemming, ngrams = ngram
               )
  
    tdfm <- dfm_trim(Dfm, min_docfreq = input$minDoc)

    # we now export to a format that we can run the topic model with
    z$Corpus <- MyCorpus
    z$dtm <- convert(tdfm, to="topicmodels")
    z$dfm <- convert(tdfm, to = "stm") #, docvars = docvars(myCorpus))
    
    print("DFM created")
  })
  
  
  v <- reactiveValues(probtopics = NULL, probterms = NULL, topicnames = NULL, stmFit = NULL, out = NULL)
  
  observeEvent(input$load.model, {
    v$probterms <- read.csv(file = "./prob-terms.csv", stringsAsFactors = F, row.names = 1)
    v$probdocs <- read.csv(file = "./prob-docs.csv", stringsAsFactors = F, row.names = 1)
    v$stmFit <- load("stmFit.RData")
    #v$topicnames <- read.csv(file = "./topic-names.csv", stringsAsFactors = F)
    
    topic.names <- character(length = ncol(v$probterms))
    
    for (i in 1:ncol(v$probterms)){
      temp <- order(-v$probterms[,i])
      temp2 <- rownames(v$probterms[temp,])
      topic.names[i] <- paste(temp2[1:5], collapse = " \n ")
    }
    
    v$topicnames <- topic.names
    
    print("Model CSV Uploaded!")
  })
  
  # topic models
  
  observeEvent(input$topic.update, {
    k <- input$num.topics
    dfm <- z$dfm
    
    # use quanteda converter to convert our Dfm
    out <- prepDocuments(dfm$documents, dfm$vocab, dfm$meta, lower.thresh = 1, subsample = NULL)
    
    stmFit <- stm(out$documents, out$vocab, K = k, #prevalence =~ Party + s(Time),
                  max.em.its = input$iter, data = out$meta, init.type = "Spectral", seed = 300)
    
    probterms <- data.frame(t(data.frame(probs = stmFit$beta[[1]])))  # words (rows) x topics (columns)
    row.names(probterms) <- stmFit$vocab
    probdocs <- data.frame(stmFit$theta)
    
    topic.names <- character(length = ncol(stmFit$theta))
    
    for (i in 1:ncol(stmFit$theta)){
      temp <- order(-probterms[,i])
      temp2 <- rownames(probterms[temp,])
      topic.names[i] <- paste(temp2[1:5], collapse = " \n ")
    }
    v$out <- out
    v$stmFit <- stmFit
    v$probdocs <- probdocs
    v$probterms <- probterms
    v$topicnames <- topic.names
  })

  # Network
  x <- reactiveValues(nodes = NULL, edges = NULL)
  
  observeEvent(input$network.update, {
   results <- new.topic.network(v$stmFit, input$parm, v$topicnames)
   x$nodes <- results[[1]]
   x$edges <- results[[2]]
   print("Network created")
  })
  
  

  observeEvent(input$save.results, {

    dir <- readDirectoryInput(session, 'directory')
    
    dir.terms <- paste0(dir,"/prob-terms.csv")
    dir.docs <- paste0(dir,"/prob-docs.csv")
    dir.topics <- paste0(dir,"/topic-names.csv")
    dir.parms <- paste0(dir,"/sparameters.csv")
    
    write.csv(v$probterms, dir.terms, row.names = T)
    write.csv(v$probdocs, dir.docs, row.names = T)
    write.csv(v$topicnames, dir.topics, row.names = F)
    parameters <- data.frame(Stopwords = input$stopwords,
                             minDoc = input$minDoc,
                             stem = input$stemming,
                             unigrams = input$ngrams,
                             NumTopics = input$num.topics,
                             Iterations = input$iter)
    
    write.csv(parameters, dir.parms, row.names = F)
     
    stmFit <- v$stmFit
    out <- v$out
    save(stmFit, file = paste0(dir,"/stmFit.RData"))
    save(out, file = paste0(dir,"/out.RData"))
    
    print("Topic model saved")
  })
  
  output$topic.network <- renderVisNetwork({

    visNetwork(x$nodes, x$edges, height = "1200px") %>%
      visNodes(labelHighlightBold = T) %>%
      visOptions(highlightNearest = T, selectedBy = "community", nodesIdSelection = T) %>%
      visInteraction(navigationButtons = T)
  })
  
  terms <- reactive({
    freq <- data.frame(v$probterms)
    temp <- as.integer(input$topic.network_selected)
    data.frame(word = rownames(v$probterms), freq = freq[,temp])
  })
  
  docs <- reactive({
    freq <- data.frame(v$probdocs)
    temp <- as.integer(input$topic.network_selected)
    data.frame(docname = rownames(v$probdocs), freq = freq[,temp])
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  
  output$topic.wordcloud <- renderPlot({
    w <- terms()
    wordcloud_rep(w$word, exp(w$freq), scale=c(4,0.5),
                  max.words=input$max.words,random.order = F, rot.per=0.1,
                  colors=brewer.pal(8, "Dark2"))
    })

  # expert table
  Docs <- reactive({
    d <- docs()
    ldaProbs <- data.frame(ID = as.integer(row.names(d)), Prob = exp(d$freq), stringsAsFactors = F)
    ldaProbs <- merge(ldaProbs, storedData$data, by = "ID")
    ldaProbs[order(ldaProbs$Prob, decreasing = T),]
  })
  
  #Representative Document
  
  output$doc.table <- renderDataTable({
    temp <- Docs()
    colnames(temp) <- c("ID","% Topic","Text")
    temp$`% Topic` <- round(temp$`% Topic`,3)
    temp$Text <- as.character(temp$Text)
    temp
  }, options = list(pageLength = 5, dom = 'tip') , rownames= FALSE)
  
  
  
  valid <- reactiveValues(results = NULL, K = NULL)
  
  # Validation
  
  observeEvent(input$run.validation, {
  
    K <- c(5,10,20,30,50,75,100)
    K <- K[as.numeric(input$k.validation)]
    
    dfm <- z$dfm
    
    # use quanteda converter to convert our Dfm
    out <- prepDocuments(dfm$documents, dfm$vocab, dfm$meta, lower.thresh = 1, subsample = NULL)
    
    valid$results <- searchK(out$documents, out$vocab, K, init.type = "Spectral", proportion = 0.5, heldout.seed = input$search.seed)
    valid$K <- K
    })
  
  output$valid.plot <- renderPlot({
    plot(valid$results)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


