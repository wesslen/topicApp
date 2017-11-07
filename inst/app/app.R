
#packages
packages <- c("shiny","quanteda","shinydashboard","RColorBrewer","DT","treemap","visNetwork","d3wordcloud",
              "igraph","wordcloud","scatterD3","reshape","grid","tidyverse","shinyjs","shinyBS","stm")

#for (i in packages){install.packages(i)}, devtools::install_github("jbkunst/d3wordcloud")

lapply(packages,library,character.only = TRUE)
source('directoryInput.R')
source('functions.R')

#source("./inst/app/functions.R")

# put stop words to start with here
exp.stop <- c()

###################################################
##############       UI       #####################
###################################################

############### Header content ####################

header <- dashboardHeader(title = "topicApp")

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
                    ),
                  hr(),
                      directoryInput('load.directory', label = 'Or load a previous model (then move to Step 4)', value = '~'),
                      bsTooltip("load.directory", "Select the directory to load a model.",
                                "left", options = list(container = "body")),
                      actionButton("load.model","Load Model")
                  
                  ),
              box(title = "Step 2: Pre-processing",
                  selectInput("tpDocs",
                              "Select Text Column",
                              c()),
                  bsTooltip("tpDocs", "Select which column contains the column of text.",
                            "left", options = list(container = "body")),
                  textInput("stopwords", label = "Stop Words", 
                            value = paste(exp.stop, collapse = ", "),
                            placeholder = "also, such, really..."),
                  bsTooltip("stopwords", "Include additional stop words to remove:",
                            "left", options = list(container = "body")),
                  sliderInput("minDoc",
                              "Minimum # of Documents (for Terms):",
                              min = 0,  max = 100,  value = 10, step = 1),
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
                                min = 0,  max = 100,  value = 10, step = 1),
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
              #actionButton("tabBut", "View Topic"),
              box(title = "Topic Network", 
                  visNetworkOutput("topic.network", height = "400px"), width = 12, collapsible = F)
              ),
             fluidRow(
               box(title = "Topic Word Cloud: Size Proportional to Word Probability", 
                   #d3wordcloudOutput("topic.wordcloud", height = "200px"), 
                   plotOutput("topic.wordcloud"),
                   width = 12
                   )
               ),
            fluidRow(
              box(title = "Representative Documents",
                  dataTableOutput("doc.table"), width = 12
                  )
                )
            )
    ,
    # # Document Tab 
    # tabItem(tabName = "document",
    #         fluidRow(
    #           box(
    #             title = "Document Attributes",
    #             #selectInput("document", "Choose a document:", choices = cmpyData$Company),
    #             dataTableOutput("doc.attribute"),
    #             height = 400),
    #           box(
    #             title = "Document Topics", 
    #             plotOutput("doc.treemap")),
    #           height = 400,
    #           collapsible = T
    #         )
    #         ,
    #         fluidRow(    
    #           box(title = "Document's Webpages", dataTableOutput("company.webpage"), width = 12, collapsible = T)
    #         )
    # ),
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

  # load previous model
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$load.directory
    },
    handlerExpr = {
      if (input$load.directory > 0) {
        # condition prevents handler execution on initial app launch
        
        path = choose.dir(default = readDirectoryInput(session, 'load.directory'))
        updateDirectoryInput(session, 'load.directory', value = path)
      }
    }
  )
  
  output$directory = renderText({
    readDirectoryInput(session, 'load.directory')
  })
  
  # save model
  
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
  
  # shinyjs below was from stmGUI: https://github.com/dzangri/stmGUI
  
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
        storedData$data$rowNum <- 1:nrow(storedData$data)
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
    
    # sets input data row number as primary key -- ensures matchback for datasets without a primary key
    docvars(MyCorpus, "rowNum") <- storedData$data$rowNum
    stp <- unlist(strsplit(input$stopwords,","))
    stp <- trimws(stp)
    ngram <- ifelse(input$ngrams==1,1L, 1L:2L)
    
    Dfm <- dfm(MyCorpus, remove = c(stopwords("english"), stp), remove_numbers = TRUE, remove_punct = TRUE,
                  stem = input$stemming, ngrams = ngram
               )
  
    tdfm <- dfm_trim(Dfm, min_docfreq = input$minDoc)

    # we now export to a format that we can run the topic model with
    z$Corpus <- MyCorpus
    z$dtm <- convert(tdfm, to= "topicmodels")
    z$dfm <- convert(tdfm, to = "stm", docvars = docvars(MyCorpus))
    
    print("DFM created")
  })
  
  v <- reactiveValues(probtopics = NULL, probterms = NULL, topicnames = NULL, stmFit = NULL, out = NULL)
  
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
  
  # load and save
  
  observeEvent(input$load.model, {
    dir <- readDirectoryInput(session, 'load.directory')
    v$probterms <- read.csv(file = paste0(dir,"/prob-terms.csv"), stringsAsFactors = F, row.names = 1)
    v$probdocs <- read.csv(file = paste0(dir,"/prob-docs.csv"), stringsAsFactors = F, row.names = 1)
    load(paste0(dir,"/stmFit.RData"))
    v$stmFit <- stmFit
    load(paste0(dir,"/out.RData"))
    v$out <- out

    topic.names <- character(length = ncol(v$probterms))
    for (i in 1:ncol(v$probterms)){
      temp <- order(-v$probterms[,i])
      temp2 <- rownames(v$probterms[temp,])
      topic.names[i] <- paste(temp2[1:5], collapse = " \n ")
    }
    v$topicnames <- topic.names
    
    print("Model Uploaded!")
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
  
  ### Network
  
  output$topic.network <- renderVisNetwork({

    visNetwork(x$nodes, x$edges, submain = "A topic is a word list of word co-occurrence clusters. Each node is a topic and each edge represents shared words between clusters.", height = "600px") %>%
      #visExport() %>%
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
    data.frame(docname = rownames(v$probdocs), freq = freq[,temp], rowNum = v$out$meta$rowNum)
  })
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)

  output$topic.wordcloud <- renderPlot({
    w <- terms()

    try <- try(wordcloud_rep(w$word,
                             exp(w$freq),
                             scale=c(4,0.5),
                             max.words=100,
                             random.order = F,
                             rot.per=0.1,
                             colors=brewer.pal(8, "Dark2")))
    if("try-error" %in% class(try)){print("Choose a topic from the network.")
    }else{wordcloud_rep(w$word,
                        exp(w$freq),
                        scale=c(4,0.5),
                        max.words=50,
                        random.order = F,
                        rot.per=0.1,
                        colors=brewer.pal(8, "Dark2"))}
    })

  #### test for new word cloud
  # wordcloud_rep <- repeatable(d3wordcloud)
  # #rm(output$topic.wordcloud)
  # output$topic.wordcloud <- renderD3wordcloud({
  #  #wordcloud_rep(c(" "),1)
  #  w <- terms()
  #  
  #  # reweight
  #  w$freq <- exp(w$freq)
  #  w <- w[order(desc(w$freq)),]
  #  
  #  # take top 100
  #  w <- w[1:100,]
  #  #wordcloud_rep(c(" "),1)
  #  
  #  runjs("$('#wordcloud svg g').empty()") 
  #  
  #  x <- NULL
  #  x <- d3wordcloud(w$word,
  #                w$freq,
  #                size.scale = "linear",
  #                color.scale = "linear",
  #                rotate.max = 0,
  #                rotate.min = 0,
  #                height = "200px",
  #                spiral = "rectangular"
  #  )
  #  
  #  x
  #  # try <- try(x)
  #  # if("try-error" %in% class(try)){print("Choose a topic from the network.")}else{x}
  #  
  # })

  
  # expert table
  Docs <- reactive({
    d <- docs()
    ldaProbs <- data.frame(rowNum = d$rowNum, Prob = exp(d$freq), stringsAsFactors = F)
    ldaProbs <- merge(ldaProbs, storedData$data, by = "rowNum")
    ldaProbs[order(ldaProbs$Prob, decreasing = T), c("rowNum","Prob",input$tpDocs)]
  })
  
  #Representative Document
  
  output$doc.table <- renderDataTable({
    temp <- Docs()
    colnames(temp) <- c("Row Num","Topic Prob","Text")
    temp[,2] <- round(log(temp[,2]),3)
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
    
    valid$results <- searchK(out$documents, 
                             out$vocab, 
                             K, 
                             init.type = "Spectral", 
                             proportion = 0.5, 
                             heldout.seed = input$search.seed, 
                             max.em.its = 200)
    valid$K <- K
    })
  
  output$valid.plot <- renderPlot({
    try <- try(plot(valid$results))
    if("try-error" %in% class(try)){print("Select the number of topics to test and run topic validation.")
    }else{plot(valid$results)}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


