library(shiny)
library(dplyr)
source("jobsearch.R")

#example set of weights to use if weights have not yet been set up
exampleWeights <- data.frame(
  keyword = c("IT", "(?i)programming", "(?i)comput[er|ing]"),
  tweight = c(10, 20, 4),
  dweight = c(5, 10, 0)
)

# Output date in the format "10 January"
outputDate <- function(date) {
  format(as.Date(as.integer(date), as.Date("1970-1-1")), "%d %b")
}


createJobLink <- function(url, saveLink) {  
  label <- if (saveLink) "Save" else "Delete"
  funcCall <- sprintf(ifelse(saveLink, "saveJob(parentNode, '%s')", "deleteJob(parentNode, '%s')"), url)
  sprintf("<a onclick=\"%s\">%s</a>", funcCall, label)
}

createSaveLinks <- function(url) {
#  paste0(sprintf("<span onpageshow=\"setupLinks(this, '%s')\">", url), 
#         createJobLink(url, TRUE), createJobLink(url, FALSE), "</span>")
  paste0(createJobLink(url, TRUE), createJobLink(url, FALSE))
}

read.weights <- function(path = weights.path) {
  weights <- if (file.exists(weightings.path)) read.csv(weightings.path) else exampleWeights
}

calcListingWeighting <- function(title, description, nfactor, weights) {  
  weight <- vector("integer", length(title))
  
  for (row in 1:nrow(weights)) {
    keyword <- weights[[row, "keyword"]]
    weight <- weight + 
      ifelse(grepl(keyword, title, ignore.case=TRUE), weights[[row, "tweight"]], 0)
    weight <- weight + 
      ifelse(grepl(keyword, description, ignore.case=TRUE), weights[[row, "dweight"]], 0)
  }  
  
  weight*nfactor
}

commitReweightedListings <- function(path = listings.path) {
  listings <- read.listings(path)
  listings <- reweightListings(listings)
  write.listings(listings, path)
}

reweightListings <- function(listings, weights = read.weights()) {
  engines <- load.engines()
  for (i in seq(1, nrow(listings))) {
    nfactor <- engines[sapply(engines, 
                              function(engine) 
                                engine$name == listings[[i, 'website']])][[1]]$nfactor
    listings[[i, "weighting"]] <- calcListingWeighting(listings[i, ], nfactor, weights)
  }
  
  listings
}

#Create a vector that is stored at the path specified
persistentVector <- function(path, default = NULL) {
  values <- NULL
  if (file.exists(path))
    load(path)
  else
    values <- default
  
  update <- function(newValues) {
      values <<- newValues
      save("values", file=path)
      values
  } 
  
  obj <- list(
    add = function(value) {
      update(c(values, value))
    },
  
    remove = function(value) {
      if (!is.null(value))
        update(values[values != value])
      else
        values
    },
    
    contains = function(value) {
      if (!is.null(value))
        value %in% values
      else
        FALSE
    },
  
    get.values = function() {
      values
    }
  )
  
  class(obj) <- append(class(obj), "persistentVector")
  obj
}
savedJobs.path <- paste0(base.dir, "savedJobs")

createLink <- function(id, value, text) {
  HTML(sprintf("<a onClick='Shiny.onInputChange(\"%s\", \"%s\")'>%s</a>", id, value, text))
}

createInput <- function(name, type, value) {
  js <- sprintf('Shiny.onInputChange("%s", value)', name)
  HTML(paste0(sprintf("<input name='%s' type='%s' value='%s' onchange='%s' />", 
               name, type, value, js, js)),
              sprintf("<script>Shiny.onInputChange('%s', '%s')</script>", name, value))
}

createWeightEditRow <- function(weights = read.weights()) {
  row <- as.integer(weights[["id"]])
  list(
    Pattern = createInput(paste0("pattern", row), "text", weights[["keyword"]]),
    "Title Weight" = createInput(paste0("tweight", row), "number", as.integer(weights[["tweight"]])),
    "Description Weight" = createInput(paste0("dweight", row), "number", as.integer(weights[["dweight"]])),
    Action = createLink("deleteWeight", row, "Delete")
  )
}

parseWeightsPage <- function(input) {
  row = 1
  weightings <- NULL
  while (!is.null(input[[paste0("pattern", row)]])) {
    weightings <- rbind(weightings, list(keyword = input[[paste0("pattern", row)]],
                                         tweight = as.integer(input[[paste0("tweight", row)]]),
                                         dweight = as.integer(input[[paste0("dweight", row)]])
    ))
    
    row <- row + 1
  }

  weightings
}

addPrettyFields <- function(listings = read.listings(), weights = read.weights()) {
  #createSaveLinksV <- Vectorize(createSaveLinks)
  mutate(listings, Job = sprintf("<a href='%s'>%s</a>", url, trim(title)),
                   Date = outputDate(datePosted),
                   Location = location,
                   Favourites = createSaveLinks(url),
                   weight = calcListingWeighting(title, description, 1, weights))
}

shinyServer(function(input, output, session) {
  #General table properties
  hide = list(visible = FALSE)
  columnProps <- list(hide, hide, hide, hide, hide, hide, hide, hide, NULL, NULL, NULL, NULL, hide)
  longTableOptions <- list(columns = columnProps, deferRender = TRUE,
                           createdRow = I("function(row, data, dataIndex) { setupLinks(row, data)}"))
  shortTableOptions <- append(longTableOptions, list(searching = FALSE, paging = FALSE, lengthChange = FALSE))

  if (!file.exists(prettyfields.path) || nrow(read.listings()) != nrow(read.csv(prettyfields.path))) {
    listingsTable <- addPrettyFields(read.listings())  
    write.csv(listingsTable, prettyfields.path, row.names = FALSE)
  }
  
  listingsTable <- read.csv(prettyfields.path)
  rValues <- reactiveValues(weights = read.weights())
  
  lastUpdate <- NULL
  listings <- reactivePoll(30000, session, function() {nrow(listingsTable)}, 
                           function() {listingsTable[with(listingsTable, order(-weight)), ]})
  
  #Setup tables
  output$listings <- renderDataTable({listings()}, longTableOptions, escape = FALSE)
  outputOptions(output, "listings", suspendWhenHidden = FALSE)  
  output$recent <- 
    renderDataTable({listings()[Sys.Date() - listings()$datePosted <= as.integer(input$recentDays),]}, 
                    longTableOptions, escape = FALSE)
  
  #Saved jobs
  savedJobs = persistentVector(savedJobs.path)
  observe({input$saveJob; rValues$savedJobs <- savedJobs$add(input$savejob)})
  observe({input$deleteJob; rValues$savedJobs <- savedJobs$remove(input$deletejob)})  
  
  output$savedJobs <-renderDataTable({rValues$savedJobs 
                                      listings()[savedJobs$contains(listings()$url), ]}, 
                                     longTableOptions, escape = FALSE)
  
  #Random jobs
  set.seed(Sys.time())
  values <- reactive({input$newrandom; as.integer(rbeta(10, 5, 1) * nrow(listings()))})
  output$random <- renderDataTable({listings()[values(), ]}, shortTableOptions, escape = FALSE)
  
  #Loading new jobs
  startLoadJobsObs <- observeEvent(input$search, {    
      progress <- shiny::Progress$new()
      listingsLoader <- createLALProcess(parallel = F)
      
      updateProgress <- reactiveTimer(1000, session)
      newJobObs <- observe({        
        updateProgress()
        update <- listingsLoader$update()        
        if (update$finished) {          
          progress$close()
          newListings <- mccollect(listingsLoader$process)[[1]]
          mcparallel({write.csv(addPrettyFields(newListings), prettyfields.path, row.names = FALSE)})          
          newJobObs$destroy()
          startLoadJobsObs$resume()
        } else {
          progress$set(update$stats$progress, sprintf("%d new jobs found", update$stats$jobCount))
          
          if (!is.null(update$newListings)) {
            newListings <- addPrettyFields(update$newListings)
            listingsTable <<- rbind(listingsTable, newListings)

          }
        }
      })
  
      startLoadJobsObs$suspend()
  })
  
  #Adjust weightings
  weightTableOptions <- list(searching = FALSE, paging = FALSE, lengthChange = FALSE)
  output$weights <- renderDataTable(
    { weights <-cbind(rValues$weights, id = 1:nrow(rValues$weights))
      do.call(rbind, apply(weights, 1, createWeightEditRow))}, 
    options = weightTableOptions, escape = FALSE)
  observeEvent(input$addWeight, 
    {rValues$weights <- rbind(rValues$weights, list(keyword = "", tweight = 0, dweight = 0))})
  observeEvent(input$deleteWeight, 
               {rValues$weights <- rValues$weights[-as.integer(input$deleteWeight),]})
  observeEvent(input$saveWeights, 
               {newWeights <- parseWeightsPage(input)                
                rValues$weights <- newWeights 
                listings <<- reweightListings(listings, newWeights)
                write.csv(newWeights, weightings.path, row.names=FALSE)})  
  
})