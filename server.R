library(shiny)
source("jobsearch.R")

createLink <- function(id, value, text) {
  HTML(sprintf("<a onClick='Shiny.onInputChange(\"%s\", \"%s\")'>%s</a>", id, value, text))
}

createInput <- function(name, type, value) {
  js <- sprintf('Shiny.onInputChange("%s", value)', name)
  HTML(paste0(sprintf("<input name='%s' type='%s' value='%s' onchange='%s' />", 
               name, type, value, js, js)),
              sprintf("<script>Shiny.onInputChange('%s', '%s')</script>", name, value))
}

createWeightEditRow <- function(weights) {
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

shinyServer(function(input, output, session) {
  #General table properties
  hide = list(visible = FALSE)
  columnProps <- list(hide, hide, hide, hide, hide, hide, hide, hide, hide, NULL, NULL, NULL, NULL)
  longTableOptions <- list(searching = TRUE, paging = TRUE, lengthChange = TRUE, 
                           columns = columnProps)
  shortTableOptions <- list(searching = FALSE, paging = FALSE, lengthChange = FALSE, 
                            columns = columnProps)
  
  jobData <- loadJobData()
  rValues <- reactiveValues(weights = jobData$weightings)
  
  lastUpdate <- NULL
  listings <- reactivePoll(30000, session, function() {nrow(jobData$listings)}, function() {jobData$listings})
  
  #Setup tables
  output$listings <- renderDataTable({listings()}, longTableOptions, escape = FALSE)
  output$recent <- 
    renderDataTable({listings()[Sys.Date() - listings()$datePosted <= as.integer(input$recentDays),]}, 
                    longTableOptions, escape = FALSE)
  
  #Saved jobs
  savedJobs <- reactive({listings()[listings()$url %in% getSavedJobs(), ]})
  
  observe({if (!is.null(input$savejob)) 
    jobData$listings[jobData$listings$url %in% saveJob(input$savejob), ]})
  observe({if (!is.null(input$deletejob)) 
    jobData$listings[jobData$listings$url %in% deleteJob(input$deletejob), ]})  
  
  output$savedJobs <-renderDataTable({savedJobs()}, longTableOptions, escape = FALSE)
  
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
          jobData$listings <<-mccollect(listingsLoader$process)[[1]]
          newJobObs$destroy()
          startLoadJobsObs$resume()
        } else {
          progress$set(update$stats$progress, sprintf("%d new jobs found", update$stats$jobCount))
          jobData$listings <<- rbind(jobData$listings, update$newListings)
        }              
      })
  
      startLoadJobsObs$suspend()
  })
  
  #Adjust weightings
  weightTableOptions <- list(searching = FALSE, paging = FALSE, lengthChange = FALSE)
  output$weights <- renderDataTable(
    { weights <-cbind(rValues$weights, id = seq(1, nrow(rValues$weights)))
      do.call(rbind, apply(weights, 1, createWeightEditRow))}, 
    options = weightTableOptions, escape = FALSE)
  observeEvent(input$addWeight, 
    {rValues$weights <- rbind(rValues$weights, list(keyword = "", tweight = 0, dweight = 0))})
  observeEvent(input$deleteWeight, 
               {rValues$weights <- rValues$weights[-as.integer(input$deleteWeight),]})
  observeEvent(input$saveWeights, 
               {newWeightings <- parseWeightsPage(input)                
                jobData$weightings <- rValues$weights <- newWeightings 
                jobData$listings <- reweightListings(jobData)
                rValues$listings <- jobData$listings                
                write.csv(jobData$weightings, "data/weightings.csv", row.names=FALSE)})  
  
})