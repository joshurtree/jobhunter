#setwd(dirname(parent.frame(2)$ofile))
library(XML)
library(shiny)
library(parallel)
options(stringsAsFactors = FALSE)
source("engines.R")

#example set of weights to use if weights have not yet been set up
exampleWeights <- data.frame(
  keyword = c("IT", "(?i)Programming", "(?i)comput[er|ing]"),
  tweight = c(10, 20, 4),
  dweight = c(5, 10, 0)
)

# remove whitespace from both ends of a string
trim <- function(value) {
  gsub("^\\s*|\\s*$", "", value)
}

# Output date in the format "10 January"
outputDate <- function(date) {
  format(as.Date(as.integer(date), as.Date("1970-1-1")), "%d %b")
}

createJobLink <- function(url, save) {
  label <- if (save) "Save" else "Delete"
  funcCall <- sprintf(
    if (save) 
      "saveJob('%s'); %s; nextSibling.style.display = 'initial'" 
    else 
      "deleteJob('%s'); %s; previousSibling.style.display = 'initial'", 
    url, "this.style.display = 'none'")
  style <- if (save != (url %in% getSavedJobs())) "" else "style='display : none'"
  
  sprintf("<a onClick=\"%s\" %s>%s</a>", 
          funcCall, style, label)
}

createSaveLinks <- function(url) {
  HTML(paste0(createJobLink(url, TRUE), createJobLink(url, FALSE)))
}

base.dir <- "~/.jobhunter/"
if (!dir.exists(base.dir)) dir.create(base.dir)
weightings.path <- paste0(base.dir, "weightings.csv")
listings.path <- paste0(base.dir, "listings.csv")

# Load information on engines, weights and job listings
loadJobData <- function() {
  weights <- if (file.exists(weightings.path)) read.csv(weightings.path) else exampleWeights
  lists <- if (file.exists(listings.path)) { 
    read.csv(listings.path, row.names = 1) 
  } else 
    data.frame()
  
  list(listings = lists, weightings = weights,  engines = allEngines)
}

commitListings <- function(listings, jobData) {    
  jobData$listings <- merge(jobData$listings, listings, all = TRUE)

  # Remove old and duplicate listings
  jobData$listings <- jobData$listings[(Sys.Date() - jobData$listings$datePosted) <= 28, ]
  jobData$listings <- jobData$listings[!duplicated(jobData$listings[c("description", "location")]), ]
  
  write.csv(jobData$listings, listings.path)    
  
  jobData
}

calcListingWeighting <- function(listing, nfactor, weightings) {  
  weighting = 0
  
  for (row in seq(1, nrow(weightings))) {
    if (length(grep(weightings[[row, "keyword"]], listing[["title"]], ignore.case=TRUE))) {
      weighting = weighting + weightings[[row, "tweight"]]
    } else if (length(grep(weightings[row, "keyword"], listing[["description"]], ignore.case=TRUE))) {
      weighting = weighting + weightings[[row, "dweight"]]
    }
  }  
  
  weighting*nfactor
}

progressCalc <- function(pageCount, websitesCompleted, totalWebsites) {
  websitesLeft <- totalWebsites - websitesCompleted
  (websitesCompleted + (1 - exp(-0.005*pageCount))*websitesLeft)/totalWebsites
}

createUpdateFunc <- function(engineCount) {
  stats <- list(enginesCompleted = 0, pageCount = 0, progress = 0.0, jobCount = 0)
  updateConn <- socketConnection(port = 12345)
  listings <- NULL
  buffer <- NULL
  
  function() {
    if (!isOpen(updateConn))
      return(list(stats = stats, finished = TRUE, listings = listings, newListings = NULL))
    
    update <- readLines(updateConn)
    finished <- FALSE
    newListings <- NULL
    
    if (length(update) != 0) {      
      for (line in update) {
        if (line == paste0(rep("=", 5), collapse="")) {
          if (is.null(buffer))
            break
          obj <- eval(parse(text = paste0(buffer)))
          
          if (!is.null(obj$newListings)) {
            listings <<- rbind(listings, obj$newListings)
            newListings <- obj$newListings
          }          

          #Gather and report information on progress          
          if (obj$state$done)
            stats$enginesCompleted <<- stats$enginesCompleted + 1
          
          stats$pageCount <<-stats$pageCount + 1
          stats$progress <<- progressCalc(stats$pageCount, stats$enginesCompleted, engineCount)
          stats$jobCount <<- if (is.null(listings)) 0 else nrow(listings)
          buffer <<- c()
        } else {
          buffer <<- c(buffer, line)
        }
      }
    }
    
    if (stats$enginesCompleted == engineCount) {
      #clean up
      close(updateConn)
      finished <- TRUE
    }
    
    
    list(stats = stats, listings = listings, finished = finished, newListings = newListings)
  } 
}

createLALProcessWithUpdate <- function(fullUpdate = FALSE, commit = TRUE, parallel = TRUE) {
  p <- createLALProcess(fullUpdate, commit, parallel)
  engineCount <- 0
  
  done <- FALSE
  while(engineCount < length(allEngines)) {
    Sys.sleep(4)
    u <- p$update()
    dump("u$stats", append = TRUE)
    if (u$finished) {
      engineCount <- engineCount + 1
    }
  }
  
  mccollect(p$process)
}

createLALProcess <- function(fullUpdate = FALSE, commit = TRUE, parallel = TRUE) {
  p <- mcparallel({
    serverConn <- socketConnection(port = 12345, server = TRUE)
    on.exit(close(serverConn))
    loadAllListings(fullUpdate, commit, parallel,                
                    function(jobupdate) {
                      writeLines(deparse(jobupdate), serverConn)
                      writeLines(paste0(rep("=", 5), collapse=""), serverConn)           
                    })
  })
  Sys.sleep(0.5) # Make sure server socket is created first
  list(process = p, update = createUpdateFunc(length(allEngines)), updateCount = 0)
}

loadAllListings <- function (fullUpdate = FALSE, commit = TRUE, parallel = TRUE, 
                             progressCallback = function(jobsearch) {}) {
  jobData <- loadJobData()
  
  if (parallel) {    
    newListings <- mclapply(jobData$engines, doCompleteSearch, jobData, fullUpdate, progressCallback)
  } else {
    newListings <- lapply(jobData$engines, doCompleteSearch, jobData, fullUpdate, progressCallback)
  }
  
  newListings <- do.call(rbind, newListings)  
  if (commit)
    jobData <- commitListings(newListings, jobData)
  
  jobData$listings
}

doCompleteSearch <- function(engine, jobData, fullUpdate = FALSE, 
                             progressCallback = function(jobsearch) {}) {
  jobsearch <- initJobSearch(engine, jobData, fullUpdate)

  while(!jobsearch$state$done) {
    jobsearch <- loadListings(jobsearch)
    progressCallback(list(state = jobsearch$state, newListings = jobsearch$newListings))
  }
  
  #commitJobSearch(jobsearch, jobData)
  jobsearch$listings
}

initJobSearch <- function(engine, jobData, fullUpdate = FALSE) {
  #jobCount = if (is.null(jobData$listings)) 0 else nrow(jobData$listings)
  jobsearch <- list(data = jobData, engine = engine, fullUpdate = fullUpdate, 
                    page = engine$indexBase, weightings = jobData$weightings)  
  
  jobsearch <- within(jobsearch, {
    state <- list(page = engine$indexBase, done = FALSE)
    
    if (!is.null(jobData$listings)) {
      currentListings <- jobData$listings[jobData$listings$website ==  engine$name, ]
      
      lasturl <- 
        if (!fullUpdate && nrow(currentListings) > 0)  
          currentListings[[1, "url"]] 
        else 
          ""
    } else {
      currentListings <- NULL
      lasturl <- ""
    }
  })
  
  jobsearch
}

# Compiles list of relevent jobs based on weightings 
# filter - specifies which job sites to use. NULL means use all of them
# fullUpdate - when set to FALSE it only adds new jobs
# commit - when set to TRUE commits results to listings.csv
loadListings <- function(jobsearch) {
  if (jobsearch$state$done)
    return(jobsearch)

  engine <- jobsearch$engine  
  pageListings = data.frame()
  finished <- FALSE
  doc <- NULL
  
  repeat {
    try({
      doc <- htmlParse(sprintf(engine$url, jobsearch$state$page), error =
                         function(msg, code, domain, line, col, level, filename, class, immediate) {})
      break
    })
  }
  
  pageListings <- xpathApply(doc, engine[["xpath"]], function(entry) {
    if (!finished) {
      listing <- engine$parser(xmlDoc(entry))
      if (!jobsearch$fullUpdate & listing[["url"]] == jobsearch$lasturl) {
        print(paste0("Update truncated: ", engine$name))
        finished <<- TRUE            
      }
      
      weight <- if (!finished) 
        calcListingWeighting(listing, engine[["nfactor"]], jobsearch$weightings) 
      else 
        -100
      return(c(listing, weighting=weight, website=engine[["name"]], 
               Job = HTML(sprintf("<a href='%s'>%s</a>", listing[["url"]], trim(listing[["title"]]))),
               Date = outputDate(listing[["datePosted"]]),
               Location = trim(listing[["location"]]),    
               Favourites = createSaveLinks(listing[["url"]])
      ))            
    } else {
      return(list(weighting=-100))
    }
  })
  
  if (length(pageListings) != 0) {
    pageListings <- do.call(rbind.data.frame, 
                            pageListings[sapply(pageListings, function(listing) { listing$weighting > 0})])
    #Remove duplicate listings
    pageListings <- pageListings[!(pageListings$url %in% jobsearch$listings$url) | 
                                 !(pageListings$url %in% jobsearch$currentListings$url), ]
    pageListings <- pageListings[pageListings$weighting > 0, ]
  } else {
    finished <- TRUE
    pageListings <- NULL
  }
   
  #print(sprintf("Page results: %d, Total Results: %d", nrow(pageListings), nrow(listings)))
  
  jobsearch$state$page <- jobsearch$state$page + 1
  jobsearch$state$done <- finished | jobsearch$state$page >= 200
  jobsearch$listings <-  if (is.null(jobsearch$listings)) 
                           pageListings
                         else 
                           merge(jobsearch$listings, pageListings, all = TRUE) 
  jobsearch$newListings <- pageListings

  message <- sprintf("%d jobs found so far on %s", 
                     if (!is.null(jobsearch$listings)) nrow(jobsearch$listings) else 0,
                     engine[["name"]])      
  cat(paste0(message, "\n"))
  
  #detailMsg <- sprintf("%d results found at %s", 
  #                     if (is.null(siteListings)) 0 else nrow(siteListings), engine[["name"]])
  #cat(paste0(detailMsg, "\n"))
    
  jobsearch
}


commitReweightedListings <- function() {
  jobData <- loadJobData()
  jobData$listings <- reweightListings(jobData)
  commitJobData(jobData)
}

reweightListings <- function(jobData) {
  listings <- jobData$listings
  for (i in seq(1, nrow(jobData$listings))) {
    nfactor <- jobData$engines[sapply(jobData[['engines']], 
                                      function(engine) 
                                        engine$name == listings[[i, 'website']])][[1]]$nfactor
    listings[[i, "weighting"]] <- calcListingWeighting(listings[i, ], nfactor, jobData$weightings)
  }

  listings
}

savedJobs.path <- paste0(base.dir, "savedJobs")
changeSavedJobs <- function(change) {
  if (file.exists(savedJobs.path))
    load(savedJobs.path)
  else
    savedJobs <- NULL

  savedJobs <- change(savedJobs)
  save(savedJobs, file = savedJobs.path)
  savedJobs
}

saveJob <- function(url) {
  if (url != "") 
    changeSavedJobs(function(savedJobs) { c(savedJobs, url) })
}

deleteJob <- function(url) {
  if (url != "")
    changeSavedJobs(function(savedJobs) { savedJobs[savedJobs != url] })
}

getSavedJobs <- function() changeSavedJobs(function(savedJobs) {savedJobs})

