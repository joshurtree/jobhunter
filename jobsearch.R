library(XML)
library(parallel)

options(stringsAsFactors = FALSE)
source("engines.R")
source("defaults.R")

# remove whitespace from both ends of a string
trim <- function(value) {
  gsub("^\\s*|\\s*$", "", value)
}

# Load information on engines, weights and job listings
read.listings <- function(path = listings.path) {
  if (file.exists(listings.path)) { 
    read.csv(listings.path, row.names = 1) 
  } else 
    data.frame()  
}

write.listings <- function(listings, path = listings.path, test = FALSE) {
  # Remove old and duplicate listings
  listings <- listings[(Sys.Date() - listings$datePosted) <= 28, ]
  listings <- listings[!duplicated(listings[c("description", "location")]), ]
  
  if (!test)
    write.csv(listings, path)
  
  listings
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
    parseAllListings(fullUpdate, commit, parallel,                
                    function(jobupdate) {

                      writeLines(deparse(jobupdate), serverConn)
                      writeLines(paste0(rep("=", 5), collapse=""), serverConn)           
                    })
  })
  Sys.sleep(0.5) # Make sure server socket is created first
  list(process = p, update = createUpdateFunc(length(allEngines)), updateCount = 0)
}

load.engines <- function() {
  allEngines
}

parseAllListings <- function (fullUpdate = FALSE, commit = TRUE, parallel = TRUE, 
                             progressCallback = function(jobsearch) {}) {
  listings <- read.listings()
  
  if (parallel & .Platform$OS.type == "unix") {    
    newListings <- mclapply(load.engines(), doCompleteSearch, listings, fullUpdate, progressCallback)
  } else {
    newListings <- lapply(load.engines(), doCompleteSearch, listings, fullUpdate, progressCallback)
  }

  newListings <- do.call(rbind, newListings)
  listings <- rbind(listings, newListings)
  if (commit)
    listings <- write.listings(listings)
  
  listings
}

doCompleteSearch <- function(engine, listings, fullUpdate = FALSE, 
                             progressCallback = function(jobsearch) {}) {
  jobsearch <- initJobSearch(engine, listings, fullUpdate)

  while(!jobsearch$state$done) {
    jobsearch <- parseListings(jobsearch)
    progressCallback(list(state = jobsearch$state, newListings = jobsearch$newListings))
  }
  
  #commitJobSearch(jobsearch, jobData)
  jobsearch$listings
}

initJobSearch <- function(engine, listings, fullUpdate = FALSE) {
  #jobCount = if (is.null(jobData$listings)) 0 else nrow(jobData$listings)
  jobsearch <- list(engine = engine, fullUpdate = fullUpdate, page = engine$indexBase)  
  
  jobsearch <- within(jobsearch, {
    state <- list(page = engine$indexBase, done = FALSE)
    
    if (!is.null(listings)) {
      currentListings <- listings[listings$website ==  engine$name, ]
      fullUpdate = TRUE
      
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
parseListings <- function(jobsearch) {
  if (jobsearch$state$done) {
    #browser()
    return(jobsearch)
  }

  engine <- jobsearch$engine  
  pageListings = data.frame()
  finished <- FALSE
  doc <- NULL
  
  # Attempt to retrive page five times then abort search
  attempts <- 0
  repeat {
    try({
      doc <- htmlParse(sprintf(engine$url, jobsearch$state$page), error =
                         function(msg, code, domain, line, col, level, filename, class, immediate) {})
      break
    })
    attempts <- attempts + 1
    
    if (attempts > 5) {
      jobsearch$state$done <- TRUE
      return(jobsearch)
    }
  }
  
  pageListings <- xpathApply(doc, engine[["xpath"]], function(entry) {
    if (!finished) {
      listing <- engine$parser(xmlDoc(entry))
      if (!jobsearch$fullUpdate) {
        #browser()
        if (listing[["url"]] == jobsearch$lasturl) {
          print(paste0("Update truncated: ", engine$name))
          finished <<- TRUE   
          return(NULL)
        }
      }
      
      #weight <- calcListingWeighting(listing, engine[["nfactor"]], jobsearch$weightings) 
      listing[["website"]] <- engine[["name"]]
      return(listing)            
    } else {
      return(NULL)
    }
  })

  pageListings <- do.call(rbind.data.frame, pageListings)
  
  if (nrow(pageListings) != 0) {
    #Remove duplicate listings
    pageListings <- pageListings[!(pageListings$url %in% jobsearch$listings$url), ]
    pageListings <- pageListings[!(pageListings$url %in% jobsearch$currentListings$url), ]
  }
  
  # terminate if website is returning sponsered jobs only
  if (nrow(pageListings) == 0) 
    finished <- TRUE
   
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

# Test function for new engines to check that they are functioning to requirements 
testEngine <- function(name, pages = 1) {   
  jobsearch <- initJobSearch(allEngines[sapply(allEngines, function(engine) {engine$name == name})][[1]], 
                             read.listings())   
  for (i in seq(1:pages)) 
    jobsearch <- parseListings(jobsearch)
  listings <- write.listings(jobsearch$listings, test.listings.path)
  head(listings)
}
