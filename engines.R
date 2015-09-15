
processDate <- function(value) {
  if (is.null(value) | value == "" | is.na(value) | 
      length(grep("(?i)today|minutes ago|hours ago|just posted", value) != 0))
    date <- Sys.Date()
  else if (length(grep("[yY]esterday", value)) != 0)
    date <- as.Date(-1, Sys.Date()) 
  else if (length(grep("hours? ago", value, ignore.case=TRUE)) != 0) 
    date <- as.Date(Sys.time() - as.integer(strsplit(value, " ")[[1]][[1]]))
  else if (length(grep("days? ago", value, ignore.case=TRUE)) != 0) 
    date <- as.Date(as.integer(strsplit(paste0("-", value), " ")[[1]][[1]]), Sys.Date())
  else {
    cat(sprintf("Unknown date '%s' encountered", value))
    date < Sys.Date()
  }

  date
}

processField <- function(entry, xpath, index = 1, default = "") {
  values <- xpathSApply(entry, xpath)

  if (length(values) != 0)
    trim(xmlValue(values[[index]]))
  else
    default
}

processAttribute <- function(entry, xpath, index = 1, default = "") {
  values <- xpathSApply(entry, xpath)
  
  if (length(values) >= index)
    trim(values[[index]])
  else
    default
}

createEngine <- function(n, u, xp, p, ib = 1, f = 1) {
  list (name = n, url = u, xpath = xp, parser = p, indexBase = ib, nfactor = f)  
}

Fish4JobsEngine <- createEngine("Fish4Jobs",
                        "http://www.fish4.co.uk/searchjobs/?countrycode=GB&Page=%d", 
                        "//li[@class='lister__item cf']",
                        function(entryDoc) {
  details <- xpathSApply(entryDoc, "//span/text()")
  
  list(
    url = sprintf("http://fish4jobs.co.uk/job/%s", xpathSApply(entryDoc, "substring(/li/@id, 6)")[[1]][[1]]),
    title = processField(entryDoc, "//a[@itemprop='title']/text()"),
    location = xmlValue(details[[1]]),
    salary = xmlValue(details[[2]]),
    employer = xmlValue(details[[3]]),
    description = processField(entryDoc, "//p[@itemscope='description']/text()"),
    datePosted = processDate(processField(entryDoc, "/li/ul/li[class='job-actions__action pipe']/text()"))
  )
})

MonsterEngine <- createEngine("Monster",
                      "http://jobsearch.monster.co.uk/search/?pg=%d",
                      "//tr[@class='odd' or @class='even']",
                      function (entry) {
  date <- processDate(processField(entry, "/tr/td/div/div/text()", 7))
  list( 
    url = xpathSApply(entry, "substring-before(/tr/td/div/div/a/@href, '?')")[[1]],
    title = processAttribute(entry, "/tr/td/div/div/a/@title"),
    location = processAttribute(entry, "/tr/td/div/div/a/@title", 2),
    salary = "Unknown",
    employer = processAttribute(entry, "/tr/td/div/div/div/a/@title"),
    description = "",
    datePosted = if (is.null(date)) Sys.Date() else as.Date(date)
  )
})

ReedEngine <- createEngine("Reed",
                   "http://www.reed.co.uk/jobs?sortby=DisplayDate&pageno=%d",
                   "//article[starts-with(@id, 'jobSection')]",
                   function(entry) {
  details <- xpathSApply(entry, "/article/div/div/ul/li/text()")
  list(
    url = paste0("http://www.reed.co.uk", 
                 xpathSApply(entry, "substring-before(/article/div/header/div/h3/a/@href, '#')")[[1]]),
    title = processField(entry, "/article/div/header/div/h3/a/text()"),
    location = xmlValue(details[[2]]),
    salary = xmlValue(details[[1]]),
    employer = processField(entry, "/article/div/div/div/span/span/a/text()"),
    description = processField(entry, "/article/p/text()"),
    datePosted = processDate(xmlValue(details[[3]]))
  )
})

GRBEngine <- createEngine("GRB",
                  "http://www.grb.uk.com/graduate-jobs/%d",
                  "//tr[position() > 1]",
                  function(entry) {
  details <- xpathSApply(entry, "/tr/td")
  list(
    url = paste0("http://www.grb.uk.com", xpathSApply(entry, "/tr/td/a/@href")[[1]]),
    title = processField(entry, "/tr/td/a/text()"),
    location = xmlValue(details[[3]]),
    salary = xmlValue(details[[4]]),
    employer = "Unknown",
    description = "",
    datePosted = as.Date(xmlValue(details[[2]]), "%d/%m/%Y")
  )
})

IndeedEngine <- createEngine("Indeed",
                     "http://www.indeed.co.uk/jobs?q=''&sort=date&start=%d0",
                     "//div[@itemtype='http://schema.org/JobPosting']",
                     function(entry) {
  list(
    url = paste0("http://www.indeed.co.uk", xpathSApply(entry, "/div/h2/a/@href")[[1]]),
    title = processField(entry, "/div/h2/a/text()"),
    location = processField(entry, "/div/span/span/span/text()"),
    salary = "",
    employer = processField(entry, "//span[@itemprop='hiringOrganization']"),
    description = processField(entry, "/div/table/tr/td/div/span/text()"),
    datePosted = processDate(processField(entry, "/div/table/tr/td/div/div/span[@class='date']/text()"))
    )
}, 0)

TotalEngine <- createEngine("TotalJobs",
                    "http://www.totaljobs.com/JobSearch/Results.aspx?PageNum=%d",
                    "//div[@typeof='JobPosting']",
                    function(entry) {
  url <- paste0("http://www.totaljobs.co.uk", xpathSApply(entry, "/div/div/div/div/h2/a/@href")[[1]])
  url <- sub("&.*", "", url)
  list(
    url = url,
    title = processField(entry, "/div/div/div/div/h2/a/text()"),
    location = processField(entry, "/div/div/div/ul/li[1]/span/span/text()"),
    salary = processField(entry, "/div/div/div/ul/li[2]/span/text()"),
    employer = processField(entry, "/div/div/div/ul/li[4]/h3/a/text()"),
    description = processField(entry, "/div/div/p/text()"),
    datePosted = processDate(processField(entry, "/div/div/div/ul/li[5]/span/text()"))
  )
})

allEngines <- list(Fish4JobsEngine, MonsterEngine, GRBEngine, IndeedEngine, ReedEngine, TotalEngine)
