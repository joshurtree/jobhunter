library(shiny)

searchLink <- if (.Platform$OS.type == "windows") { 
    tags$a(onClick = "alert('Function not available in Windows')", "Find new jobs")
  } else {
    actionLink("search", "Find new jobs")
  }

shinyUI(fluidPage(titlePanel("Job Hunter"),
    tags$script(src="script.js"),
    tags$script(sprintf("savedUrls = '%s'", 
                        paste(persistentVector(savedJobs.path)$get.values(), collapse=", "))),
    sidebarLayout(sidebarPanel(fluidPage(
       fluidRow(searchLink),
       tags$br(),
       conditionalPanel(condition = "input.main == 'recent'", 
                        fluidRow(selectInput("recentDays", 
                                "Show jobs posted in the last (days):", 
                                c(1, 2, 3, 4, 5, 7, 10, 14), 2))),
       conditionalPanel(condition = "input.main == 'weights'",
                        fluidRow(actionLink("addWeight", "Add keyword")), 
                        fluidRow(actionLink("saveWeights", "Apply new weights"))),
       conditionalPanel(condition = "input.main == 'random'",
                        fluidRow(actionLink("newrandom", "Shuffle")))
    ), width = 3),
    mainPanel(navbarPage("",
      tabPanel("All jobs", dataTableOutput("listings"),  value = "all"),
      tabPanel("Recent Jobs", dataTableOutput("recent"), value = "recent"),
      tabPanel("Saved Jobs", dataTableOutput("savedJobs"), value = "saved"), 
      tabPanel("Random Jobs", dataTableOutput("random"), value = "random"),  
      tabPanel("Weights", dataTableOutput("weights"), value = "weights"),
      id = "main"))
    ))
)