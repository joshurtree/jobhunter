library(shiny)

shinyUI(fluidPage(titlePanel("Job Hunter"),
  tags$script(paste0("function saveJob(url) { Shiny.onInputChange('savejob', url) };",
                     "function deleteJob(url) { Shiny.onInputChange('deletejob', url) }")),
    sidebarLayout(sidebarPanel(fluidPage(
       fluidRow(actionLink("search", "Find new jobs")),
       tags$br(),
       conditionalPanel(condition = "input.main == 'recent'", 
                        fluidRow(selectInput("recentDays", 
                                "Show jobs posted in the last (days):", 
                                c(1, 2, 3, 4, 5, 7, 10, 14), 2))),
       conditionalPanel(condition = "input.main == 'weights'",
                        fluidRow(actionLink("addWeight", "Add Keyword")), 
                        fluidRow(actionLink("saveWeights", "Save"))),
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