library(shiny)

# Define UI for random distribution application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Distribution of Test Statistics"),
  
  # Sidebar with controls to select the random distribution type
  # and number of observations to generate. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      radioButtons("dist", "Population distribution type:",
                   c("Normal" = "norm",
                     "Uniform" = "unif",
                     "Log-normal" = "lnorm",
                     "Exponential" = "exp",
                     "heavy tails" = "heavy",
                     "HEAVY tails" = "heavyyy")),
#      checkboxInput("checkboxnorm", label = "Show normal density", value = FALSE)
      radioButtons("tstat","Test statistic:",
                   c("z" = "z",
                     "t" = "t",
                     "chi-square" = "chisq")),    
      sliderInput("n", 
                  "Sample size:", 
                  value = 30,
                  min = 5, 
                  max = 400),
      sliderInput("reps", 
                  "Number of repetitions:", 
                  value = 200,
                  min = 1, 
                  max = 5000),
      checkboxInput("checkbox", label = "Add samples one at a time", value = FALSE),
      actionButton("resample", label = "Draw New Sample"),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),br()
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
))