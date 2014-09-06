# ui.R


counties <- readRDS("data/counties.rds")
StateNames <- c("ALL",toupper(unique(unlist(
  lapply (counties$name,function (x) strsplit(x,',')[[1]][1])))))

shinyUI(fluidPage(
  titlePanel("Census Visualizer"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Explore county-level demographic info
        as maps and data, using information from the 
        2010 US Census. View data for one state or for
        the entire United States."),
      
      selectInput("var", 
                  label = "Choose a variable to display",
                  choices = c("White", "Black",
                              "Hispanic", "Asian"),
                  selected = "White"),
      
      selectInput("state", label="Choose a state",
                  choices = StateNames, selected = "ALL"),
      
      
      selectInput("pctOrTot", label="As",
                  choices = c("Percent", "Total"), selected = "Percent"),
      
      sliderInput("range", 
                  label = "Range of interest:",
                  min = 0, max = 100, value = c(0,100))
    ),

  mainPanel(
    tabPanel ("Interactive Map", plotOutput("map")),
    tabPanel("Data explorer", dataTableOutput("countyTable"))
  )
         
    
  )
))
