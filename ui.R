#Test with means
library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

#This was for trying to get a vetical slider
# js<-"$(function() {
#       var $elie = $(document.getElementsByClassName('form-group shiny-input-container'));
# rotate(270);
# function rotate(degree) {
# $elie.css({ WebkitTransform: 'rotate(' + degree + 'deg)'});
# $elie.css({ '-moz-transform': 'rotate(' + degree + 'deg)'});
# }
# });"

dashboardPage(skin="blue",
              
              #Title
              dashboardHeader(title="Significance Testing with Population means",titleWidth=450),
              
              #Sidebar
              dashboardSidebar(
                width = 260,
                sidebarMenu(
                  
                  menuItem("Overview", tabName = "over", icon = icon("university")),
                  menuItem("Choose a DataSet", tabName = "second", icon = icon("table")),
                  menuItem("Part 2", tabName = "third", icon = icon("pencil-square"))
                )),
              
              #Content within the tabs
              dashboardBody(
                tabItems(
                  
                  tabItem(tabName = "over",
                          
                          fluidRow(
                            #column of length 12 which is the whole width
                            #I include everthing in a column though because this way there are margins and it looks better
                            column(12,
                                   "About"
                                   
                            )
                          )
                  ),
                  tabItem(tabName = "second",
                          fluidPage(
                            titlePanel("Uploading Files"),
                            sidebarLayout(
                              sidebarPanel(
                                #Let them choose a preloaded dataset or input their own
                                #If it is preloaded output some information about the dataset
                                selectInput(inputId = "choose", "Select which data set you would like to use", choices = c(CEOSalaries = "ceosal",NHLData = "NHL",InputYourOwn = "input")),
                                conditionalPanel("input.choose == 'NHL'",
                                                 "This data set was collected by statisticians working for the NHL over the course of the 2016-2017 NHL season. ",br(),br(),
                                                 "It is filtered to only include players who played at least half of the season (41 games). ",br(),br(),
                                                 "There are 526 players in this set", br(),br(),
                                                 "Player = Player's Name", br(),
                                                 "GP = Games Played",br(),
                                                 "G = Goals", br(),
                                                 "A = Assists",br(),
                                                 "PTS = Points",br(),
                                                 "PIM = Penalties in Minutes", br(),
                                                 "S = Shots on Goal", br(),
                                                 "TOI = Time on Ice", br(),
                                                 "BLK = Blocks at Even Strength",br(),
                                                 "HIT = Hits at Even Strength", br(),
                                                 "FOW = Faceoff Wins at Even Strength", br(),
                                                 "FOL = Faceoff Losses at Even Strenth"
                                ),
                                conditionalPanel("input.choose == 'ceosal'",
                                                 "In 1994 Forbes collected CEO data on 'America's Best Small Companies'.",br(),br(),
                                                 "Small companies were defined as those with annual sales greater than five and less than $350 million. Companies were ranked according to 5-year average return on investment. This data covers the first 60 ranked firms."
                                                 ),
                                conditionalPanel("input.choose == 'input'",
                                                 uiOutput("fileInclude"),
                                                 fileInput('file1', 'Choose Data File:',
                                                           accept=c('.csv', '.txt', '.xls',
                                                                    '.xlsx', '.sas7bdat')),
                                                 checkboxInput('header', 'Header', TRUE)
                                                 ) 
                                # Only show this panel if the plot type is a histogram
                                #uiOutput("varSelect")
                                
                              ),
                              mainPanel(
                                tabsetPanel(type = "tabs",
                                            tabPanel("Data Display",
                                                     dataTableOutput('display')
                                            ),
                                            
                                            tabPanel("Data Summary",
                                                     verbatimTextOutput('summary'),
                                                     uiOutput("MissingNotice")
                                            )
                                )
                              )
                            )
                          )
                          
                          
                          
                          ),
                  tabItem(tabName = "third",
                          fluidRow(
                            withMathJax(),
                            column(4,
                                   h4("Conduct a test about the population mean"),
                                   uiOutput("var.sel"),
                                   uiOutput("size.sel"),
                                   numericInput(inputId = "null", "Set the null hypothesis (The population mean is equal to ___)", value = 0, min = 0, max = 10000),
                                   #use mu not equal to the null mean
                                   radioButtons(inputId= "alt","Set the alternative hypothesis", choices = c("$$\\mu\\neq null mean$$ " = "choice1", "$$\\mu < null mean$$" = "choice2","$$\\mu > null mean$$" = "choice3")),
                                   #selectInput(inputId = "alt", "Set the alternative hypothesis",choices = c("$$\\mu\\neq null mean$$ ", "$$\\mu < H_0$$",NotEqualH0 = "notEqual")),
                                   tags$style(type = "text/css",
                                              ".irs-bar {width: 100%; height: 25px; background: black; border-top: 1px solid black; border-bottom: 1px solid black;}
                                              .irs-bar-edge {background: black; border: 1px solid black; height: 25px; border-radius: 0px; width: 20px;}
                                              .irs-line {border: 1px solid black; height: 25px; border-radius: 0px;}
                                              .irs-grid-text {font-family: 'arial'; color: white; bottom: 17px; z-index: 1;}
                                              .irs-grid-pol {display: none;}
                                              .irs-max {font-family: 'arial'; color: black;}
                                              .irs-min {font-family: 'arial'; color: black;}
                                              .irs-single {color:black; background:#6666ff;}
                                              .irs-slider {width: 8px; height: 30px; top: 22px;}
                                              "),
                                   textOutput("pvalue"),
                                   tags$head(tags$style("#pvalue{color: blue;font-size: 30px;font-style: bold;}")),
                                   sliderInput(inputId = "conflev1","Select the Confidence level:",min = 0,max = 99,value = 95)
                            ),
                            column(8,
                                   plotOutput("plot.hist"),
                                   plotOutput("plot.CI")
                            )
                          )
                  )
                )
              )
)



