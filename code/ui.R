#
# This is the user-interface definition of the Shiny web application to analyse REMI-seq data stemming from 
# a grid layout.
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(grid)


shinyUI(navbarPage("Grid analyser",
                    tabPanel("Create layout",
                             fluidRow(
                               h4('1. Choose number of layers for each grid:'),
                               column(6,
                                      numericInput('lgrid1', 'Layers in Grid 1', 1,
                                                   min = 1, max = 5)),
                               column(6,
                                      numericInput('lgrid2', 'Layers in Grid 2', 1,
                                                   min = 1, max = 5))
                             ),
                             br(),
                             br(),
                             fluidRow(
                               h4('2. Populate plate and enzyme infos'),
                               column(6,
                                      fileInput('file1', 'Choose CSV File',accept=c('text/csv','text/comma-separated-values,text/plain'))
                               )
                             ),
                             br(),
                             br(),
                             fluidRow(
                               h4('3. Check if everything is correct'),
                               column(6,
                               headerPanel('Grid1'),
                               plotOutput('plot1')),
                               column(6, 
                               headerPanel('Grid2'),
                               plotOutput('plot2'))
                             )
                    ),
                    tabPanel("Analyse fastq or position files",
                             mainPanel(tabsetPanel(type="tabs",
                                tabPanel("Table",
                                  fluidRow(
                                    column(12,
                                      br(),
                                      br(),
                                      textInput("directory", "Choose directory that contains files:",value = "")
                                    )
                                  ),
                                  br(),
                                  hr(),
                                  br(),
                                  fluidRow(
                                  	column(5,align="center",
                                  				 actionButton("read_list", label = "Read all fastq.gz files")),
                                  	column(2,align="center",
                                  				 h3('or')),
                                  	column(5,align="center",
                                  				 actionButton("read_list1", label = "Read all positions files"))
                                  ),
                                  br(),
                                  br(),
                                  fluidRow(
                                    column(12,
                                      downloadButton('downloadData', 'Download'),
                                      verbatimTextOutput("DExpTable_data"),
                                      DT::dataTableOutput("DExp", width = "100%", height = "100%")      
                                  )
                                )),
                                tabPanel("Plots",
                                         sidebarPanel(
                                           selectInput("my_choices", "Which plot do you want to see?",choices = c("perc", "numb_mut","numb_mut_index"), selected = 1,width=200)
                                         ),
                                         mainPanel(
                                         fluidRow(
                                           conditionalPanel(
                                             condition = "input.my_choices == 'perc'",
                                             plotOutput('usable',width = "100%")
                                           ),
                                           conditionalPanel(
                                             condition = "input.my_choices == 'numb_mut'",
                                             plotOutput('numb_mut',width = "100%")
                                           ),
                                           conditionalPanel(
                                             condition = "input.my_choices == 'numb_mut_index'",
                                             plotOutput('numb_mut_index',width = "100%")
                                           )
                                         )
                                ))
                    ))),
                    tabPanel("Analyse positions",
                      
                        fluidRow(
                          DT::dataTableOutput("pos", width = "100%", height = "100%")
                        ),
                        br(),
                        hr(),
                        br(),
                        fluidRow(
                          DT::dataTableOutput("grid", width = "100%", height = "100%"),
                          downloadButton('download_overlapData', 'Download')
                        )
                      
                    ),
                    tabPanel("add plate infos",
                             DT::dataTableOutput("plate", width = "100%", height = "100%"),
                    				 downloadButton('download_plateData', 'Download')
                    ),
									  tabPanel("statistics",
									 				 fluidRow(
									 				 	DT::dataTableOutput("stats", width = "100%", height = "100%"),
									 				 	downloadButton('download_stats', 'Download')
									 				 )
									 )
                    
))
