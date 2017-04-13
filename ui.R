library(shiny)
library(rhandsontable)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(broom)
library(forecast)
library(lubridate)
library(zoo)
library(dplyr)
library(plyr)

pdf(NULL)

ui <- shinyUI(fluidPage(
  titlePanel("easy analytics"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 downloadButton('downloadtemplate', 'Download template'),
                 # added interface for uploading / downloading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"')
               ),
               
               mainPanel(
                 textOutput(outputId = "inst", container = div),
                 tableOutput('contents')
               )
             )
    ),
    tabPanel("Time Series",
             pageWithSidebar(
               headerPanel('Time Series'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'Date Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = ""),
                 selectInput('zcol', "Color Variable", "", selected = ""),
                 numericInput(inputId = "date_breaks",
                              label = "Date breaks",
                              value = 250)
                 
               ),
               mainPanel(
                 textOutput(outputId = "instTS", container = div),
                 plotlyOutput('MyPlot')
               )
             )
    ),
    tabPanel("3d scatter plot",
             pageWithSidebar(
               headerPanel('3d scatter'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol2', 'Day or Hour Variable', ""),
                 selectInput('ycol2', 'Hour or Day Variable', "", selected = ""),
                 selectInput('zcol2', "kW Variable", "", selected = ""),
                 selectInput('qcol2', "color Variable", "", selected = "")
                 
               ),
               mainPanel(
                 textOutput(outputId = "inst3d", container = div),
                 plotlyOutput('MyPlot2')
               )
             )
    ),
    tabPanel("Heat Map plot",
             pageWithSidebar(
               headerPanel('Heat Map'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol3', 'Date Variable', ""),
                 selectInput('ycol3', 'Hour Variable', "", selected = ""),
                 selectInput('zcol3', "Color Variable", "", selected = "")
                 
               ),
               mainPanel(
                 textOutput(outputId = "heat map", container = div),
                 plotlyOutput('MyPlot3')
               )
             )
    ),
    tabPanel("Reg. analysis",
             pageWithSidebar(
               headerPanel('OAT regression'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol4', 'OAT Variable', ""),
                 selectInput('ycol4', 'kW Variable', "", selected = ""),
                 selectInput('zcol4', 'Date Variable', "", selected = ""),
                 numericInput(inputId = "n",
                              label = "number of scheduled periods",
                              value = 1),
                 numericInput(inputId = "p",
                              label = "number of segments",
                              value = 4),
                 numericInput(inputId = "m",
                              label = "interval in minutes",
                              value = 15),
                 actionButton(inputId = "clicks",
                              label = "Click to run model"),
                 tableOutput("statsable")
               ),
               mainPanel(
                 textOutput(outputId = "instoat", container = div),
                 rHandsontableOutput('hot'),
                 plotlyOutput('MyPlot4'),
                 plotlyOutput('MyPlot5')
                 )
             )
    ),
    tabPanel("Normalize-it",
             pageWithSidebar(
               headerPanel('Let the Good Times Role'),
               sidebarPanel(
                 numericInput(inputId = "station",
                              label = "Station ID",
                              value = 690150),
                 htmlOutput("mySite"),
                 numericInput(inputId = "psave",
                              label = "estimated % Savings",
                              value = .1),
                 numericInput(inputId = "dur",
                              label = "number of M&V measurements",
                              value = 8640),
                 actionButton(inputId = "clicks2",
                              label = "Click to run model"),
                 tableOutput("statsable2")
               ),
               mainPanel(
                 textOutput(outputId = "instnorm", container = div),
                 rHandsontableOutput('hot2'),
                 plotlyOutput('MyPlot6'),
                 plotlyOutput('MyPlot7')
                 
               )
             )
    )
  )
))