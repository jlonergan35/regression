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

source('util.R')

template <- data.frame(Date=NA, OAT=NA, kW=NA)[numeric(0), ]

server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  output$mySite <- renderUI({
    tags$a(href = "http://www.neecons.com/services/ddstudies/map-of-tmy3-weather-station","Link to Station Map")
  })
  
  output$downloadtemplate <- downloadHandler(
    filename = function() { 
      paste('template', '.csv', sep='') 
    },
    
    content = function(file) {
      write.csv(template, file)
    }
  )
  output$inst <- renderText("Welcome to the quick and dirty energy dashboard, generating analytics since
                            right now.  To use, simply download the template, paste in your data, keeping 
                            the date time stamp in the following format: '11/29/2017 00:00'. save
                            and upload.  The dashboard will take care of the rest.  The dashboard has sample 
                            Data for user experimentation...") 
  
  output$instTS <- renderText("This figure is set up to render a time series plot, however will also work 
                              for an X vs Y type scatter plot.  Use the Date Breaks input to vary the interval
                              on the x axis tick marks and the Color Variable to select which variable scales by
                              color")
  
  output$inst3d <- renderText("This plot is one way to show the daily load profiles, alongside the daily average 
                              demand curves, and the heat map.  It has a nice zoom feature. The intended use is 
                              That the x and y axes be Day and Hour, while the Z axis be kW.  the color can be temp 
                              or kW.")
  
  output$heatmap <- renderText("This plot is a heat map, another way to render time series data")
  
  output$instoat <- renderText("this is an regression tool using the time and temperature model created
                               by LBNL, and a user input schedule.  It includes  fit statistics, and two plots
                               an OAT plot with occupied and un-occupied bins, and a time series model showing
                               actual and predicted demand. To use for M&V, select a TMY 3 data set on the adjacent
                               page, input a TMY schedule, (should be the same as the regression schedule, over the TMY 
                               year of 2000.) The model will be applied to the TMY data, and total error calculated
                               based on estimated percent savings, M&V duration, and the model fit statistics, per ASHRAE
                               Guideline 14")
  output$instnorm <- renderText("this tab will normalize the model from the regression analysis")
  
  data <- reactive({ 
    #req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    if (is.null(inFile))
      df <- read.csv("sample_data.csv", stringsAsFactors = FALSE)
    else
    
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep,
                   quote = input$quote, stringsAsFactors = FALSE)
    
    df$Date <- strftime(as.POSIXct(df$Date, format = "%m/%d/%Y %H:%M"))
    
    df$Day <- strftime(df$Date, format = "%m/%d/%Y")
    
    df$Hour <- as.numeric(strftime(df$Date, format = "%H"))
    
    updateSelectInput(session, inputId = 'xcol', label = 'Date Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                      choices = names(df), selected = names(df)[2])
    updateSelectInput(session, inputId = 'zcol', label = 'Color Variable',
                      choices = names(df), selected = names(df)[3])
    
    updateSelectInput(session, inputId = 'xcol2', label = 'Day or Hour Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol2', label = 'Hour or Day Variable',
                      choices = names(df), selected = names(df)[2])
    updateSelectInput(session, inputId = 'zcol2', label = 'kW Variable',
                      choices = names(df), selected = names(df)[3])
    updateSelectInput(session, inputId = 'qcol2', label = 'color Variable',
                      choices = names(df), selected = names(df)[3])
    
    updateSelectInput(session, inputId = 'xcol3', label = 'Day Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol3', label = 'Hour Variable',
                      choices = names(df), selected = names(df)[2])
    updateSelectInput(session, inputId = 'zcol3', label = 'Color Variable',
                      choices = names(df), selected = names(df)[3])
    
    updateSelectInput(session, inputId = 'xcol4', label = 'OAT Variable',
                      choices = names(df), selected = names(df))
    updateSelectInput(session, inputId = 'ycol4', label = 'kW Variable',
                      choices = names(df), selected = names(df)[2])
    updateSelectInput(session, inputId = 'zcol4', label = 'Date Variable',
                      choices = names(df), selected = names(df)[3])
    
    return(df)
  })
  
  output$contents <- renderTable({
    head(data(),50)
  })
  
  
  
  output$MyPlot <- renderPlotly({
   
    x <- data()[,input$xcol]
    y <- data()[, input$ycol]
    z <- data()[,input$zcol]
    
    fmt <- list(
      family = "Arial, sans-serif",
      size = 12
    )
    fmt2 <- list(
      family = "Arial, sans-serif",
      size = 9
    )
    
    xtit <- list(
      title = input$xcol,
      titlefont = fmt,
      tickangle = 45,
      tickfont = fmt2,
      dtick = input$date_breaks,
      showticklabels = TRUE
    )
    
    ytit <- list(
      title = input$ycol,
      titlefont = fmt,
      tickangle = 45,
      tickfont = fmt2
    )
    m <- list(
      b = 100,
      pad = 4
    )
    
    leg <- list(colorbar = list(title = input$zcol,
                                titlefont = fmt))
    
    
    plot_ly(x = x, y = y,type = "scatter", mode ="markers", marker = leg, color = ~z, size = ~y) %>%
      layout(xaxis = xtit, yaxis = ytit, margin = m) 
    
  })
  output$MyPlot2 <- renderPlotly({
    
    fmt <- list(
      family = "Arial, sans-serif",
      size = 12
    )
    x <- data()[,input$xcol2]
    y <- data()[,input$ycol2]
    z <- data()[,input$zcol2]
    q <- data()[,input$qcol2]
    
    leg <- list(colorbar = list(title = input$qcol2,
                                titlefont = fmt))
    
    plot_ly(x = x, y = y, z = z, type = "scatter3d", mode = "markers",marker = leg, color = ~q) %>%
      layout(scene = list(xaxis = list(title = input$xcol2),
                          yaxis = list(title = input$ycol2),
                          zaxis = list(title = input$zcol2)))
    
  })
  output$MyPlot3 <- renderPlotly({
    
    fmt <- list(
      family = "Arial, sans-serif",
      size = 12
    )
    x <- data()[,input$xcol3]
    y <- data()[,input$ycol3]
    z <- data()[,input$zcol3]
    
    leg <- list(colorbar = list(title = input$zcol3,
                                titlefont = fmt))
    xtit <- list(
      title = input$xcol3,
      titlefont = fmt,
      tickangle = 45,
      tickfont = fmt,
      showticklabels = TRUE
    )
    
    ytit <- list(
      title = input$ycol3,
      titlefont = fmt,
      tickfont = fmt,
      showticklabels = TRUE
    )
    
    m <- list(
      b = 100,
      l = 100,
      pad = 4
    )
    
    col <- c("blue3","green3","yellow3","orange3","red3","purple3")
    
    plot_ly(x = x, y = y, z = z, type = "heatmap", colorbar = list(title = input$zcol3, titlefont = fmt), color = ~-z, colors = colorRamp(col)) %>%
      layout(margin = m, xaxis = xtit, yaxis = ytit)
    
    
  })
  values = reactiveValues()
  setHot = function(x) values[["hot"]] <- x
  DF <- reactive(data.frame(Schedule.Name = 1:input$n, Start.Date = "1/1/2000",
                            End.Date = "12/31/2025",
                            Monday.Start = 6, Monday.End = 18, Monday.Window = as.integer(1),
                            Tuesday.Start = 6, Tuesday.End = 18, Tuesday.Window = as.integer(1),
                            Wednesday.Start = 6, Wednesday.End = 18, Wednesday.Window = as.integer(1),
                            Thursday.Start = 6, Thursday.End = 18, Thursday.Window = as.integer(1),
                            Friday.Start = 6, Friday.End = 18, Friday.Window = as.integer(1),
                            Saturday.Start = 0, Saturday.End = 0, Saturday.Window = as.integer(1),
                            Sunday.Start =0, Sunday.End = 0, Sunday.Window = as.integer(1),
                            stringsAsFactors = F))
  
  output$hot <- renderRHandsontable({
    
    rhandsontable(DF(), selectCallback = TRUE) %>%
      hot_validate_numeric(col= list(4:24), min = 0, max = 24) 
  })
  observe({
    if(is.null(input$hot)) {
      hotdf <-DF()
      setHot(hotdf)
    } else if(!is.null(input$hot)) { 
      hotdf = hot_to_r(input$hot)
      setHot(hotdf)
    }
  })
  
  setHot2 = function(x) values[["hot2"]] <- x
  DF2 <- reactive(data.frame(Schedule.Name = 1:input$n, Start.Date = "1/1/2000",
                            End.Date = "12/31/2000",
                            Monday.Start = 6, Monday.End = 18, Monday.Window = as.integer(1),
                            Tuesday.Start = 6, Tuesday.End = 18, Tuesday.Window = as.integer(1),
                            Wednesday.Start = 6, Wednesday.End = 18, Wednesday.Window = as.integer(1),
                            Thursday.Start = 6, Thursday.End = 18, Thursday.Window = as.integer(1),
                            Friday.Start = 6, Friday.End = 18, Friday.Window = as.integer(1),
                            Saturday.Start = 0, Saturday.End = 0, Saturday.Window = as.integer(1),
                            Sunday.Start =0, Sunday.End = 0, Sunday.Window = as.integer(1),
                            stringsAsFactors = F))
  
  output$hot2 <- renderRHandsontable({
    
    rhandsontable(DF2(), selectCallback = TRUE) %>%
      hot_validate_numeric(col= list(4:24), min = 0, max = 24) 
  })
  observe({
    if(is.null(input$hot2)) {
      hotdf <-DF2()
      setHot2(hotdf)
    } else if(!is.null(input$hot2)) { 
      hotdf = hot_to_r(input$hot2)
      setHot2(hotdf)
    }
  })
  
  observeEvent(
    input$clicks,
   {
    
    sch <- values[["hot"]]
    
    dd <- data()[,c(input$xcol4,input$ycol4,input$zcol4)]
    names(dd) <- c("OAT", "kW", "Date")
    # Encode with hour
    dd$hour <- as.numeric(strftime(dd$Date,'%H'))
    
    # Encode with day of week
    dd$Day <- as.factor(strftime(dd$Date,'%a'))
    
    # Encode with weekhour
    dd$weekhour <- as.integer((strftime(dd$Date,'%w')))*24.0 + (dd$hour+1)
    
    # Add an occupancy factor placeholder to base data
    dd <- cbind(dd,list(Occ=NA))#,Schedule=NA))
    
    bs <- sch
    bs$Start.Date <- as.POSIXct(bs$Start.Date,format="%m/%d/%Y",tz='America/Los_Angeles')
    bs$End.Date <- as.POSIXct(bs$End.Date,format="%m/%d/%Y",tz='America/Los_Angeles')
    
    dd <- apply.schedule(dd,bs)
    dd$Occ[is.na(dd$Occ)] <- 0
    
    # Separate into occupied and unoccupied
    dd.unocc <- filter(dd, Occ == 0)
    dd.occ <- filter(dd, Occ == 1)
    
    segs <- input$p
    #segs <- as.numeric(unlist(strsplit(segs,",")))
    if(length(index(dd.occ)) > 0){
    max_oat <- max(dd.occ$OAT,na.rm=T) 
    min_oat <- min(dd.occ$OAT,na.rm=T)
    
    dd.occ <- dd.occ %>% 
      rowwise() %>% 
      do(data.frame(.,Tc=component_temperatures(.$OAT,min_oat,max_oat,segs))) 
    dd.occ$Occ[is.na(dd.occ$Occ)] <- 1
    dd.occ[is.na(dd.occ)] <- 0
    
    cc.occt <- dd.occ %>%
      group_by(Day,hour)
    
    occpredt <- lm(as.formula(paste('kW ~',paste('I(Tc.',1:segs,')',sep='',collapse='+'))),data= cc.occt)
    }
 
        # UNOCCUPIED: Do regression for each Day/Hour combination
    if(length(index(dd.unocc)) > 0){
    cc.unocct <- dd.unocc %>%
      group_by(Day,hour)
    
    unoccpredt <- lm(kW ~ OAT,data=cc.unocct)
    }
    
    dd <- cbind(dd,list(Predicted = NA))
    if(length(index(dd.occ)) > 0){
    dd$Predicted[with(dd, Occ == 1)] <- predict(occpredt,cc.occt)
    }
    if(length(index(dd.unocc)) > 0){
    dd$Predicted[with(dd, Occ ==0)] <- predict(unoccpredt,cc.unocct)
    dd[is.na(dd)] <- 0
      }
    levels(dd$Occ) <- c(levels(dd$Occ), "Occupied","Unoccupied")
    dd$Occ[dd$Occ == 1] <- 'Occupied'
    dd$Occ[dd$Occ == 0] <- 'Unoccupied'
  
    # calculate standard error
    ste <- sqrt(sum((dd$Predicted-dd$kW)^2,na.rm = TRUE)/(nrow(dd)-1))
    pste <- (2*ste)/mean(dd$kW)*100
    
    # calculate the NDB
    meane <- (dd$Predicted-dd$kW)
    mmeane <- mean(meane, na.rm=TRUE)
    meane <- sum((dd$Predicted-dd$kW)/(nrow(dd)-1)*mmeane, na.rm=TRUE)
   
    pmtab <- data.frame(matrix(nrow = 7, ncol = 2))
    names(pmtab) <- c("metric", "value")
    
    pmtab[1,] <- c("CV(RMSE)",ste)
    pmtab[2,] <- c("CV(NRMSE)",pste)
    pmtab[3,] <- c("NMBE",meane)
    pmtab[4,] <- c("peak kW",max(dd$kW))
    pmtab[5,] <- c("kWh",sum(dd$kW/4))
    pmtab[6,] <- c("predicted peak kW",max(dd$Predicted))
    pmtab[7,] <- c("predicted kWh",sum(dd$Predicted/(60/input$m)))
    
    output$statsable <- renderTable(pmtab)
    
    output$MyPlot4 <- renderPlotly({
      
      plot_ly(data = dd, x = ~OAT, y= ~ kW, color = ~ Occ, type = "scatter", mode = 'markers') %>%
                layout(xaxis = list(title = input$xcol4),
               yaxis = list(title = input$ycol4),
               legend = list(x = 100, y = 0.90))
      
    })
 
  output$MyPlot5 <- renderPlotly({
    fmt <- list(
      family = "Arial, sans-serif",
      size = 12
    )
    fmt2 <- list(
      family = "Arial, sans-serif",
      size = 9
    )
    xtit <- list(
      title = input$zcol4,
      titlefont = fmt,
      tickangle = 45,
      tickfont = fmt2,
      dtick = input$date_breaks,
      showticklabels = TRUE
    )
    
    ytit <- list(
      title = input$ycol4,
      titlefont = fmt,
      tickangle = 45,
      tickfont = fmt2
    )
    m <- list(
      b = 100,
      pad = 4
    )
    leg <- list(colorbar = list(title = input$xcol4,
                                titlefont = fmt))
    plot_ly(data = dd, x= ~dd$Date) %>%
      add_lines(y = ~dd$kW,  name = "Actual", mode = 'lines', line = list(color = rgb(0,.2,.5), width = 4)) %>%
      add_lines(y = ~dd$Predicted,  name = "Predicted", mode ='lines', line = list(color = rgb(.2,.75,0), width = 2)) %>%
      layout(margin = m, xaxis = xtit, yaxis = ytit)
  })
  
  observeEvent(
    input$clicks2, {
      
    sch2 <- values[["hot2"]]
      
  file <- paste("http://rredc.nrel.gov/solar/old_data/nsrdb/1991-2005/data/tmy3/",input$station,"TYA.CSV",sep="")                      
  wtable <- read.csv(file, header = TRUE, skip = 1)
  cols <- c("Date..MM.DD.YYYY.","Time..HH.MM.","Dry.bulb..C.","Dew.point..C.","RHum....")
  T.dat <- wtable[cols]
  T.dat$Date <- as.POSIXct(T.dat$Date..MM.DD.YYYY., format = "%m/%d/%Y")
  T.dat$Time <- as.POSIXct(T.dat$Time..HH.MM., format = "%H:%M")
  
         #Celcius to Fahrenheit temps function
  ctof <- function(var){9/5*var+32}
  
         #make Fahrenheit temps columns, re-build date time stamp to be 1 contiguous year (2000)
  T.dat$OAT <- sapply(T.dat$Dry.bulb..C.,ctof)
  T.dat$Dew.point_Deg.F <- sapply(T.dat$Dew.point..C.,ctof)
  T.dat$month <- as.numeric(strftime(T.dat$Date,"%m"))
  T.dat$day <- as.numeric(strftime(T.dat$Date, "%d"))
  T.dat$hour <- as.numeric(strftime(T.dat$Time, "%H"))
  T.dat$Date <- ISOdatetime(2000,T.dat$month,T.dat$day,T.dat$hour,0,0, tz = "")
  T.dat$Date[2186] <- as.POSIXct("4/2/2000", format = "%m/%d/%Y")
  
         #wetbulb temp calc function
         #source = http://journals.ametsoc.org/doi/pdf/10.1175/JAMC-D-11-0143.1 
        #mean abs. error = 0.28 deg C
  empiricalWB <- function(DB,Rh){DB*atan(0.151977*(Rh+8.313659)^.5)+atan(DB+Rh)-atan(Rh-1.676331)+0.00391838*(Rh)^(3/2)*atan(0.023101*Rh)-4.686035}
  
        # make wet bulb columns C and F
  T.dat$WB_Deg.C <- mapply(empiricalWB,T.dat$Dry.bulb..C.,T.dat$RHum....)
  T.dat$WB_Deg.F <- sapply(T.dat$WB_Deg.C,ctof)
  
  bs2 <- sch2
  
  bs2$Start.Date <- as.POSIXct(bs2$Start.Date,format="%m/%d/%Y")
  bs2$End.Date <- as.POSIXct(bs2$End.Date,format="%m/%d/%Y")
  T.dat <- cbind(T.dat,list(Occ=NA))
  T.dat <- cbind(T.dat,list(Predicted = NA))
  
  T.dat <- apply.schedule(T.dat,bs2)
  
  max_oat2 <- max(T.dat$OAT,na.rm=T) 
  min_oat2 <- min(T.dat$OAT,na.rm=T)
  
  T.dat <- T.dat %>% 
    rowwise() %>% 
    do(data.frame(.,Tc=component_temperatures(.$OAT,min_oat2,max_oat2,segs))) 
  
  T.datocc <- filter(T.dat, Occ == 1)
  T.datunocc <- filter(T.dat, Occ == 0)
  
  if(length(index(T.datocc)) > 0){
  T.dat$Predicted[with(T.dat, Occ == 1)] <- predict(occpredt,T.datocc)
  }
  if(length(index(T.datunocc)) > 0){
  T.dat$Predicted[with(T.dat, Occ ==0)] <- predict(unoccpredt,T.datunocc)
  }
  T.dat[is.na(T.dat)] <- 0
  
  levels(T.dat$Occ) <- c(levels(T.dat$Occ), "Occupied","Unoccupied")
  
  T.dat$Occ[T.dat$Occ == 1] <- 'Occupied'
  T.dat$Occ[T.dat$Occ == 0] <- 'Unoccupied'
  
  pmtab2 <- data.frame(matrix(nrow = 3, ncol = 2))
  names(pmtab2) <- c("metric", "value")
  
  pmtab2[1,] <- c("Standard Error @ 68% CI",1*(1.26*ste/input$psave*sqrt((nrow(T.dat)+2)/(nrow(T.dat)*input$dur))))
  pmtab2[2,] <- c("Normalized peak kW",max(T.dat$Predicted))
  pmtab2[3,] <- c("Normalized kWh",sum(T.dat$Predicted/(60/input$m)))
  
  output$statsable2 <- renderTable(pmtab2)
  
  output$MyPlot6 <- renderPlotly ({
    fmt <- list(
      family = "Arial, sans-serif",
      size = 12
    )
    fmt2 <- list(
      family = "Arial, sans-serif",
      size = 9
    )
    
    xtit <- list(
      title = "Date",
      titlefont = fmt,
      tickfont = fmt2,
      showticklabels = TRUE
    )
    
    ytit <- list(
      title = "OAT",
      titlefont = fmt,
      tickfont = fmt2
    )
     
      
    plot_ly(x = ~T.dat$Date, y = ~T.dat$OAT ,type = "scatter", mode ="markers", color = ~T.dat$Occ) %>% 
          layout(xaxis = xtit, yaxis = ytit) 
  })
  output$MyPlot7 <- renderPlotly({
    fmt <- list(
      family = "Arial, sans-serif",
      size = 12
    )
    fmt2 <- list(
      family = "Arial, sans-serif",
      size = 9
    )
    xtit <- list(
      title = input$zcol4,
      titlefont = fmt,
      tickangle = 45,
      tickfont = fmt2,
      showticklabels = TRUE
    )
    
    ytit <- list(
      title = input$ycol4,
      titlefont = fmt,
      tickangle = 45,
      tickfont = fmt2
    )
    m <- list(
      b = 100,
      pad = 4
    )
    
    
    plot_ly(x = T.dat$Date, y = T.dat$Predicted,type = "scatter", mode ="markers", color = ~T.dat$Occ) %>%
      layout(xaxis = xtit, yaxis = ytit, margin = m) 
  })
  })
    })
})