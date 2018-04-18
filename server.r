#source("fun/helpers.r")
source("fun/plotWindrose.r")
source("fun/rmsd.r")
source("fun/movingAverage.r")
source("fun/multiplot.r")
colnames <- c("date","TAavg","TAmax","TAmin","RHavg","RHmax","RHmin","WSavg","WSmax","GHRsum")

shinyServer(function(input, output) {
      data <- reactive({
        inFile <- input$datafile
        if (is.null(inFile))
          return(NULL)
        data <- read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
        #ajuste de formatos para fechas
        data[,input$date_time_column_number]<-as.POSIXct(data[,input$date_time_column_number])
        data$year <- format(data[,input$date_time_column_number], "%Y")
        data$month <- format(data[,input$date_time_column_number], "%Y-%m")
        data$day <- format(data[,input$date_time_column_number], "%Y-%m-%d")
        data$month_day_hour <- format(data[,input$date_time_column_number], "%m-%d %H")
        return(data)
      })

dailydata <- reactive({
        inFile <- input$datafile
        if (is.null(inFile))
          return(NULL)
        data <- as.data.frame(data())
        ##calcular minima, media y maxima de las variables
        #crear vectores para indices de columnas
        meanColNum=c(input$temperature_column_number,input$humidity_column_number,input$wind_speed_column_number)
        maxColNum=c(input$temperature_column_number,input$humidity_column_number,input$wind_speed_column_number)
        minColNum=c(input$temperature_column_number,input$humidity_column_number)
        sumColNum=c(input$radiation_column_number)
        
        #generar tablas de valores diarios por separado para medias, maximas, minimas y suma
        meandailydata<-aggregate(data[,meanColNum],by=list(data$day), mean)
        maxdailydata<-aggregate(data[,maxColNum],by=list(data$day), max)
        mindailydata<-aggregate(data[,minColNum],by=list(data$day), min)
        sumdailydata<-aggregate(data[,sumColNum],by=list(data$day), sum)
        
        #cosntruir tabla de valores diarios
        dailydata<-cbind(meandailydata[,1:2], maxdailydata[,2], mindailydata[,2], meandailydata[,3],
                         maxdailydata[,3], mindailydata[,3], meandailydata[,4],maxdailydata[,4],sumdailydata[,2])
        names(dailydata) <- colnames
        dailydata$date <- as.Date(dailydata$date)
        dailydata$month <- format(dailydata$date, "%Y-%m")
        dailydata$month_day <- format(dailydata$date, "%m-%d")
        return(dailydata)
      })

dailydatamean <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  dailydata <- as.data.frame(dailydata())
  #calcular el año promedio
  dailydatamean <- aggregate(dailydata[,2:10],by=list(paste("2000",dailydata$month_day,sep="-")), mean)
  names(dailydatamean)[1] <- "date"
  dailydatamean$date <- as.Date(dailydatamean$date)
  dailydatamean$month <- format(dailydatamean$date, "%Y-%m")
  dailydatamean$month_day <- format(dailydatamean$date, "%m-%d")
  dailydatamean$monthnum <- format(dailydatamean$date, "%m) %b")
  dailydatamean$year <- "Avg"
  return(dailydatamean)
})

dailycdf <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  dailydata <- as.data.frame(dailydata())
  #obtener las distribucion de frecuencia acumulada (CDF) de cada mes de cada año
  dailycdf<-NULL
  for (i in 2:10){
    tmpj<-NULL
    for (j in unique(dailydata$month)){
      fun <- ecdf(dailydata[dailydata$month==j,i])
      tmp <- fun(dailydata[dailydata$month==j,i])
      tmpj <- c(tmpj,tmp)
    }
    dailycdf <- cbind(dailycdf,tmpj)
  }
  dailycdf <- as.data.frame(dailycdf)
  names(dailycdf)<-colnames[-1]
  dailycdf$date <- dailydata$date
  dailycdf$month <- format(dailycdf$date, "%Y-%m")
  dailycdf$monthnum <- format(dailycdf$date, "%m) %b")
  dailycdf$year <- format(dailycdf$date, "%Y")
  return(dailycdf)
})

dailycdfmean <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  dailydatamean <- as.data.frame(dailydatamean())
  #obtener las distribucion de frecuencia acumulada (CDF) de cada mes del año medio
  dailycdfmean<-NULL
  for (i in 2:10){
    tmpj<-NULL
    for (j in unique(dailydatamean$month)){
      fun <- ecdf(dailydatamean[dailydatamean$month==j,i])
      tmp <- fun(dailydatamean[dailydatamean$month==j,i])
      tmpj <- c(tmpj,tmp)
    }
    dailycdfmean <- cbind(dailycdfmean,tmpj)
  }
  dailycdfmean <- as.data.frame(dailycdfmean)
  names(dailycdfmean)<-colnames[-1]
  return(dailycdfmean)
})

cdfdiff <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  dailycdf <- as.data.frame(dailycdf())
  dailycdfmean <- as.data.frame(dailycdfmean())
  ##Calcular estadistico FS para cada mes
  #Calcular diferencias de CDF de cada año con cdf año medio y recostruir tabla
  cdfdiff <- dailycdf[,1:9]-dailycdfmean[rep(seq_len(nrow(dailycdfmean)), input$year_to - input$year_from + 1), ]
  cdfdiff <- as.data.frame(cdfdiff)
  names(cdfdiff)<-colnames[-1]
  cdfdiff$date <- dailycdf$date
  cdfdiff$month <- format(cdfdiff$date, "%Y-%m")
  return(cdfdiff)
})

FS <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  cdfdiff <- as.data.frame(cdfdiff())
  ##Calcular estadistico FS para cada mes
  #estadistico FS
  FS <- aggregate(abs(cdfdiff[,1:9]), by=list(paste(cdfdiff$month,"01", sep="-")), mean)
  names(FS)[1] <- "date"
  FS$date <- as.Date(FS$date)
  return(FS)
})

WFS <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  FS <- as.data.frame(FS())
  #Promedio ponderado WFS
  weights <- cbind(input$W_TAmean, input$W_TAmax, input$W_TAmin, input$W_RHmean, 
                   input$W_RHmax, input$W_RHmin, input$W_WindSpeedmean, 
                   input$W_WindSpeedmax, input$W_Global_Rad)/100
  WFS <- data.frame(date=FS$date, ws=rowSums(FS[,2:10]*rep(weights,nrow(FS))))
  WFS$date <- as.Date(WFS$date)
  WFS$monthnum <- as.numeric(format(WFS$date, "%m"))
  return(WFS)
})


selecMonth <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  WFS <- as.data.frame(WFS())
  #Selección de fechas según los tres valores con WFS más bajo.
  #Para esto se comparan entre los meses equivalentes de todos los años
  selecMonth=NULL
  for (i in unique(WFS$monthnum)){
    tmp <- data.frame(ws=sort(WFS[WFS$monthnum==i,2])[1:3],order=1:3)
    tmp <- merge(tmp,WFS, by="ws",all.x=TRUE)
    selecMonth <- rbind(selecMonth,tmp)
  }
  selecMonth$month <- format(selecMonth$date, "%Y-%m")
  return(selecMonth)
})

hourlyError <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  data <- as.data.frame(data())
  #Calcular el RMSD para cada mes seleccionado
  ##Año promedio horario
  indexHourlyMean <- c(input$temperature_column_number, input$humidity_column_number, 
                       input$wind_speed_column_number, input$radiation_column_number)
  hourlydatamean <- aggregate(data[,indexHourlyMean],by=list(data$month_day_hour), mean)
  hourlyError <- data[,indexHourlyMean] - do.call("rbind", 
                                                  replicate(input$year_to - input$year_from + 1,
                                                            hourlydatamean[,-1], 
                                                            simplify = FALSE))
  hourlyError$date <- data[,input$date_time_column_number]
  hourlyError$month <- format(hourlyError$date, "%Y-%m")
  return(hourlyError)
})

hourlyError <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  data <- as.data.frame(data())
  #Calcular el RMSD para cada mes seleccionado
  ##Año promedio horario
  indexHourlyMean <- c(input$temperature_column_number, input$humidity_column_number, 
                       input$wind_speed_column_number, input$radiation_column_number)
  hourlydatamean <- aggregate(data[,indexHourlyMean],by=list(data$month_day_hour), mean)
  hourlyError <- data[,indexHourlyMean] - do.call("rbind", 
                                                  replicate(input$year_to - input$year_from + 1,
                                                            hourlydatamean[,-1], 
                                                            simplify = FALSE))
  hourlyError$date <- data[,input$date_time_column_number]
  hourlyError$month <- format(hourlyError$date, "%Y-%m")
  return(hourlyError)
})

RMSDmonth <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  hourlyError <- as.data.frame(hourlyError())
  #Calcular RMSD mensual para todos los años
  RMSDmonth=NULL
  for (i in unique(hourlyError$month)){
    tmp <- sapply(hourlyError[hourlyError$month==i,1:4],rmsd)
    RMSDmonth <- rbind(RMSDmonth,tmp)
  }
  rownames(RMSDmonth)<-NULL
  RMSDmonth<-as.data.frame(RMSDmonth)
  RMSDmonth$date<-as.Date(paste(unique(hourlyError$month),1,sep="-"))
  RMSDmonth$month<-format(RMSDmonth$date, "%Y-%m")
  RMSDmonth$monthnum<-format(RMSDmonth$date, "%m")
  return(RMSDmonth)
})

SxTotal <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  RMSDmonth <- as.data.frame(RMSDmonth())
  #Calculo de Sx para todos los meses
  RMSDmonthMin <- aggregate(RMSDmonth[,1:4], by=list(RMSDmonth$monthnum), min)
  SxTotal <- do.call("rbind", replicate(input$year_to - input$year_from + 1,
                                        RMSDmonthMin[,-1], 
                                        simplify = FALSE))/RMSDmonth[,1:4]
  SxTotal$date<-as.Date(RMSDmonth$date)
  SxTotal$month<-format(SxTotal$date, "%Y-%m")
  return(SxTotal)
})

WSx <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  SxTotal <- as.data.frame(SxTotal())
  #Promedio ponderado WSx
  weightsWSx <- cbind(TAw=sum(input$W_TAmean, input$W_TAmax, input$W_TAmin), 
                      RHw=sum(input$W_RHmean, input$W_RHmax, input$W_RHmin), 
                      WSw=sum(input$W_WindSpeedmean, input$W_WindSpeedmax), 
                      GHRw=input$W_Global_Rad)/100
  WSx <- data.frame(month=SxTotal$month, 
                    wsx=rowSums(SxTotal[,1:4]*rep(weightsWSx,nrow(SxTotal))))
  return(WSx)
})

selecMonth2 <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  selecMonth <- as.data.frame(selecMonth())
  WSx <- as.data.frame(WSx())
  #Subseting de los valores WSx para los meses preseleccionados
  selecMonth <- merge(selecMonth, WSx, by="month", all.x=TRUE)
  selecMonth <- selecMonth[,c("date","month","monthnum","order","ws","wsx")] #Re-ordenar columnas
  selecMonth <- selecMonth[order(selecMonth$monthnum),]
  return(selecMonth)
})

selecYear <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  selecMonth <- as.data.frame(selecMonth2())
  wsxselect <- data.frame(monthnum=aggregate(selecMonth[,6], by=list(selecMonth$monthnum), max)[,1],
                          wsx=aggregate(selecMonth[,6], by=list(selecMonth$monthnum), max)[,2])
  selecYear <- merge(wsxselect, selecMonth[,c(2,6)], by="wsx", all.x=TRUE)
  selecYear <-selecYear[order(selecYear$monthnum),]
  rownames(selecYear) <- 1:12
  return(selecYear)
})

TMYrawdata <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  data <- as.data.frame(data())
  selecYear <- as.data.frame(selecYear())
  #Consulta de datos originales por meses tipicos
  TMYrawdata <- data[data$month %in% selecYear$month, 1:(input$total_variables+1)]
  TMYrawdata<-TMYrawdata[order(format(TMYrawdata[,input$date_time_column_number], "%m")),]
  rownames(TMYrawdata) <- 1:nrow(TMYrawdata)
  return(TMYrawdata)
})

TMYfinaldata <- reactive({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  TMYfinaldata <- as.data.frame(TMYrawdata())
  #Ajuste de los 5 valores antes y despues del cambio de mes
  monthChanges <- c(744,1416,2160,2880, 3624, 4344, 5088, 5832, 6552, 7296, 8016)
  TMYdataavg <- sapply(TMYfinaldata[,2:3], movingAverage)
  for (i in 1:length(monthChanges)){
    TMYfinaldata[(monthChanges[i]-5):(monthChanges[i]+5),2:3]<-TMYdataavg[(monthChanges[i]-5):(monthChanges[i]+5),]
  }
  TMYfinaldata$year <- as.numeric(format(TMYfinaldata[,input$date_time_column_number], "%Y"))
  TMYfinaldata$month <- as.numeric(format(TMYfinaldata[,input$date_time_column_number], "%m"))
  TMYfinaldata$day <- as.numeric(format(TMYfinaldata[,input$date_time_column_number], "%d"))
  TMYfinaldata$minute <- 60
  names(TMYfinaldata[,input$date_time_column_number])<-"date"
  TMYfinaldata <- TMYfinaldata[,c(1,input$total_variables+2 , input$total_variables+3 ,
                                  input$total_variables+4 , input$total_variables+5,
                                  1:input$total_variables+1)]
  return(TMYfinaldata)
})

###############################################
###############################################
###############################################

#Salidas SHINY

output$weightControl <- renderText({
  inFile <- input$datafile
  if (is.null(inFile) || sum(input$W_TAmean, input$W_TAmax, input$W_TAmin, input$W_RHmean, 
                             input$W_RHmax, input$W_RHmin,input$W_WindSpeedmean, 
                             input$W_WindSpeedmax, input$W_Global_Rad)==100)
    return(NULL)
  paste("ERROR: The sum of the weights in FS-Statistic must be equal 100" )
  })

output$selecYear<-renderTable({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  selecYear <- as.data.frame(selecYear())
  selecYear <-selecYear[,c("monthnum","month","wsx")]
  colnames(selecYear) <- c("Month num","Month","WSx")
  selecYear$WSx <- round(selecYear$WSx,4)
  rownames(selecYear)<-c("Jan","Feb","Mar","Apr","May","Jun",
                         "Jul","Aug","Sep","Oct","Nov","Dec")
  return(t(selecYear))
  })

output$summary<-renderTable({
  inFile <- input$datafile
  if (is.null(inFile))
    return(NULL)
  TMYfinaldata <- as.data.frame(TMYfinaldata())
  summary <- cbind(Mean=colMeans(TMYfinaldata[,-1:-5]), 
                   Min=sapply(TMYfinaldata[,-1:-5],min),
                   Max=sapply(TMYfinaldata[,-1:-5],max))
  return(t(summary))
  })

#Download Data
datasetInput <- reactive({
  switch(input$dataset,
         'TMY final data' = TMYfinaldata(),
         'TMY raw data' = TMYrawdata(),
         'Selected months' = selecYear())
  })

output$downloadData <- downloadHandler(
  filename = function() {paste(input$dataset, '.csv', sep='')},
  content = function(file) {write.table(datasetInput(), file, sep=",",row.names=FALSE)})

#Graficos de CDF por variable
CDFplotinput <- reactive({
  switch(input$cdfplot,
         'Temperature (avg)' = aes(TAavg, TAavg.1, colour=year),
         'Temperature (max)' = aes(TAmax, TAmax.1, colour=year),
         'Temperature (min)' = aes(TAmin, TAmin.1, colour=year),
         'Humidity (avg)' = aes(RHavg, RHavg.1, colour=year),
         'Humidity (max)' = aes(RHmax, RHmax.1, colour=year),
         'Humidity (min)' = aes(RHmin, RHmin.1, colour=year),
         'Wind speed (avg)' = aes(WSavg, WSavg.1, colour=year),
         'Wind speed (max)' = aes(WSmax, WSmax.1, colour=year),
         'Global Radiation' = aes(GHRsum, GHRsum.1, colour=year)
         )
  })

  output$CDFPlotoutput <- renderPlot({
    inFile <- input$datafile
    if (is.null(inFile))
      return(NULL)
    colyears <- colorRampPalette(brewer.pal(9,"Set1"))(input$year_to - input$year_from + 2)
    colyears <- colyears[-length(colyears)]
    cdfplot <- ggplot(data.frame(as.data.frame(dailydata()),as.data.frame(dailycdf())),
           CDFplotinput())+
      geom_point()+facet_wrap(~monthnum, ncol=6)+
      geom_point(data=data.frame(as.data.frame(dailydatamean()),as.data.frame(dailycdfmean())),
                 CDFplotinput())+
      facet_wrap(~monthnum, ncol=6)+ylab("CDF")+ guides(colour = guide_legend("Year"))+
      scale_colour_manual(values=c(colyears,"black"))+
      theme(panel.background =  element_rect(fill = "#f9f9f9"))
    return(cdfplot)
  })
# Grafico de series meteorológicas
  varweatherplot <- reactive({
    switch(input$varweatherplot,
           'Temperature' = aes(x= Date, y=Temperature),
           'Humidity' = aes(x= Date, y=Humidity), 
           'Global solar radiation' = aes(x= Date, y=Radiation), 
           'Wind speed' = aes(x= Date, y=Windsp)
    )
  })
  
  colourweatherplot <- reactive({
    switch(input$varweatherplot,
           'Temperature' = "#ff6501",
           'Humidity' = "#019bff", 
           'Global solar radiation' = "#ffe401", 
           'Wind speed' = "#8c8c8c"
    )
  })
  
  ylabweatherplot <- reactive({
    switch(input$varweatherplot,
           'Temperature' = "[ºC]",
           'Humidity' = "[%]", 
           'Global solar radiation' = expression(paste("[Wh/m"^2,"]",sep="")), 
           'Wind speed' = "[m/s]"
    )
  })
  
  ranges <- reactiveValues(x = NULL, y = NULL)
  
  output$weatherplot <- renderPlot({
    inFile <- input$datafile
    if (is.null(inFile))
      return(NULL)
    TMYfinaldata <- as.data.frame(TMYfinaldata())
    names(TMYfinaldata)[input$date_time_column_number]  <- "Date"
    names(TMYfinaldata)[input$temperature_column_number+4]  <- "Temperature"
    names(TMYfinaldata)[input$humidity_column_number+4]  <- "Humidity"
    names(TMYfinaldata)[input$radiation_column_number+4]  <- "Radiation"
    names(TMYfinaldata)[input$wind_speed_column_number+4]  <- "Windsp"
    TMYfinaldata$Date <- as.POSIXct(paste(input$year_to,format(TMYfinaldata$Date, "%m-%d %H:%M"),sep="-"))
    plot <- ggplot(TMYfinaldata,varweatherplot())+
      geom_line(colour=colourweatherplot())+
      geom_smooth()+
      theme(panel.background =  element_rect(fill = "#f9f9f9"))+
      ylab(ylabweatherplot())+ggtitle(input$varweatherplot)+coord_cartesian(xlim = ranges$x)
    return(plot)
  })
  
  observeEvent(input$weatherplot_dblclick, {
    brush <- input$weatherplot_brush
    if (!is.null(brush)) {
      ranges$x <- as.POSIXct(c(brush$xmin, brush$xmax),origin = "1970-01-01")
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
})