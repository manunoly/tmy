shinyUI(
  div(class="header",
    fluidPage(
      title = "TMY-generator", theme = "bootstrap.css",
      div(class="container-fluid",
        fluidRow(#Encabezado
          column(2,
                 a(href="http://www.iner.gob.ec/", target="_blank",
                   img(src='logoINERhorizontal.png', align = "right", width=200))
                 ),
          column(10,
                 titlePanel(h1("Typical Meteorological Year from long-term hourly data", 
                               align="center"),"TMY Generator")
                 )
          )
        ),
      div(class="container-fluid",
        fluidRow(#configuraciones
          column(2,
                 wellPanel(#Table input
                   fluidRow(
                     h4("Table input", align="center"),
                     fileInput('datafile', 'Select file',
                               accept=c('text/csv', 
                                        'text/comma-separated-values,text/plain', 
                                        '.csv')),
                     column(6, 
                            checkboxInput('header', 'Header', TRUE),
                            radioButtons('quote', 'Quote', 
                                         c(None='', 'Double Quote'='"',
                                           'Single Quote'="'"),'"')
                            ),
                     column(6, 
                            radioButtons('sep', 'Separator',
                                         c(Comma=',', Semicolon=';',
                                           Tab='\t', space = " "), ','))
                     )
                   )
                 ),
          column(2,
                 wellPanel(#Period settings
                   fluidRow(
                     h4("Period settings", align="center"),
                     numericInput("year_from", label = h6("From (year)"), value = 2008),
                     numericInput("year_to", label = h6("To (year)"), value = 2013),
                     numericInput("hours_per_year", label = h6("Values per year"), value = 8760)
                     )
                   )
                 ),
          column(4,
                 wellPanel(#Table Settings
                   fluidRow(
                     h4("Table settings", align="center"),
                     column(6,
                            numericInput("total_variables", label = h6("Total variables"), value = 5, min=5),
                            numericInput("date_time_column_number", label = h6("Date-Time column"), value = 1),
                            numericInput("temperature_column_number", label = h6("Temperature (Ta) column"), value = 2)
                            ),
                     column(6,
                            numericInput("humidity_column_number", label = h6("Humidity (Rh) column"), value = 3),
                            numericInput("wind_speed_column_number", label = h6("Wind speed (Ws) column"), value = 4),
                            numericInput("radiation_column_number", label = h6("Glob. Radiation (Ghr) column"), value = 6)
                            )
                     )
                   )
                 ),
          column(4,
                 wellPanel(#Weights FS
                   fluidRow(
                     h4("Weighting for Filkenstein–Schaffer (FS) statistics", align="center"),
                     helpText(textOutput('weightControl'), align="center"),
                     fluidRow(
                       column(4,
                              numericInput("W_TAmean", label = h6("Ta (avg)"), value = 30),
                              numericInput("W_TAmax", label = h6("Ta (max)"), value = 5),
                              numericInput("W_TAmin", label = h6("Ta (min)"), value = 5)
                              ),
                       column(4,
                              numericInput("W_RHmean", label = h6("Rh (avg)"), value = 5),
                              numericInput("W_RHmax", label = h6("Rh (max)"), value = 2.5),
                              numericInput("W_RHmin", label = h6("Rh (min)"), value = 2.5)
                              ),
                       column(4,
                              numericInput("W_WindSpeedmean", label = h6("Ws (avg)"), value = 5),
                              numericInput("W_WindSpeedmax", label = h6("Ws (max)"), value = 5),
                              numericInput("W_Global_Rad", label = h6("Ghr (sum)"), value = 40)
                              )
                       )
                     )
                   )
                 )
          )
        ),
      div(class="container-fluid",
        fluidRow(#Resultados
          column(2, #Download
                 div(style= "background-color: #ffffcc; width: 100%; height: 100%; 
                     border: 1px solid #eeeedb; border-radius: 4px; padding: 10px;",
                 h4("Download TMY tables", align="center"), 
                 selectInput("dataset", "Choose results dataset to download:", 
                             choices = c("TMY final data", "TMY raw data", "Selected months")),
                 downloadButton('downloadData', 'Download')
                 )
                 ),
          column(10, #displays
                 mainPanel(width = 12,
                           tabsetPanel(type = "tabs", 
                                       tabPanel("Summary",
                                                fluidRow(h4("Selected months"), 
                                                         tableOutput('selecYear')), 
                                                fluidRow(h4("Summary"), 
                                                         tableOutput("summary"))
                                       ),
                                       tabPanel("Weather plots", 
                                                fluidRow(
                                                column(4,selectInput("varweatherplot", "Choose variable:", 
                                                            choices = c('Temperature', 'Humidity', 'Global solar radiation', 'Wind speed'))),
                                                column(8, helpText("- Draw a box inside the plot and double click to zoom-in. Repeat the action as needed.", br(), 
                                                       "- Double click to restore defaults"))
                                                ),
                                                fluidRow(
                                                plotOutput('weatherplot', height = "400px", 
                                                           dblclick = "weatherplot_dblclick",
                                                           brush = brushOpts(id = "weatherplot_brush", resetOnNew = TRUE))
                                                )
                                       ),
                                       tabPanel("Montly CDF Plots", 
                                                selectInput("cdfplot", "Choose variable:", 
                                                            choices = c('Temperature (avg)',
                                                                        'Temperature (max)',
                                                                        'Temperature (min)',
                                                                        'Humidity (avg)',
                                                                        'Humidity (max)',
                                                                        'Humidity (min)',
                                                                        'Wind speed (avg)',
                                                                        'Wind speed (max)',
                                                                        'Global Radiation')),
                                                plotOutput('CDFPlotoutput',height = "400px")
                                                ),
                                       tabPanel("Help", 
                                                fluidRow(
                                                  column(3, 
                                                         div(style= "background-color: #f5f5f5; 
                                                                width: 100%; height: 100%; 
                                                                border: 1px solid #eeeedb; 
                                                                border-radius: 4px; 
                                                                padding: 10px;",
                                                           img(src='tmy_gen_logo2.png', align = "center", width="100%"),
                                                           hr(),
                                                           p("Version: 0.4.0", align="center"),
                                                           includeMarkdown("help/about.md"),
                                                           hr()
                                                         )
                                                         ),
                                                column(9, tags$iframe(src="Manual1.html", width="100%", height="600px" , frameBorder=0)))
                                       )
                                       )
                           )
                 )
          )
        )
      )
    )
  )
