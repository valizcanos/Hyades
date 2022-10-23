####################################0.0. PACKAGES #######################################################

if(!require(shiny)) { install.packages("shiny"); library(shiny) }# To make web app with R
if(!require(shinyFiles)) { install.packages("shinyFiles"); library(shinyFiles) }# To make web app with R
if(!require(shinyWidgets)) { install.packages("shinyWidgets"); library(shinyWidgets) }#Widgets to use in Shiny
if(!require(shinythemes)) { install.packages("shinythemes"); library(shinythemes) }# To show any style theme
if(!require(shinyjs)) { install.packages("shinyjs"); library(shinyjs) }# To read Javascript
if(!require(raster)) { install.packages("raster"); library(raster) }# Geographic Data Analysis and Modeling
if(!require(maptools)) { install.packages("maptools"); library(maptools) }# Tools for Handling Spatial Objects 
if(!require(rgdal)) { install.packages("rgdal"); library(rgdal) }# For 'Geospatial' Data 
if(!require(shinyhttr)) { install.packages("shinyhttr"); library(shinyhttr) }#to map closely to the underlying http protocol
if(!require(sp)) { install.packages("sp"); library(sp) }# Classes and Methods for Spatial Data
if(!require(maps)) { install.packages("maps"); library(maps) }# Draw Geographical Maps
if(!require(leaflet)) { install.packages("leaflet"); library(leaflet) }# Create Interactive Web Maps with the JavaScript 'Leaflet'
if(!require(ggplot2)) { install.packages("ggplot2"); library(ggplot2) }# To make plots
if(!require(ggpubr)) { install.packages("ggpubr"); library(ggpubr) }# To filter data
if(!require(ggspatial)) { install.packages("ggspatial"); library(ggspatial) }# Spatial Data Framework for ggplot2
if(!require(remotes)) { install.packages("remotes"); library(remotes) }# Remote Repositories, Including Github
if(!require(ncdf4)) { install.packages("ncdf4"); library(ncdf4) }# Interface to Unidata netCDF (Version 4 or Earlier) Format Data Files
if(!require(RNetCDF)) { install.packages("RNetCDF"); library(RNetCDF) }# Interface to Unidata netCDF (Version 4 or Earlier) Format Data Files
if(!require(sf)) { install.packages("sf"); library(sf) }# Simple Features for R
if(!require(R.utils)) { install.packages("R.utils"); library(R.utils) }# Various Programming Utilities
if(!require(GADMTools)) { install.packages("GADMTools"); library(GADMTools) }#To download shapefiles
if(!require(elevatr)) { install.packages("elevatr"); library(elevatr) }#To download DEM
if(!require(leaflet)) { install.packages("leaflet"); library(leaflet) }#To make interactive maps
if(!require(leaflet.extras)) { install.packages("leaflet.extras"); library(leaflet.extras) }#To make interactive maps
if(!require(leafem)) { install.packages("leafem"); library(leafem) }# To see maps
if(!require(mapview)) { install.packages("mapview"); library(mapview) }# To see maps
if(!require(DT)) { install.packages("DT"); library(DT) }# To make data tables
if(!require(shapefiles)) { install.packages("shapefiles"); library(shapefiles) }#To import shapefiles
if(!require(reshape)) { install.packages("reshape"); library(reshape) } # To change the shape of my dataframe
if(!require(plotly)) { install.packages("plotly"); library(plotly) }# To make plots
if(!require(factoextra)) { install.packages("factoextra"); library(factoextra) } #Plot my cluster
if(!require(dplyr)) { install.packages("dplyr"); library(dplyr) } # To manage my data
if(!require(lubridate)) { install.packages("lubridate"); library(lubridate) }
if(!require(tidyverse)) { install.packages("tidyverse"); library(tidyverse) }
if(!require(rChoiceDialogs)) { install.packages("rChoiceDialogs"); library(rChoiceDialogs) }
if(!require(rJava)) { install.packages("rJava"); library(rJava) }
if(!require(BiocManager)) { install.packages("BiocManager"); library(BiocManager) }

options(repos = BiocManager::repositories())
#suppressPackageStartupMessages(library(h2o))
#h2o.init(nthreads = -1)


#######################################################################################################################
options(shiny.maxRequestSize=3000*1024^2) #Increase max upload file size in Shiny

############################################## GRAPHICAL UNIT INTERFACE ###############################################
ui = navbarPage(tags$img(src="icono6.png", height='65', align='center', style="display: block; margin-lef:20px; margin-top: -20px;"),
                theme = shinytheme("paper"), 
                ##### 1.0.A. DOWMNLOAD DATA FROM CHIRPS 0.05 degree (5x5km) #####
                tabPanel("GetData", 
                         sidebarPanel(width=4, #Width of sidebar, Control Panel
                           tags$h4("Download CHIRPS"),
                           selectInput("Regions", "Select region:", choices= c("World"="global", "Central America and Caribean"="camer-carib", 
                                                                        "Africa"="africa")), #Geographical area to download
                           selectInput("TResolution", "Select temporal resolution:",choices = c("Monthly"="monthly", "Daily"="daily")), #Temporal resolution of CHIRPS raster
                           tags$hr(),
                           tags$strong(tags$p("Select date range:")),
                           tags$hr(),
                           dateInput("Date1", label = 'Start Date: yyyy-mm-dd',value = Sys.Date()-180), #Start date
                           dateInput("Date2", label = 'End Date: yyyy-mm-dd',value = Sys.Date()-60), #End date
                           actionButton("init", "Download"),
                           tags$p("Go to the CHIRPS dataset: ",tags$strong(tags$a(href="https://www.chc.ucsb.edu/data", "Here!"))),
                           tags$br(),
                           tags$p("To extract .gz Files from CHIRPS click on the below button"), 
                           actionButton("Extract","Extract Files"), #Uncompress rasters
                           tags$br(),
                           tags$hr(),
                           tags$h4("Crop Image Collection"),
                           textInput("Country", "Please enter the ISO 3166 country code to download the file :","COL"), #Countrie codes
                           selectInput("Level", "Select Level", choices = c(0,1,2), selected = 2), #Level of administrative division
                           tags$p("The level refers to administrative divisions. Level 2 is the most specific country subdivision"),
                           actionButton("adquireSHP", "Plot the first Shp"), #Plot the first shape
                           actionButton("adquireDataSHP", "Data on Shape"), #Show the data of the first shape
                           tags$hr(),
                           tags$p("Do you want to select an administrative subdivision? Only applies to levels 1 and 2."),
                           textInput("Region", "Type the territorial entity","Santiago de Cali"), #Input territorial entity
                           selectInput("subLevel", "Select the level that corresponds to a territorial entity", choices = c(1,2), selected = 2), #Select sublevel of administrative division
                           actionButton("adquireSHP2", "Plot the second Shp"), #Plot sublevel of administrative division
                           tags$hr(),
                           selectInput("SelectShape", "Do you want to use as Mask the:", choices = c("First Shape", "Second Shape", "My own mask"), selected = "Second Shape"), #Choose the level of mask
                           tags$hr(),
                           #fileInput("Mk", label = "Choose a file as Mask",  multiple = TRUE, accept = c(".rds",".shp")), #Select Mask
                           actionButton("Mk","Choose a file as a Mask"),
                           actionButton("cutRaster", "Apply Crop"), #Apply cut of raster
                           tags$p("Get user guide", tags$strong(tags$a(href="https://www.dropbox.com/sh/5acafv762v6u1l3/AAB_iMrqt7Yh5Y0K1TrTILyya?dl=0", "Here!")))
                         ), #End of SideBarPanel 1.0.A
                         mainPanel(#Outputs of the first tab (data from CHIRPS)
                           tags$h3("ISO 3166 COUNTRY CODES"),
                           DT::dataTableOutput("MyTableCountrySHP"), #Table contries code
                           plotOutput("PlotSHP", width = "100%"), #Plot first shape
                           DT::dataTableOutput("MyTableSHP"), #Table of data shape
                           plotOutput("PlotSHP2", width = "100%"), #PLot the second shape
                           verbatimTextOutput("MaskLoadedInf") #Information of mask
                         ) #End of MainPanel 1.0.A
                         ),#End of TabPanel 1.0.A

                ##### 2.0. GET HEIGHTS AND DISPLAY AREA TO WORK #####
                navbarMenu("Terrain", #Begin TabPanel 2.0
                           ##### 2.1. GET ELEVATIONS #####
                           tabPanel("Get elevations",sidebarPanel( #Control Panel
                             tags$p("If you do not know the extent of the elevation map, select a raster as a reference."),
                             fileInput("SelectRaster", "Select Raster to an extent:", accept = ".tif"),
                             tags$hr(),
                             tags$p("Enter the extends:"),
                             textInput("XMin", "x Min.", value = "-81.6"), #Min. longitude
                             textInput("XMax", "x Max.", value = "-75.7"), #Max. longitude
                             textInput("YMin", "y Min.", value = "2.949999"),#Min. latitude
                             textInput("YMax", "y Max.", value= "4.999999"),#Max. latitude
                             textInput("ZoomLevel", "Zoom level", value = "5"),#Zoom level
                             textInput("NGrids", "Enter the number of grids", value="100"),#Number of grids
                             textInput("NLevels", "Enter the number of contour levels", value = "10"),#Number of contours
                             actionButton("GetElev", "Get elevation"),#To get elevation model
                             tags$hr(),
                             actionButton("DownloadHeights", "Download Heights")#To download table of heights
                           ), # End of SideBarPanel 2.1
                           mainPanel( #Outputs of elevations
                             dataTableOutput("Extention"), #To show the extentions of raster mask
                             plotOutput("ElevationPlot") #To plot DEM and contours   
                           ) # End of MainPanel 2.1
                           ), # End of TabPanel 2.1
                           ##### 2.2. SELECT AREA #####
                           tabPanel("Show area", 
                                    sidebarPanel( #Control Panel 2.2
                                      tags$p("Select XYZ points"),
                                      fileInput("MyDataPoints", "Upload XYZ File created on the 'Get Elevation' tab", accept = ".csv"),
                                      tags$hr(),
                                      tags$p("Select one of the rasters cropped"),
                                      fileInput("RasterCutted", "Add a Cropped Raster File", accept = ".tif"),
                                      tags$hr(),
                                      tags$p("If you agree with the data fields shown in the right table, you can apply them to the other cropped rasters by clicking the 'download data' button"),
                                      actionButton("DownloadData", "Download data"),
                                      progressBar(id="pb",value=0,title="", display_pct = TRUE)
                                    ),#End of SideBarPanel 2.2
                                    mainPanel( #OutPuts of Areas
                                      leafletOutput("InteractiveMap"),
                                      DT::dataTableOutput("mytableValues")
                                    ) #End of MainPanel 2.2
                                    )# End of TabPanel 2.2
                           ), #end of NavBarMenu Terrain or TabPanel 2.0
                ##### 3.0. DATA ANALYSIS ######
                navbarMenu("EAD", #Begin TabPanel 3.0
                           ##### 3.1. CLUSTER ANALYSIS ######
                           tabPanel("Clustering", #TabPanel 3.1
                                    sidebarPanel( #Control Panel 3.1
                                      tags$h4("Make cluster analysis"),
                                      fileInput("DataCluster", "Upload data", accept = ".csv"),
                                      textInput("Variables", "Type the variables to analyze. Please separate the values with commas and without blank spaces", value = "Var1,Var2,...,Varn"),
                                      actionButton("BestKM", "Show K-Means"),
                                      sliderInput("SelectCenters", "Number of centers", min = 1, max=10, value = 2),
                                      sliderInput("SelectIterations", "Number of max. iterations", min = 1, max=100, value = 10),
                                      actionButton("MakeMyCluster", "Make cluster"),
                                      actionButton("ShowMyCluster", "Show the cluster groups")
                                    ), #end of SideBarPanel
                                    mainPanel( #Outputs of cluster analysis
                                      DT::dataTableOutput("DataTableForCluster"),
                                      plotOutput("BestKMeanPlot"),
                                      plotOutput("MyPlotClusterKN"),
                                      DT::dataTableOutput("ShowMyDataCluster")
                                    ) #end of MainPanel
                                    ), #end of TabPanel 3.1.
                           ##### 3.2. SUMMARY OF DATA AND FILL MISSING VALUES ######
                           tabPanel("Analysis",
                                    sidebarPanel( #Control Panel 3.2
                                      fileInput("UploadDataSet", "Upload Dataset", accept = ".csv"),
                                      tags$br(),
                                      tags$p("Please, separate dates (yyyy-mm-dd) in years, months and days"),
                                      textInput("NameOfDates", "Enter the name of a variable that contains the dates in your dataset", "Column Name"),
                                      actionButton("FilterByDate", "Filter by date"),
                                      tags$hr(),
                                      tags$br(),
                                      #Plot editor
                                      tags$p("Plot editor"),
                                      sliderInput("AlphaPlot", "Alpha plot", min=0.0, max=1.0, value = 0.5),
                                      selectInput("ColorPlot", "Color Plot", choices = c("black","blue","brown","cyan","gray","green","magenta","maroon","orange", "pink",
                                                                                         "purple","red","steelblue","violet","white","yellow"), selected = "steelblue"),
                                      tags$hr(),
                                      tags$br(),
                                      #clusters
                                      tags$p("Please select the cluster gruop that you want to analyze. Select zero for all datasets"),
                                      sliderInput("FilterByCluster","Filter by cluster", min = 0, max = 10, value = 0),
                                      textInput("NameOfYValue", "Type the variable's name to access its data on rainfall", "Rainfall"),
                                      textInput("XLabel", "Enter the X Label", "X Label"),
                                      textInput("YLabel", "Enter the Y Label", "Y Label"),
                                      actionButton("PLotByClusterGroup", "Plot by cluster"),
                                      radioButtons("ext", "Extension", choices = c(".png",".svg")),
                                      actionButton("DownloadPlotByCluster", "Download"),
                                      actionButton("ShowSummaryC", "Show summary"),
                                      tags$hr(),
                                      tags$br(),
                                      #Seasons
                                      tags$p("Analyze your data by quarter. Please enter the months that correspond to each quarter"),
                                      checkboxGroupInput("Season1", label = h6("Season 1"), 
                                                         choices = list("January" = "January", "February" = "February", "March" = "March", "April" = "April",
                                                                        "May"="May", "June"="June", "July"="July", "August"="August", 
                                                                        "September"="September", "Octuber"="Octuber", "November"="November", "December"="December"),
                                                         selected = c("December","January","February")),
                                      checkboxGroupInput("Season2", label = h6("Season 2"), 
                                                         choices = list("January" = "January", "February" = "February", "March" = "March", "April" = "April",
                                                                        "May"="May", "June"="June", "July"="July", "August"="August", 
                                                                        "September"="September", "Octuber"="Octuber", "November"="November", "December"="December"),
                                                         selected = c("March","April","May")),
                                      checkboxGroupInput("Season3", label = h6("Season 3"), 
                                                         choices = list("January" = "January", "February" = "February", "March" = "March", "April" = "April",
                                                                        "May"="May", "June"="June", "July"="July", "August"="August", 
                                                                        "September"="September", "Octuber"="Octuber", "November"="November", "December"="December"),
                                                         selected = c("June","July","August")),
                                      checkboxGroupInput("Season4", label = h6("Season 4"), 
                                                         choices = list("January" = "January", "February" = "February", "March" = "March", "April" = "April",
                                                                        "May"="May", "June"="June", "July"="July", "August"="August", 
                                                                        "September"="September", "Octuber"="Octuber", "November"="November", "December"="December"),
                                                         selected = c("September","Octuber","November")),
                                      actionButton("BoxplotByQuarter", "Boxplot by quarter"),
                                      actionButton("DownloadBoxplotByQuarter", "Download Boxplot by quarter"),
                                      tags$hr(),
                                      actionButton("BarplotByQuarter", "Barplot by quarter"),
                                      actionButton("DownloadBarplotByQuarter", "Download Barplot by quarter"),
                                      tags$hr(),
                                      actionButton("LineplotByQuarter", "Lineplot by quarter"),
                                      actionButton("DownloadlineplotByQuarter", "Download Lineplot by quarter"),
                                      tags$hr(),
                                      actionButton("HistplotByQuarter", "Hist. by quarter"),
                                      actionButton("DownloadHistplotByQuarter", "Download Hist. by quarter"),
                                      tags$hr(),
                                      actionButton("ShowSummaryS","Show summary by Quarter"),
                                      tags$hr(),
                                      actionButton("BoxplotByMonths", "Boxplot by months"),
                                      actionButton("DownloadBoxplotByMonths", "Download Boxplot by months"),
                                      tags$hr(),
                                      actionButton("BarplotByMonths", "Barplot by months"),
                                      actionButton("DownloadBarplotByMonths", "Download Barplot by months"),
                                      tags$hr(),
                                      actionButton("LineplotByMonths", "Lineplot by months"),
                                      actionButton("DownloadLineplotByMonths", "Download Lineplot by months"),
                                      tags$hr(),
                                      actionButton("HistplotByMonths", "Hist. by months"),
                                      actionButton("DownloadHistplotByMonths", "Download Hist. by months"),
                                      tags$hr(),
                                      actionButton("ShowSummaryM", "Show summary by Months"),
                                      tags$hr(),
                                      tags$p("Missing values are rare in the CHIRPS data. The functions listed below can be used to fill in any CHIRPS pixels that are missing a value"),
                                      tags$p("Fill Missing Values with multi-layer feedforward artificial neural network:"),
                                      selectInput("ActFunc", "Select activation function:",choices= c("Tanh","Rectifier","Maxout"), selected = "Rectifier"),
                                      numericInput("Epochs", "Specify the number of times to iterate the dataset (Epochs):", value = 100),
                                      sliderInput("HiddenLayers", "Specify the hidden layer sizes", min=1, max=1000, value=c(10,20)),
                                      sliderInput("FracTraining", "Specify the fraction of data to train", min=0.50, max=0.95, value=c(0.70)),
                                      textInput("PredictorsV", "Specify the possible set of predictors (each one separated by comma and without spaces after the comma):", value = "x,y,Heights,cluster,Months,Years"),
                                      tags$hr(),
                                      actionButton("FillMV", "Fill missing values")
                                    ), #End of SideBarPanel 3.2
                                    mainPanel( #Outputs of analysis of data
                                      DT::dataTableOutput("MyDataSetT"),
                                      DT::dataTableOutput("missingValuesByDatesT"),
                                      plotOutput("ShowPlotsByCluster"),
                                      textOutput("PrintShowSummaryC"),
                                      plotOutput("BoxplotByQuarterP"),
                                      plotOutput("BarplotByQuarterP"),
                                      plotOutput("LineplotByQuarterP"),
                                      plotOutput("HistplotByQuarterP"),
                                      textOutput("PrintShowSummaryS"),
                                      plotOutput("BoxplotByMonthsP"),
                                      plotOutput("BarplotByMonthsP"),
                                      plotOutput("LineplotByMonthsP"),
                                      plotOutput("HistplotByMonthsP"),
                                      plotOutput("PlotDataFilled"),
                                      textOutput("PrintShowSummaryM")
                                    ) #End of MainPanel 3.2
                                    ) #end of TabPanel 3.2
                           ), #End of NavBarMenu3.0
                ##### 4.0 EXTREME VALUES ANALYSIS #####
                navbarMenu("EVT",
                           ##### 4.1. EXTREME VALUES ######
                         tabPanel("Extreme value analysis",
                                  sidebarPanel(
                                    tags$h3("Extreme value analysis"),
                                    fileInput("LoadData", "Upload data",accept = ".csv"),
                                    tags$hr(),
                                    tags$br(),
                                    textInput("ColDates", "Enter the name of the column that contains the dates data", value="MyDatesName"),
                                    tags$br(),
                                    textInput("ColVar", "Enter the column name that contains the rainfall data", value = "MyVarName"),
                                    actionButton("GetMaxValues", "Get max. values"),
                                    tags$hr(),
                                    tags$br(),
                                    actionButton("GetEVIValues", "Get EVI"),
                                    tags$hr(),
                                    tags$br(),
                                    sliderInput("AlphaPlot2", "Alpha plot", min=0.0, max=1.0, value = 0.5),
                                    selectInput("ColorPlot2", "Color Plot", choices = c("black","blue","brown","cyan","gray","green","magenta","maroon","orange", "pink",
                                                                                       "purple","red","steelblue","violet","white","yellow"), selected = "steelblue"),
                                    textInput("VarLabel", "Insert name of label var.", value="Precipitation (mm)"),
                                    actionButton("GetPlots", "Get plots"),
                                    selectInput("PlotExtent", "Select format", choices = c(".svg",".png")),
                                    actionButton("DownloadEVIPlots", "Download plot"),
                                    tags$hr(),
                                    tags$br(),
                                    tags$p("Goodness Of Fit Test"),
                                    selectInput("Significance","Insert level of significance",choices = c("99%"="0.01","98%"="0.02","95%"="0.05","90%"="0.1","80%"="0.2")),
                                    actionButton("KolmogorovS","Kolmogorov"),
                                    actionButton("R2", "Coefficient of determination"),
                                    tags$hr(),
                                    tags$br(),
                                    tags$p("Estimations"),
                                    sliderInput("ReturnPeriod","Select a return period (T)",min = 1, max=1000, value = 10),
                                    sliderInput("Selectxt","Select a value (xt)",min = 0, max=10000, value = 491),
                                    sliderInput("POcurrence","Select a probability of ocurrence (P)",min = 0.01, max=1.00, value = 0.90, step = 0.01),
                                    sliderInput("POcurrenceY","Select a year to estimate the probability of ocurrence that a T event will ocurr at a least once in N years (Pn)",min = 1, max=1000, value = 21),
                                    sliderInput("Selectyt","Select a reduce variate (yt)",min = 0, max=10, value = 2.25, step = 0.01),
                                    tags$p("T: Return period"),
                                    tags$p("xt: Magnitude used as reference to estimate the ocurrence of any extreme event X "),
                                    tags$p("P: Probability of ocurrence to any event X <= xt"),
                                    tags$p("Pn: Probability of ocurrence that a T event will ocurr at a least once in N years"),
                                    tags$p("yt: Reduce variate, define as: (xt - u)/a")
                                  ), # End of SideBarPanel 4.1
                                  mainPanel(
                                    textOutput("Structure"),
                                    DT::dataTableOutput("MaxiTable"),
                                    DT::dataTableOutput("EVIMaxiTable"),
                                    plotOutput("EVIPlotsOut"),
                                    textOutput("ShowKolmogorovS"),
                                    textOutput("ShowR2"),
                                    tableOutput("Estimations")
                                  ) # End of MainPanel 4.1
                                  ) # End of TabPanel 4.1
                         ) # End of NavBarMenu 4.0
                ) #Close the navbarPage
#######################################################################################################################

######################################################## FUNCTIONS ####################################################
server = function(input,output, session){
  
  ##### 1.0.A. FUNCTIONS OF FIRST TAB -A ######
  #--------------------------------------#
  # Download data from CHRIPS
  
  observeEvent(input$init, {
    source("Scripts/RainfallData.R")
    Direc = setwd(chartr("\\","/",  choose.dir()))
    #r_global$volumes = c("~/")
    #Direc = "~/"

    Desc = MyDataRain(input$Regions,input$TResolution, input$Date1, input$Date2)
    if(input$TResolution=="monthly"){
      MyDates = seq(as.Date(input$Date1), as.Date(input$Date2), by="month")
    }else{
      MyDates = seq(as.Date(input$Date1), as.Date(input$Date2), by="day")
    }
    
    withProgress(message = "Downloading", value=0,{ #Begin BarProgress
    for(i in 1:nrow(Desc)){
      download.file(paste(Desc[i,1]), destfile = paste(input$Regions, input$TResolution, MyDates[i],".tif.gz", sep=""), method = "auto")
      incProgress(1/nrow(Desc), detail = paste((i/nrow(Desc))*100),"%")
      Sys.sleep(0.1)
    }
    })#end of BarProgress
  })
  

  #--------------------------------------#
  # Unzip the downloaded data from CHIRPS
  observeEvent(input$Extract, {
    withProgress(message = "Uncompressing files",{#Begin BarProgress
      Directory = jchoose.dir()
      ChirpsFiles = list.files(Directory, (pattern = "\\.gz$"))
      ChirpsFiles2 = paste(Directory,"\\",ChirpsFiles, sep="")
      for(i in 1:length(ChirpsFiles2)){
        gunzip(ChirpsFiles2[i])
        Sys.sleep(0.1)
        incProgress(1/i, detail = paste(length(ChirpsFiles2),":",i, sep = ""))
      }
    })#end of BarProgress
  })
  #--------------------------------------#
  # Add a shapefile as mask
  # Display countries code
  DataCountrySHP = reactive({
    source("Scripts/loadcutshapes.R")
    TheCountries = CODES()
  })
  output$MyTableCountrySHP = DT::renderDataTable({DataCountrySHP()})
  #--------------------------------------#
  # To plot the first Shape
  SHP = eventReactive(input$adquireSHP,{
    MyDirShape = choose.dir()
    MyDirShape = gsub("\\","/", MyDirShape, fixed=TRUE)
    source("Scripts/loadcutshapes.R")
    FirstMapAdquisition = FirstMap(CodC= input$Country, LevelC= as.numeric(input$Level), Ubicacion = paste(MyDirShape,"/",sep=""))
  })
  output$PlotSHP = renderPlot({
    plot(SHP())
    })
  #--------------------------------------#
  # To display the first shape data
  DataSHP = eventReactive(input$adquireDataSHP,{
    source("Scripts/loadcutshapes.R")
    FirstMapAdquisition = DataFirstMap(CodC= input$Country, LevelC= as.numeric(input$Level))
  })
  output$MyTableSHP = DT::renderDataTable({DataSHP()})
  #--------------------------------------#
  # To plot the second Shape
  SHP2 = eventReactive(input$adquireSHP2,{
    source("Scripts/loadcutshapes.R")
    FirstMapAdquisition = SecondMap(CodC= input$Country, LevelC= as.numeric(input$Level),  LevelD = as.numeric(input$subLevel) ,RegionD = input$Region)
  })
  output$PlotSHP2 = renderPlot({plot(SHP2())})
  #--------------------------------------#
  #Apply cut to Raster
  inFile = eventReactive(input$Mk,{
    #Maskara = input$Mk
    #if(is.null(Maskara)){return(NULL)}
    if(input$SelectShape == "First Shape"){
      ChoosedShape = readRDS(file.choose())
    }
    else if(input$SelectShape == "Second Shape"){
      ChoosedShape = readRDS(file.choose())
    }
    else if (input$SelectShape == "My own mask"){
      ChoosedShape = readOGR(file.choose())
      }
    else{ChoosedShape = "Not uploaded"}
    })  
  output$MaskLoadedInf = renderPrint({
    paste(input$SelectShape, " has been selected as mask to the crop raster files")
  })
  #--------------------------------------#
  observeEvent(input$cutRaster,{
    RasterDir = choose.dir()
    RasterFiles = list.files(RasterDir, pattern = "\\.tif$")
    RasterFiles2 = paste(RasterDir,"\\",RasterFiles, sep="")
    RasterFiles2 = gsub("\\","/", RasterFiles2, fixed=TRUE)
    mask = inFile()
    #mask = readRDS(PathFile$datapath)
    Seleccion = input$SelectShape
    WrittenName = gsub("\\.tif$","_",RasterFiles2)
    withProgress(message = "Cropping",value = 0,{ #Begin BarProgress
    for(i in 1:length(RasterFiles2)){
      if(Seleccion == "First Shape"){
        writeRaster(raster::crop(raster(paste(RasterFiles2[i])), mask), filename = paste(WrittenName[i],"Cropped", i, ".tif" ,sep=""), overwrite=TRUE ) 
        incProgress(1/length(RasterFiles2), detail = paste(i))
        Sys.sleep(0.1)
        }else if (paste(Seleccion) == "Second Shape"){
          if(as.numeric(paste(input$subLevel))==1){
           writeRaster(raster::crop(raster(paste(RasterFiles2[i])), mask[mask@data$NAME_1 == paste(input$Region),]), filename = paste(WrittenName[i],"Cropped", ".tif" ,sep=""),overwrite=TRUE)
            incProgress(1/length(RasterFiles2), detail = paste(i))
            Sys.sleep(0.1)
            }#end of if 
          else{
            writeRaster(raster::crop(raster(paste(RasterFiles2[i])), mask[mask@data$NAME_2 == paste(input$Region),]),filename = paste(WrittenName[i],"Cropped", ".tif" ,sep=""),overwrite=TRUE)
            incProgress(1/length(RasterFiles2), detail = paste(i))
            Sys.sleep(0.1)
            }#end of else
        }else{
          writeRaster(raster::crop(raster(paste(RasterFiles2[i])), mask), filename = paste(WrittenName[i],"Cropped", ".tif" ,sep=""),overwrite=TRUE)
          incProgress(1/length(RasterFiles2), detail = paste(i))
          Sys.sleep(0.1)
        }#end of else	
    }#End of for bucle
      })#end of BarProgress
  }) 

  ##### 2.0. FUNCTIONS OF SECOND TAB ######
  #--------------------------------------#
  ##### 2.1. FUNCTIONS OF SECOND TAB ######
  projectionElev = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  #--------------------------------------#
  # To show a table with coordinates (max. and min.) to plot the raster
  output$Extention = renderDataTable({
    RasterMapSelected = input$SelectRaster
    if(is.null(RasterMapSelected)){return(NULL)}
    DataRaster = raster(RasterMapSelected$datapath)
    df_RasterMapSelected = data.frame(x = c(DataRaster@extent[1],DataRaster@extent[2]), y = c(DataRaster@extent[3],DataRaster@extent[4]))
    rownames(df_RasterMapSelected) = c("Min","Max")
    df_RasterMapSelected
  })
  #--------------------------------------#
  # To get extention from another raster mask
  GetElevation= eventReactive(input$GetElev,{
    ExtentionsSelected = data.frame(x=c(as.numeric(input$XMin), as.numeric(input$XMax)), y=c(as.numeric(input$YMin),as.numeric(input$YMax)))
    GridExtention = data.frame(x=rep(seq(ExtentionsSelected[1,1],ExtentionsSelected[2,1],length.out = as.numeric(input$NGrids)),as.numeric(input$NGrids)), sort(rep(seq(ExtentionsSelected[1,2],ExtentionsSelected[2,2],length.out = as.numeric(input$NGrids)),as.numeric(input$NGrids))))
  })
  # To plot raster of heights and contour lines
  output$ElevationPlot = renderPlot({
    Raster_Elevations = get_elev_raster(GetElevation(),prj = projectionElev, z = as.numeric(input$ZoomLevel))
    Points_Elevations = SpatialPoints(GetElevation(),proj4string = CRS(projectionElev))
    Data_Elevations = raster::extract(Raster_Elevations, Points_Elevations@coords)
    Data_Elevations = as.data.frame(Data_Elevations)
    colnames(Data_Elevations) = c("Heights")
    Coordinates_Elevations = as.data.frame(Points_Elevations@coords)
    colnames(Coordinates_Elevations) = c("x","y")
    Data_Elevations = cbind(Coordinates_Elevations,Data_Elevations)
    X= unique(Data_Elevations$x)
    Y= unique(Data_Elevations$y)
    Z= matrix(Data_Elevations$Heights, ncol = length(Y), nrow = length(X))
    par(mfrow=c(1,2))
    plot(Raster_Elevations)
    plot(Points_Elevations, add=TRUE)
    contour(X,Y,Z, nlevels=as.numeric(input$NLevels))
    })
  
    #--------------------------------------#
  # To download the data of heights
  observeEvent(input$DownloadHeights,{
    Raster_Elevations = get_elev_raster(GetElevation(),prj = projectionElev, z = as.numeric(input$ZoomLevel))
    Points_Elevations = SpatialPoints(GetElevation(),proj4string = CRS(projectionElev))
    Data_Elevations = raster::extract(Raster_Elevations, Points_Elevations@coords)
    Data_Elevations = as.data.frame(Data_Elevations)
    colnames(Data_Elevations) = c("Heights")
    Coordinates_Elevations = as.data.frame(Points_Elevations@coords)
    colnames(Coordinates_Elevations) = c("x","y")
    Data_Elevations = cbind(Coordinates_Elevations,Data_Elevations)
    DirDownload = jchoose.dir()
    write.csv(Data_Elevations, paste(DirDownload,"\\","DataElevations.csv", sep=""))
    })
    #--------------------------------------#
  ##### 2.2. FUNCTIONS OF SECOND TAB ######
  #--------------------------------------#
  # The main map
  output$InteractiveMap = renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery")%>%
      setView(lng = -76.3, lat = 3.26, zoom = 12)%>%
      addDrawToolbar(targetGroup='draw',
                     polylineOptions=drawPolylineOptions(shapeOptions = drawShapeOptions()),
                     polygonOptions = drawPolygonOptions(showArea = TRUE,shapeOptions = drawShapeOptions(clickable = TRUE),metric = TRUE),
                     markerOptions = drawMarkerOptions(),
                     circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions()),
                     circleMarkerOptions = drawCircleMarkerOptions(),
                     editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())) %>%
      addLayersControl(overlayGroups = c('draw'), options = layersControlOptions(collapsed=FALSE)) %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479")%>%
      addMiniMap(toggleDisplay = TRUE)
  })
  #--------------------------------------#
  # Add points to main map
  observe({
    MYDATAPOINTS = input$MyDataPoints
    if(is.null(MYDATAPOINTS)){
      return(NULL)
    }
    MYDATAPOINTSFILE = read.csv(MYDATAPOINTS$datapath, header = TRUE, sep = ",", dec = ".")
    
    
    leafletProxy("InteractiveMap")%>%
      addCircleMarkers(data = MYDATAPOINTSFILE, lng = ~x, lat = ~y, label = ~as.character(Heights) ,radius = 5, color = ~ ifelse(Heights == "red","green","blue"),clusterOptions = markerClusterOptions())
  })
  #--------------------------------------#
  # Add raster to main map
  MyRasterCutted= reactive({
    RasterCUTTED = input$RasterCutted
    if(is.null(RasterCUTTED)){
      return(NULL)
    }
    RASTERCUTTED = raster(RasterCUTTED$datapath)
  })
  #--------------------------------------#
  observe({ #Show Raster at interactive map
    RasterCUTTED = input$RasterCutted
    if(is.null(RasterCUTTED)){
      return(NULL)
    }
    RASTERCUTTED = raster(RasterCUTTED$datapath)
    
    pal <- colorNumeric(palette = "Blues", domain= values(RASTERCUTTED), na.color = "transparent")
    
    leafletProxy("InteractiveMap") %>%
      addRasterImage(RASTERCUTTED, colors = pal, opacity = 0.8, group = "RasterRaifall")%>%
      addLegend(pal = pal, values = values(RASTERCUTTED), title = "Values")
  })
  #--------------------------------------#
  # Visualize points
  output$mytableValues = DT::renderDataTable({
    if(is.null(MyRasterCutted())){
      return(NULL)
    }
    DF= rasterToPoints(MyRasterCutted())
    colnames(DF) = c("x","y","data")
    DF = data.frame(DF)
    Extension = extent(MyRasterCutted())
    Proyeccion = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    CapturarElevacion = get_elev_raster(DF[,c(1,2)], prj = Proyeccion, z=5)
    PuntosExtension = SpatialPoints(DF[,c(1,2)], proj4string = CRS(Proyeccion))
    ElevacionesRequeridas = raster::extract(CapturarElevacion,PuntosExtension@coords)
    ElevacionesRequeridas = data.frame(ElevacionesRequeridas)
    colnames(ElevacionesRequeridas) = c("Heights")
    DF = cbind(DF, ElevacionesRequeridas)
    DT::datatable(
      DF, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),pageLength = nrow(DF)
      )
    )
  })
  #--------------------------------------#
  # Download all raster data
  observeEvent(input$DownloadData,{
    DirDownload = jchoose.dir()
    SetOfRasters = list.files(DirDownload, pattern = "_Cropped.tif$", full.names = TRUE)
    StacksRaster = stack(SetOfRasters)
    if(is.null(MyRasterCutted())){
      return(NULL)
    }
    DF= rasterToPoints(MyRasterCutted())
    colnames(DF) = c("x","y","data")
    DF = data.frame(DF)
    Extension = extent(MyRasterCutted())
    Proyeccion = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    CapturarElevacion = get_elev_raster(DF[,c(1,2)], prj = Proyeccion, z=5)
    PuntosExtension = SpatialPoints(DF[,c(1,2)], proj4string = CRS(Proyeccion))
    ElevacionesRequeridas = raster::extract(CapturarElevacion,PuntosExtension@coords)
    ElevacionesRequeridas = data.frame(ElevacionesRequeridas)
    colnames(ElevacionesRequeridas) = c("Heights")
    ElevacionesRequeridas = rep(ElevacionesRequeridas[["Heights"]], nlayers(StacksRaster))
    ElevacionesRequeridas = data.frame(ElevacionesRequeridas)
    colnames(ElevacionesRequeridas) = c("Heights")
    
    Fechas = gsub(SetOfRasters, pattern = "_Cropped.tif$", replacement = "")
    Fechas = substr(Fechas, nchar(Fechas)-9,nchar(Fechas))
    Fechas = rep(Fechas, length(values(StacksRaster[[1]])))
    Fechas = sort(Fechas, decreasing = FALSE)
    Fechas = data.frame(Fechas)
    
    RasterDF = melt(as.data.frame(StacksRaster[[1:nlayers(StacksRaster)]], row.names=NULL, optional=FALSE, xy=TRUE, na.rm=FALSE), id=c("x","y"))
    RasterDF = RasterDF[,-c(3)]
    RasterDF = cbind(Fechas,RasterDF)
    
    DF_final = cbind(RasterDF, ElevacionesRequeridas)
    
    write.csv(DF_final, paste(DirDownload,"\\","MyDataSet.csv", sep=""))
    progress(session, id = "pb")
  })
#--------------------------------------#
  ##### 3.0. FUNCTIONS OF THIRD TAB #####
  #--------------------------------------#
  ##### 3.1. FUNCTIONS OF THIRD TAB #####
  #Upload data for cluster
  DataForCluster = reactive({
    dflocation = input$DataCluster
    if(is.null(dflocation)){return(NULL)}
    df = read.csv(dflocation$datapath, header=TRUE, stringsAsFactors = FALSE) 
  }) 
  output$DataTableForCluster = DT::renderDataTable({
    DT::datatable(DataForCluster())
  })
  #--------------------------------------#
  #Select the best number of centers
  MyBestKValues = eventReactive(input$BestKM,{
    df_To_scaled = DataForCluster()
    VariablesCol1 = input$Variables
    VariablesCol2 =c(strsplit(VariablesCol1,","))
    df_To_scaled = df_To_scaled[, c(VariablesCol2[[1]])]
  })
  output$BestKMeanPlot = renderPlot({
    source("Scripts/kmeansv.R")
    TBKM = TheBestKMeans(MyBestKValues(),"silhouette")
    plot(TBKM)
  })
  #--------------------------------------#
  #Make cluster
  MyClusterKM = eventReactive(input$MakeMyCluster,{
    df_To_scaled = DataForCluster()
    VariablesCol1 = input$Variables
    VariablesCol2 =c(strsplit(VariablesCol1,","))
    df_To_scaled = df_To_scaled[, c(VariablesCol2[[1]])]
  })
  output$MyPlotClusterKN = renderPlot({
    source("Scripts/kmeansv.R")
    KMeansList = MakeKMeans(MyClusterKM(), as.numeric(paste(input$SelectCenters)), as.numeric(paste(input$SelectIterations)), 25)
    plot(KMeansList[[3]])
  })
  #--------------------------------------#
  #Show data cluster
  ShowClusterKM = eventReactive(input$ShowMyCluster,{
    df_To_scaled = DataForCluster()
    VariablesCol1 = input$Variables
    VariablesCol2 =c(strsplit(VariablesCol1,","))
    df_To_scaled = df_To_scaled[, c(VariablesCol2[[1]])]
  })
  output$ShowMyDataCluster = DT::renderDataTable({
    source("Scripts/kmeansv.R")
    KMeansList = MakeKMeans(ShowClusterKM(), as.numeric(paste(input$SelectCenters)), as.numeric(paste(input$SelectIterations)), 25)
    df_DataSet = DataForCluster()
    df_DataSet1 = input$Variables
    df_DataSet2 =c(strsplit(df_DataSet1,","))
    df_DataSet3 = df_DataSet[!(names(df_DataSet) %in% c(df_DataSet2[[1]]))]
    
    KMeansList = cbind(df_DataSet3, KMeansList[[2]])
    
    DT::datatable(
      KMeansList, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),pageLength = nrow(KMeansList)
      )
    )
  })
  #--------------------------------------#
  ##### 3.2. FUNTIONS OF THE THIRD TAB #####
  #Upload Dataset
  FileDataSet = reactive({
    MyDataSet = input$UploadDataSet
    if(is.null(MyDataSet)){return(NULL)}
    MyDataSetPath = MyDataSet$datapath
    Dataset = read.csv(MyDataSetPath,sep=",",dec=".",header=TRUE,stringsAsFactors = FALSE)
  })
  output$MyDataSetT = DT::renderDataTable({
    MyTableDataSet = FileDataSet()
    DT::datatable(
      MyTableDataSet, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),pageLength = 10#,pageLength = nrow(MyTableDataSet)
      )
    )
  })
  #--------------------------------------#
  #Separate dates
  missingValuesByDates = eventReactive(input$FilterByDate,{
    MyFile = FileDataSet()
    source("Scripts/NAValues.R")
    MyFile2 = SeparateDates(MyFile, input$NameOfDates)
  })
  output$missingValuesByDatesT = DT::renderDataTable({
    MyTableDataSetDates = missingValuesByDates()
    DT::datatable(
      MyTableDataSetDates, extensions = 'Buttons', options = list(
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),pageLength = 10))
  })
  #--------------------------------------#
  #Filter Values by Cluster Upload
  missingValuesByCluster = eventReactive(input$PLotByClusterGroup,{
    MyFile = missingValuesByDates()
    source("Scripts/NAValues.R")
    MyFile2 = SeparateByCluster(MyFile, input$FilterByCluster)
  }) 
  output$ShowPlotsByCluster = renderPlot({
    MyFile = missingValuesByCluster() #
    plot1 = ggplot(MyFile, aes(y = MyFile[,input$NameOfYValue], x=factor(0))) +
      geom_boxplot(notch=TRUE, width=0.2, fill=input$ColorPlot, alpha=input$AlphaPlot) + labs(y = input$YLabel) + theme(axis.title.x=element_blank(),
                                                                                             axis.text.x=element_blank(),
                                                                                             axis.ticks.x=element_blank())
    #plot2 = data.frame(Information = c("Complete", "Missing"), Values =c(length(MyFile[,input$NameOfYValue]), length(na.omit(MyFile[,input$NameOfYValue]))-length(MyFile[,input$NameOfYValue])))
    plot2 = data.frame(Information = c("Complete", "Missing"), Values =c(length(MyFile[,input$NameOfYValue]), length(which(is.na(MyFile[,input$NameOfYValue])))))
    Plot2 = ggplot(data=plot2, aes(x=Information, y= Values, fill=Information)) + geom_bar(stat="identity", position = position_dodge(), alpha=input$AlphaPlot) +
        geom_text(aes(label=Values), vjust=-0.3, size=3.5, position = position_dodge(0.9)) +
        theme_minimal() + ylim(0, (plot2[1,2]+ plot2[2,2])) + scale_fill_manual(values=c(input$ColorPlot,"black"))+
        labs(title= paste("Total records: ", plot2[1,2]+ plot2[2,2], sep=""), x="Data")
    StDv = sd(MyFile[,input$NameOfYValue], na.rm=TRUE)
        plot3 =ggplot(MyFile, aes(x= MyFile[,input$NameOfDates], y = MyFile[,input$NameOfYValue])) + labs(x= input$XLabel, y= input$YLabel)+ ylim(min(MyFile[,input$NameOfYValue]), max(MyFile[,input$NameOfYValue])) +
      geom_line(color=input$ColorPlot, alpha=input$AlphaPlot) + geom_point(color=input$ColorPlot, alpha=input$AlphaPlot) + geom_errorbar(aes(ymin=MyFile[,input$NameOfYValue]-StDv, ymax= MyFile[,input$NameOfYValue]+StDv), width=.2, color=input$ColorPlot, alpha=input$AlphaPlot)
    plot4 = ggplot(MyFile, aes(x=MyFile[,input$NameOfYValue])) +  geom_density(alpha=input$AlphaPlot) + geom_histogram(aes(y=..density..),binwidth = 5, fill=input$ColorPlot, alpha=input$AlphaPlot)+
      geom_vline(aes(xintercept=mean(MyFile[,input$NameOfYValue])),linetype="dashed") +
      labs(x= input$YLabel, y="Density")
    ggpubr::ggarrange(plot1, Plot2, plot3, plot4, labels = c("A", "B", "C", "D"), ncol = 2, nrow  = 2)
  }) 
  #--------------------------------------#
  #Filter Values by Cluster Download
  observeEvent(input$DownloadPlotByCluster,{
    SelectDirPlot1 = jchoose.dir()
    MyFile = missingValuesByCluster()
    plot1 = ggplot(MyFile, aes(y = MyFile[,input$NameOfYValue], x = factor(0))) +
      geom_boxplot(notch=TRUE, width=0.2, fill=input$ColorPlot) + labs(y = input$YLabel) + theme(axis.title.x=element_blank(),
                                                                                                           axis.text.x=element_blank(),
                                                                                                           axis.ticks.x=element_blank())
    plot2 = data.frame(Information = c("Complete", "Missing"), Values =c(length(MyFile[,input$NameOfYValue]), length(na.omit(MyFile[,input$NameOfYValue]))-length(MyFile[,input$NameOfYValue])))
    Plot2 = ggplot(data=plot2, aes(x=Information, y= Values, fill=Information)) + geom_bar(stat="identity", alpha=input$AlphaPlot, position = position_dodge()) +
      geom_text(aes(label=Values), vjust=-0.3, size=3.5, position = position_dodge(0.9)) +
      theme_minimal() + ylim(0, (plot2[1,2]+ plot2[2,2])) + scale_fill_manual(values=c(input$ColorPlot,"black"))+
      labs(title= paste("Total records: ", plot2[1,2]+ plot2[2,2], sep=""), x="Data")
    StDv = sd(MyFile[,input$NameOfYValue], na.rm=TRUE)
    plot3 =ggplot(MyFile, aes(x= MyFile[,input$NameOfDates], y = MyFile[,input$NameOfYValue])) + labs(x= input$XLabel, y= input$YLabel)+ ylim(min(MyFile[,input$NameOfYValue]), max(MyFile[,input$NameOfYValue])) +
      geom_line(color=input$ColorPlot) + geom_point(color=input$ColorPlot) + geom_errorbar(aes(ymin=MyFile[,input$NameOfYValue]-StDv, ymax= MyFile[,input$NameOfYValue]+StDv), width=.2, color=input$ColorPlot)
    plot4 = ggplot(MyFile, aes(x=MyFile[,input$NameOfYValue])) +  geom_histogram(stat = "bin", bins= NULL, binwidth= NULL,aes(bins = 30, y=..density..), fill=input$ColorPlot)+
      geom_density(alpha=input$AlphaPlot) + geom_vline(aes(xintercept=mean(MyFile[,input$NameOfYValue])),linetype="dashed") +
      labs(x= input$YLabel, y="Density")
    plot5 = ggpubr::ggarrange(plot1, Plot2, plot3, plot4, labels = c("A", "B", "C", "D"), ncol = 2, nrow  = 2)
    
    if(input$ext == ".png"){ggsave(paste(SelectDirPlot1,"\\", "PlotsGruppedByCluster.png", sep=""))}
    else{ggsave(paste(SelectDirPlot1,"\\", "PlotsGruppedByCluster.svg", sep=""))}
  })
  #--------------------------------------#
  #Filter Values by Cluster -Show summary
  observeEvent(input$ShowSummaryC,{
    MyFile = missingValuesByCluster()
    output$PrintShowSummaryC = renderPrint(summary(MyFile[,input$NameOfYValue]))
  })
  #--------------------------------------#
  #Filter Values by Seasons
   MissingValuesByQuarter = reactive({
     source("Scripts/NAValues.R")
     MyFile = missingValuesByCluster()
     Season_1 = input$Season1
     Season_2 = input$Season2
     Season_3 = input$Season3
     Season_4 = input$Season4
     SeparatedBySeason(MyFile,Season_1,Season_2,Season_3,Season_4)
   })
  #--------------------------------------#
  #Filter Values by Season Upload Boxplots
   BoxplotByQuarterD=eventReactive(input$BoxplotByQuarter,{ #Boxplot by quarter
     MyFile = MissingValuesByQuarter()
     MyFile = rbind(data.frame(x= factor("Season 1"), y= MyFile[[1]][[input$NameOfYValue]]),
                    data.frame(x= factor("Season 2"), y= MyFile[[2]][[input$NameOfYValue]]),
                    data.frame(x= factor("Season 3"), y= MyFile[[3]][[input$NameOfYValue]]),
                    data.frame(x= factor("Season 4"), y= MyFile[[4]][[input$NameOfYValue]])) 
     plot1 = ggplot(MyFile, aes(y = y, x = x)) + geom_boxplot(notch=TRUE, width=0.2, fill=input$ColorPlot, alpha=input$AlphaPlot) + labs(y = input$YLabel) + theme(axis.title.x=element_blank())
     plot1
   })
   output$BoxplotByQuarterP = renderPlot({
     plot(BoxplotByQuarterD())
   })
   #--------------------------------------#
   #Filter Values by Season Download Boxplots
   observeEvent(input$DownloadBoxplotByQuarter,{
     SelectDirPlot2 = jchoose.dir()
     MyFile = MissingValuesByQuarter()
     MyFile = rbind(data.frame(x= factor("Season 1"), y= MyFile[[1]][[input$NameOfYValue]]),
                    data.frame(x= factor("Season 2"), y= MyFile[[2]][[input$NameOfYValue]]),
                    data.frame(x= factor("Season 3"), y= MyFile[[3]][[input$NameOfYValue]]),
                    data.frame(x= factor("Season 4"), y= MyFile[[4]][[input$NameOfYValue]])) 
     plot1 = ggplot(MyFile, aes(y = y, x = x)) + geom_boxplot(notch=TRUE, width=0.2, fill=input$ColorPlot, alpha=input$AlphaPlot) + labs(y = input$YLabel) + theme(axis.title.x=element_blank(),
                                                                                                                                        axis.text.x=element_blank(),
                                                                                                                                        axis.ticks.x=element_blank())
     if(input$ext == ".png"){ggsave(paste(SelectDirPlot2,"\\", "BoxplotGruppedByQuarter.png", sep=""))}
     else{ggsave(paste(SelectDirPlot2,"\\", "BoxplotGruppedByQuarter.svg", sep=""))}
   })
  #--------------------------------------#
  #Filter Values by Season Upload Barplots
   BarplotByQuarterD=eventReactive(input$BarplotByQuarter,{ #Barplot by quarter
     MyFile = MissingValuesByQuarter()
     MyFile1 = data.frame(y= MyFile[[1]][[input$NameOfYValue]])
     MyFile2 = data.frame(y= MyFile[[2]][[input$NameOfYValue]])
     MyFile3 = data.frame(y= MyFile[[3]][[input$NameOfYValue]])
     MyFile4 = data.frame(y= MyFile[[4]][[input$NameOfYValue]])
     #, fill= Information
     #, position=position_dodge() 
     plot2 = data.frame(Information = rep(c("Complete", "Missing"),4), Values =c(length(MyFile1[!is.na(MyFile1)]), length(MyFile1[is.na(MyFile1)]), length(MyFile2[!is.na(MyFile2)]), length(MyFile2[is.na(MyFile2)]), length(MyFile3[!is.na(MyFile3)]), length(MyFile3[is.na(MyFile3)]), length(MyFile4[!is.na(MyFile4)]), length(MyFile4[is.na(MyFile4)])), Seasons = c("Season1","Season1","Season 2", "Season 2", "Season 3", "Season 3", "Season 4", "Season 4") )
     Plot2 = ggplot(data=plot2, aes(x=Seasons, y= Values, fill= Information)) + geom_bar(stat="identity" , position=position_dodge(), alpha=input$AlphaPlot) +
       geom_text(aes(label=Values), vjust=-0.3, size=3.5, position = position_dodge(0.9)) +
       ylim(0, (plot2[1,2]+ plot2[2,2])) +
       labs(title= paste("Total records: ", sum(plot2[,2]), sep=""), x="Data") + scale_fill_manual(values = c(input$ColorPlot, "black"))
   })
   output$BarplotByQuarterP = renderPlot({
     plot(BarplotByQuarterD())
   })
  #--------------------------------------#
  #Filter Values by Season Download Barplots
   observeEvent(input$DownloadBarplotByQuarter,{
     SelectDirPlot2 = jchoose.dir()
     MyFile = MissingValuesByQuarter()
     MyFile1 = data.frame(y= MyFile[[1]][[input$NameOfYValue]])
     MyFile2 = data.frame(y= MyFile[[2]][[input$NameOfYValue]])
     MyFile3 = data.frame(y= MyFile[[3]][[input$NameOfYValue]])
     MyFile4 = data.frame(y= MyFile[[4]][[input$NameOfYValue]])
     #, fill= Information
     #, position=position_dodge() 
     plot2 = data.frame(Information = rep(c("Complete", "Missing"),4), Values =c(length(MyFile1[!is.na(MyFile1)]), length(MyFile1[is.na(MyFile1)]), length(MyFile2[!is.na(MyFile2)]), length(MyFile2[is.na(MyFile2)]), length(MyFile3[!is.na(MyFile3)]), length(MyFile3[is.na(MyFile3)]), length(MyFile4[!is.na(MyFile4)]), length(MyFile4[is.na(MyFile4)])), Seasons = c("Season1","Season1","Season 2", "Season 2", "Season 3", "Season 3", "Season 4", "Season 4") )
     Plot2 = ggplot(data=plot2, aes(x=Seasons, y= Values, fill= Information)) + geom_bar(stat="identity", position=position_dodge(), alpha=input$AlphaPlot) +
       geom_text(aes(label=Values), vjust=-0.3, size=3.5, position = position_dodge(0.9)) +
       ylim(0, (plot2[1,2]+ plot2[2,2])) +
       labs(title= paste("Total records: ", sum(plot2[,2]), sep=""), x="Data")+ scale_fill_manual(values = c(input$ColorPlot, "black"))                                                              
                                                                                                                                        
     if(input$ext == ".png"){ggsave(paste(SelectDirPlot2,"\\", "BarplotGruppedByQuarter.png", sep=""))}
     else{ggsave(paste(SelectDirPlot2,"\\", "BarplotGruppedByQuarter.svg", sep=""))}
   })
  #--------------------------------------#
  #Filter Values by Season Upload Lineplots
   LineplotByQuarterD=eventReactive(input$LineplotByQuarter,{#Lineplot by quarter
     MyFile = MissingValuesByQuarter()
     MyFile0 = dplyr::bind_rows(MyFile, .id = "Season") 
     MyFile1 = data.frame(MyFile[[1]])
     MyFile2 = data.frame(MyFile[[2]])
     MyFile3 = data.frame(MyFile[[3]])
     MyFile4 = data.frame(MyFile[[4]])
     Mini = min(MyFile0[,input$NameOfYValue], na.rm=TRUE)
     Maxi = max(MyFile0[,input$NameOfYValue], na.rm=TRUE)
     StDv1 = sd(MyFile1[,input$NameOfYValue], na.rm=TRUE)
     StDv2 = sd(MyFile2[,input$NameOfYValue], na.rm=TRUE)
     StDv3 = sd(MyFile3[,input$NameOfYValue], na.rm=TRUE)
     StDv4 = sd(MyFile4[,input$NameOfYValue], na.rm=TRUE)
     plot31 =ggplot(MyFile1, aes(x= MyFile1[,input$NameOfDates], y = MyFile1[,input$NameOfYValue])) + labs(x= input$XLabel, y= input$YLabel)+ ylim(Mini, Maxi) + ggtitle("Season 1") +
       geom_line(color=input$ColorPlot,alpha=input$AlphaPlot)+ geom_point(color=input$ColorPlot,alpha=input$AlphaPlot) + geom_errorbar(aes(ymin=MyFile1[,input$NameOfYValue]-StDv1, ymax= MyFile1[,input$NameOfYValue]+StDv1), width=.2, color=input$ColorPlot,alpha=input$AlphaPlot) 
     plot32 =ggplot(MyFile2, aes(x= MyFile2[,input$NameOfDates], y = MyFile2[,input$NameOfYValue])) + labs(x= input$XLabel, y= input$YLabel)+ ylim(Mini, Maxi) + ggtitle("Season 2") +
       geom_line(color=input$ColorPlot,alpha=input$AlphaPlot)+ geom_point(color=input$ColorPlot,alpha=input$AlphaPlot) + geom_errorbar(aes(ymin=MyFile2[,input$NameOfYValue]-StDv2, ymax= MyFile2[,input$NameOfYValue]+StDv2), width=.2, color=input$ColorPlot,alpha=input$AlphaPlot) 
     plot33 =ggplot(MyFile3, aes(x= MyFile3[,input$NameOfDates], y = MyFile3[,input$NameOfYValue])) + labs(x= input$XLabel, y= input$YLabel)+ ylim(Mini, Maxi) + ggtitle("Season 3") +
       geom_line(color=input$ColorPlot,alpha=input$AlphaPlot)+ geom_point(color=input$ColorPlot,alpha=input$AlphaPlot) + geom_errorbar(aes(ymin=MyFile3[,input$NameOfYValue]-StDv3, ymax= MyFile3[,input$NameOfYValue]+StDv3), width=.2, color=input$ColorPlot,alpha=input$AlphaPlot) 
     plot34 =ggplot(MyFile4, aes(x= MyFile4[,input$NameOfDates], y = MyFile4[,input$NameOfYValue])) + labs(x= input$XLabel, y= input$YLabel)+ ylim(Mini, Maxi) + ggtitle("Season 4") +
       geom_line(color=input$ColorPlot,alpha=input$AlphaPlot)+ geom_point(color=input$ColorPlot,alpha=input$AlphaPlot) + geom_errorbar(aes(ymin=MyFile4[,input$NameOfYValue]-StDv4, ymax= MyFile4[,input$NameOfYValue]+StDv4), width=.2, color=input$ColorPlot,alpha=input$AlphaPlot) 
     plot3 = ggpubr::ggarrange(plot31, plot32, plot33, plot34, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2, align = "hv")
   })
   output$LineplotByQuarterP = renderPlot({
     plot(LineplotByQuarterD())
   })
  #--------------------------------------#
  #Filter Values by Season Download Lineplots
   observeEvent(input$DownloadlineplotByQuarter,{
     MyFile = MissingValuesByQuarter()
     SelectDirPlot2 = jchoose.dir()
     MyFile0 = dplyr::bind_rows(MyFile, .id = "Season") 
     MyFile1 = data.frame(MyFile[[1]])
     MyFile2 = data.frame(MyFile[[2]])
     MyFile3 = data.frame(MyFile[[3]])
     MyFile4 = data.frame(MyFile[[4]])
     Mini = min(MyFile0[,input$NameOfYValue], na.rm=TRUE)
     Maxi = max(MyFile0[,input$NameOfYValue], na.rm=TRUE)
     StDv1 = sd(MyFile1[,input$NameOfYValue], na.rm=TRUE)
     StDv2 = sd(MyFile2[,input$NameOfYValue], na.rm=TRUE)
     StDv3 = sd(MyFile3[,input$NameOfYValue], na.rm=TRUE)
     StDv4 = sd(MyFile4[,input$NameOfYValue], na.rm=TRUE)
     plot31 =ggplot(MyFile1, aes(x= MyFile1[,input$NameOfDates], y = MyFile1[,input$NameOfYValue])) + labs(x= input$XLabel, y= input$YLabel)+ ylim(Mini, Maxi) + ggtitle("Season 1") +
       geom_line(color=input$ColorPlot,alpha=input$AlphaPlot)+ geom_point(color=input$ColorPlot,alpha=input$AlphaPlot) + geom_errorbar(aes(ymin=MyFile1[,input$NameOfYValue]-StDv1, ymax= MyFile1[,input$NameOfYValue]+StDv1), width=.2, color=input$ColorPlot,alpha=input$AlphaPlot) 
     plot32 =ggplot(MyFile2, aes(x= MyFile2[,input$NameOfDates], y = MyFile2[,input$NameOfYValue])) + labs(x= input$XLabel, y= input$YLabel)+ ylim(Mini, Maxi) + ggtitle("Season 2") +
       geom_line(color=input$ColorPlot,alpha=input$AlphaPlot)+ geom_point(color=input$ColorPlot,alpha=input$AlphaPlot) + geom_errorbar(aes(ymin=MyFile2[,input$NameOfYValue]-StDv2, ymax= MyFile2[,input$NameOfYValue]+StDv2), width=.2, color=input$ColorPlot,alpha=input$AlphaPlot) 
     plot33 =ggplot(MyFile3, aes(x= MyFile3[,input$NameOfDates], y = MyFile3[,input$NameOfYValue])) + labs(x= input$XLabel, y= input$YLabel)+ ylim(Mini, Maxi) + ggtitle("Season 3") +
       geom_line(color=input$ColorPlot,alpha=input$AlphaPlot)+ geom_point(color=input$ColorPlot,alpha=input$AlphaPlot) + geom_errorbar(aes(ymin=MyFile3[,input$NameOfYValue]-StDv3, ymax= MyFile3[,input$NameOfYValue]+StDv3), width=.2, color=input$ColorPlot,alpha=input$AlphaPlot) 
     plot34 =ggplot(MyFile4, aes(x= MyFile4[,input$NameOfDates], y = MyFile4[,input$NameOfYValue])) + labs(x= input$XLabel, y= input$YLabel)+ ylim(Mini, Maxi) + ggtitle("Season 4") +
       geom_line(color=input$ColorPlot,alpha=input$AlphaPlot)+ geom_point(color=input$ColorPlot,alpha=input$AlphaPlot) + geom_errorbar(aes(ymin=MyFile4[,input$NameOfYValue]-StDv4, ymax= MyFile4[,input$NameOfYValue]+StDv4), width=.2, color=input$ColorPlot,alpha=input$AlphaPlot) 
     plot3 = ggpubr::ggarrange(plot31, plot32, plot33, plot34, labels = c("A", "B", "C", "D"), ncol = 2, nrow = 2, align = "hv")
     
     if(input$ext == ".png"){ggsave(paste(SelectDirPlot2,"\\", "LineplotGruppedByQuarter.png", sep=""))}
     else{ggsave(paste(SelectDirPlot2,"\\", "LineplotGruppedByQuarter.svg", sep=""))}
   })
  #--------------------------------------#
  #Filter Values by Season Upload Histplots
   HistplotByQuarterD=eventReactive(input$HistplotByQuarter,{#Histogram by quarter
     MyFile = MissingValuesByQuarter()
     MyFile = dplyr::bind_rows(MyFile, .id="Seasons")
     plot4 = ggplot(MyFile, aes(MyFile[,input$NameOfYValue])) + geom_density() + geom_histogram(aes(y = ..density..),binwidth = 5, fill=input$ColorPlot, alpha=input$AlphaPlot) +
       facet_grid(.~Seasons) + labs(x= input$YLabel, y="Density")
     plot4
     })
   output$HistplotByQuarterP = renderPlot({
     plot(HistplotByQuarterD())
   })
  #--------------------------------------#
  #Filter Values by Season Download Histplots
   observeEvent(input$DownloadHistplotByQuarter,{
     MyFile = MissingValuesByQuarter()
     SelectDirPlot2 = jchoose.dir()
     MyFile = dplyr::bind_rows(MyFile, .id="Seasons")
     plot4 = ggplot(MyFile, aes(MyFile[,input$NameOfYValue])) + geom_density() + geom_histogram(aes(y = ..density..),binwidth = 5, fill=input$ColorPlot, alpha=input$AlphaPlot) +
       facet_grid(.~Seasons) + labs(x= input$YLabel, y="Density")
     plot4
     if(input$ext == ".png"){ggsave(paste(SelectDirPlot2,"\\", "HistplotGruppedByQuarter.png", sep=""))}
     else{ggsave(paste(SelectDirPlot2,"\\", "HistplotGruppedByQuarter.svg", sep=""))}
   })
   #--------------------------------------#
   #Filter Values by Season -Show summary
   observeEvent(input$ShowSummaryS,{
     MyFile = MissingValuesByQuarter()
     output$PrintShowSummaryS = renderPrint(cat("SEANSON 1 ===>", "\n",
                                                summary(MyFile[[1]][input$NameOfYValue]),
                                                "SEANSON 2 ===>", "\n",
                                                summary(MyFile[[2]][input$NameOfYValue]),
                                                "SEANSON 3 ===>", "\n",
                                                summary(MyFile[[3]][input$NameOfYValue]),
                                                "SEANSON 4 ===>", "\n",
                                                summary(MyFile[[4]][input$NameOfYValue]))
                                              )
     
   })
   #--------------------------------------#
   #Filter Values by Months
   MissingValuesByMonths = reactive({
     source("Scripts/NAValues.R")
     MyFile = missingValuesByCluster()
     SeparatedByMonth(MyFile)
   })
   #--------------------------------------#
   #Filter Values by Months Upload Boxplots
   BoxplotByMonthsD=eventReactive(input$BoxplotByMonths,{ #Boxplot by months
     MyFile = MissingValuesByMonths()
     MyFile = dplyr::bind_rows(MyFile, .id="Meses")
     plot1 = ggplot(MyFile, aes(factor(Months), value)) + geom_boxplot(fill=input$ColorPlot, alpha=input$AlphaPlot) + labs(x="Months", y=input$YLabel) +
       scale_x_discrete(labels=unique(MyFile$Meses)) +theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
   })
   output$BoxplotByMonthsP = renderPlot({
     plot(BoxplotByMonthsD())
   })
   #--------------------------------------#
   #Filter Values by Months Download Boxplots
   observeEvent(input$DownloadBoxplotByMonths,{
     SelectDirPlot2 = jchoose.dir()
     MyFile = MissingValuesByMonths()
     MyFile = dplyr::bind_rows(MyFile, .id="Meses")
     plot1 = ggplot(MyFile, aes(factor(Months), value)) + geom_boxplot(fill=input$ColorPlot, alpha=input$AlphaPlot) + labs(x="Months", y=input$YLabel) +
       scale_x_discrete(labels=unique(MyFile$Meses)) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
                                                                                                                                        
     if(input$ext == ".png"){ggsave(paste(SelectDirPlot2,"\\", "BoxplotGruppedByMonths.png", sep=""))}
     else{ggsave(paste(SelectDirPlot2,"\\", "BoxplotGruppedByMonths.svg", sep=""))}
   })
   #--------------------------------------#
   #Filter Values by Months Upload Barplots
   BarplotByMonthsD=eventReactive(input$BarplotByMonths,{ #Barplot by months
     MyFile = MissingValuesByMonths()
     MyFile = dplyr::bind_rows(MyFile, .id="Meses")
     MD = matrix(data=NA, ncol=2,nrow=12)
     for(i in 1:12){
       MD[i,1]=nrow(na.omit(subset(MyFile, Months == i, select = input$NameOfYValue)))
       MD[i,2]=nrow(subset(MyFile, Months == i, select = input$NameOfYValue)) - nrow(na.omit(subset(MyFile, Months == i, select = input$NameOfYValue)))
     }
     plot2 = data.frame(Information = rep(c("Complete", "Missing"),12), Values = c(MD[1,1],MD[1,2],MD[2,1],MD[2,2], 
                                                                                   MD[3,1],MD[3,2], MD[4,1],MD[4,2],
                                                                                   MD[5,1],MD[5,2], MD[6,1],MD[6,2],
                                                                                   MD[7,1],MD[7,2],MD[8,1],MD[8,2],
                                                                                   MD[9,1],MD[9,2],MD[10,1],MD[10,2],
                                                                                   MD[11,1],MD[11,2],MD[12,1],MD[12,2]),
                        Months=c("January","January","February","February","March","March","April","April","May","May", "June" ,"June",
                                 "July","July","August","August","September","September","Octuber","Octuber","November","November","December","December" ))
     #, position=position_dodge()
     #, fill= Information
     Plot2 = ggplot(data=plot2, aes(x=factor(Months), y= Values, fill= Information)) + geom_bar(stat="identity", position=position_dodge(), alpha=input$AlphaPlot) +
       geom_text(aes(label=Values), vjust=-0.3, size=3.5, position=position_dodge(0.9)) +
       ylim(0, max(MD[,1])) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
       labs(title= paste("Total records: ", sum(MD[,1])+sum(MD[,2]), sep=""), x="Data") + scale_fill_manual(values=c(input$ColorPlot,"black"))
     Plot2
   })
   output$BarplotByMonthsP = renderPlot({
     plot(BarplotByMonthsD())
   })
   #--------------------------------------#
   #Filter Values by Months Download Barplots
   observeEvent(input$DownloadBarplotByMonths,{
     SelectDirPlot2 = jchoose.dir()
     MyFile = MissingValuesByMonths()
     MyFile = dplyr::bind_rows(MyFile, .id="Meses")
     MD = matrix(data=NA, ncol=2,nrow=12)
     for(i in 1:12){
       MD[i,1]=nrow(na.omit(subset(MyFile, Months == i, select = input$NameOfYValue)))
       MD[i,2]=nrow(subset(MyFile, Months == i, select = input$NameOfYValue)) - nrow(na.omit(subset(MyFile, Months == i, select = input$NameOfYValue)))
     }
     plot2 = data.frame(Information = rep(c("Complete", "Missing"),12), Values = c(MD[1,1],MD[1,2],MD[2,1],MD[2,2], 
                                                                                   MD[3,1],MD[3,2], MD[4,1],MD[4,2],
                                                                                   MD[5,1],MD[5,2], MD[6,1],MD[6,2],
                                                                                   MD[7,1],MD[7,2],MD[8,1],MD[8,2],
                                                                                   MD[9,1],MD[9,2],MD[10,1],MD[10,2],
                                                                                   MD[11,1],MD[11,2],MD[12,1],MD[12,2]),
                        Months=c("January","January","February","February","March","March","April","April","May","May", "June" ,"June",
                                 "July","July","August","August","September","September","Octuber","Octuber","November","November","December","December" ))
     #, position=position_dodge()
     #, fill= Information
     Plot2 = ggplot(data=plot2, aes(x=Months, y= Values, fill= Information)) + geom_bar(stat="identity", position=position_dodge(), alpha=input$AlphaPlot) +
       geom_text(aes(label=Values), vjust=-0.3, size=3.5, position=position_dodge(0.9)) +
       ylim(0, max(plot2[,2])) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))+
       labs(title= paste("Total records: ", sum(plot2[,2]), sep=""), x="Data") + scale_fill_manual(values=c(input$ColorPlot,"black"))
     Plot2
     
     if(input$ext == ".png"){ggsave(paste(SelectDirPlot2,"\\", "BarplotGruppedByMonths.png", sep=""))}
     else{ggsave(paste(SelectDirPlot2,"\\", "BarplotGruppedByMonths.svg", sep=""))}
   })
   #--------------------------------------#
   #Filter Values by Months Upload Lineplots
   LineplotByMonthsD=eventReactive(input$LineplotByMonths,{ #Lineplot by months
     MyFile = MissingValuesByMonths()
     MyFile = dplyr::bind_rows(MyFile, .id="Meses")
     StDv = aggregate(MyFile[,input$NameOfYValue], list(MyFile$Meses), sd)
  
     ggplot(MyFile, aes(x= MyFile[,input$NameOfDates], y = MyFile[,input$NameOfYValue])) + geom_line(color=input$ColorPlot, alpha=input$AlphaPlot)+
       geom_point(color=input$ColorPlot, alpha=input$AlphaPlot) + #geom_errorbar(aes(ymin=MyFile[,input$NameOfYValue]-StDv[,2], ymax= MyFile[,input$NameOfYValue]+StDv[,2]), width=.2, color="steelblue")+
       labs(x= input$XLabel, y= input$YLabel)+facet_grid(Meses ~., scales = "fixed",space="fixed")+ 
       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
     })
   output$LineplotByMonthsP = renderPlot({
     plot(LineplotByMonthsD())
   })
   #--------------------------------------#
   #Filter Values by Months Download Lineplots
   observeEvent(input$DownloadLineplotByMonths,{
     SelectDirPlot2 = jchoose.dir()
     MyFile = MissingValuesByMonths()
     MyFile = dplyr::bind_rows(MyFile, .id="Meses")
     StDv = aggregate(MyFile[,input$NameOfYValue], list(MyFile$Meses), sd)
     ggplot(MyFile, aes(x= MyFile[,input$NameOfDates], y = MyFile[,input$NameOfYValue])) + geom_line(color=input$ColorPlot, alpha=input$AlphaPlot)+
       geom_point(color=input$ColorPlot, alpha=input$AlphaPlot) + #geom_errorbar(aes(ymin=MyFile[,input$NameOfYValue]-StDv[,2], ymax= MyFile[,input$NameOfYValue]+StDv[,2]), width=.2, color="steelblue")+
       labs(x= input$XLabel, y= input$YLabel)+facet_grid(Meses ~., scales = "free",space = "free")+ 
       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
     if(input$ext == ".png"){ggsave(paste(SelectDirPlot2,"\\", "LineplotGruppedByMonths.png", sep=""))}
     else{ggsave(paste(SelectDirPlot2,"\\", "LineplotGruppedByMonths.svg", sep=""))}
   })
   #--------------------------------------#
   #Filter Values by Months Upload Histplots
   HistplotByMonthsD=eventReactive(input$HistplotByMonths,{ #Histplot by months
     MyFile = MissingValuesByMonths()
     MyFile = dplyr::bind_rows(MyFile, .id="Meses")
     plot4 = ggplot(MyFile, aes(MyFile[,input$NameOfYValue])) + geom_density() + geom_histogram(aes(y = ..density..),binwidth = 5, fill=input$ColorPlot, alpha=input$AlphaPlot) +
       facet_grid(.~ Meses, scales = "free") + labs(x= input$YLabel, y="Density") + 
       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
     plot4
   })
   output$HistplotByMonthsP = renderPlot({
     plot(HistplotByMonthsD())
   })
   #--------------------------------------#
   #Filter Values by Months Download Histplots
   observeEvent(input$DownloadHistplotByMonths,{
     SelectDirPlot2 = jchoose.dir()
     MyFile = MissingValuesByMonths()
     MyFile = dplyr::bind_rows(MyFile, .id="Meses")
     plot4 = ggplot(MyFile, aes(MyFile[,input$NameOfYValue])) + geom_density() + geom_histogram(aes(y = ..density..),binwidth = 5, fill=input$ColorPlot, alpha=input$AlphaPlot) +
       facet_grid(.~ Meses, scales = "free") + labs(x= input$YLabel, y="Density") + 
       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
     plot4
     if(input$ext == ".png"){ggsave(paste(SelectDirPlot2,"\\", "HistplotGruppedByMonths.png", sep=""))}
     else{ggsave(paste(SelectDirPlot2,"\\", "HistplotGruppedByMonths.svg", sep=""))}
   })
   #--------------------------------------#
   #Filter Values by Season -Show summary
   observeEvent(input$ShowSummaryM,{
     MyFile = MissingValuesByMonths()
     output$PrintShowSummaryM = renderPrint(cat("JANUARY  ===>", "\n",
                                                summary(MyFile[[1]][input$NameOfYValue]),
                                                "FEBRUARY  ===>", "\n",
                                                summary(MyFile[[2]][input$NameOfYValue]),
                                                "MARCH  ===>", "\n",
                                                summary(MyFile[[3]][input$NameOfYValue]),
                                                "APRIL  ===>", "\n",
                                                summary(MyFile[[4]][input$NameOfYValue]),
                                                "MAY  ===>", "\n",
                                                summary(MyFile[[1]][input$NameOfYValue]),
                                                "JUNE  ===>", "\n",
                                                summary(MyFile[[2]][input$NameOfYValue]),
                                                "JULY  ===>", "\n",
                                                summary(MyFile[[3]][input$NameOfYValue]),
                                                "AUGUST  ===>", "\n",
                                                summary(MyFile[[4]][input$NameOfYValue]),
                                                "SEPTEMBER  ===>", "\n",
                                                summary(MyFile[[1]][input$NameOfYValue]),
                                                "OCTUBER ===>", "\n",
                                                summary(MyFile[[2]][input$NameOfYValue]),
                                                "NOVEMBER ===>", "\n",
                                                summary(MyFile[[3]][input$NameOfYValue]),
                                                "DECEMBER ===>", "\n",
                                                summary(MyFile[[4]][input$NameOfYValue])))
   })
   #Neural Network to fill missing values
   #--------------------------------------#
   PrepareData = eventReactive(input$FillMV,{
     library(caTools)
     library(ggfortify)
     source("Scripts/NAValues_RNA.R")
     DirToSaveNN = jchoose.dir()
     ##Infile = missingValuesByCluster()
     Infile =FileDataSet()
     Predictors = strsplit(c(paste(input$PredictorsV)), ",")
     HL = input$HiddenLayers
     DatesSeparated = SepararDates(Infile, input$NameOfDates)
     TrainingData = PreprararData(DatesSeparated,c(Predictors[[1]]), input$NameOfYValue, "MonthsN", input$FracTraining, "cluster")
     TrainingData
     FillData = RedNeuronalArt(Infile,TrainingData[[1]],c(Predictors[[1]]),input$NameOfYValue,TrainingData[[5]],TrainingData[[2]],TrainingData[[3]],HL[1],HL[2], input$Epochs)
     FillData
     write.csv(FillData$DataFilled,file = paste(DirToSaveNN,"\\", "dataFilled.csv", sep=""))
     Errors = c("MSE"=FillData$MeanSqError, "RMSE" = FillData$RootMeanSqError)
     write.table(Errors,file = paste(DirToSaveNN,"\\", "ErrorsdataFilled.txt", sep=""))
     FillData = FillData$DataFilled
   })
   output$PlotDataFilled = renderPlot({
     DataF = PrepareData()
     plotting = ggplot(DataF, aes(x=factor(DataF[,input$NameOfDates]), y=DataF[,input$NameOfYValue])) + 
       geom_point(color="steelblue", alpha=0.5) + labs(x= input$XLabel, y= input$YLabel) +
       theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
     plotting
   })
   #--------------------------------------#
  ##### 4.0. EXTREME VALUES THEORY TAB #####
  ##### 4.1. EXTREME VALUES #####
   #Upload dataset
   DS_Input = reactive({
     inFile = input$LoadData
     if(is.null(inFile)){return(NULL)}
     inFile = inFile$datapath
     read.csv(inFile, header = TRUE)
   })
   output$Structure = renderPrint({
     str(DS_Input())
   })
   #Get the maximum values
   MaxiValues = eventReactive(input$GetMaxValues,{
     source("Scripts/EVT.R")
     Dset = DataMax(DS_Input(), input$ColDates, input$ColVar)
     Dset
   })
   output$MaxiTable= DT::renderDataTable({
     DT::datatable(MaxiValues(),options = list(
       dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),pageLength = 5))
   })
   
   #Get EVI 
   EVIValues = eventReactive(input$GetEVIValues,{
     source("Scripts/EVT.R")
     Dset = DataMax(DS_Input(), input$ColDates, input$ColVar)
     GetAlpha = AlphaFq(Dset[["MaxValue"]])
     GetU = UFq(Dset[["MaxValue"]],GetAlpha)
     EVIMaxi = EVIMax(Dset,GetAlpha,GetU)
     EVIMaxi
   })
   output$EVIMaxiTable = DT::renderDataTable({
     DT::datatable(EVIValues(),options = list(
       dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),pageLength = 5))
   })
   #Get EVI plots
   EVIPlots = eventReactive(input$GetPlots,{
     source("Scripts/EVT.R")
     Dset = DataMax(DS_Input(), input$ColDates, input$ColVar)
     GetAlpha = AlphaFq(Dset[["MaxValue"]])
     GetU = UFq(Dset[["MaxValue"]],GetAlpha)
     EVIMaxi = EVIMax(Dset,GetAlpha,GetU)
     EVIValuesRP=EVI_RP(EVIMaxi)
     dframe = data.frame(cbind(EVIValuesRP["PR"],EVIMaxi["MaxSortedData"]))
     colnames(dframe) = c("PR","MaxSortedData")
     
     plt1 =ggplot(Dset, aes(MaxValue))+ 
       geom_histogram(binwidth = 5, aes(y=..density..), fill=input$ColorPlot2) + 
       geom_density(fill=input$ColorPlot2, alpha=input$AlphaPlot2) + labs(x=input$VarLabel)
     
     plt2 =ggplot(EVIMaxi, aes(x=MaxSortedData, y=EVI_Fx)) + geom_line(color=input$ColorPlot2, alpha=input$AlphaPlot2, size=2) +
        labs(x=input$VarLabel, y="P(X<=xt)")
     
     plt3 = ggplot(EVIMaxi, aes(x=yt, y=MaxSortedData)) + geom_line(color=input$ColorPlot2, alpha=input$AlphaPlot2, size=2)+
       geom_segment(aes(x=0, y=0, xend=0, yend=GetU), color="black",linetype="dashed")+
       geom_segment(aes(x=min(yt), y=GetU, xend=0, yend=GetU), color="black",linetype="dashed")+
       labs(x="yt", y=input$VarLabel)
     
     plt4= ggplot(dframe, aes(x=PR, y=MaxSortedData)) + geom_line(color=input$ColorPlot2, alpha=input$AlphaPlot2, size=2)+
       labs(x="Return period (years)", y=input$VarLabel)
     
     pltF = ggpubr::ggarrange(plt1, plt2, plt3, plt4,  labels = c("A", "B", "C", "D"), ncol = 2, nrow  = 2)
   })
   
   output$EVIPlotsOut = renderPlot({
     plot(EVIPlots())
   })
   
   observeEvent(input$DownloadEVIPlots,{
     DirS = jchoose.dir()
     NamePlot = paste(DirS,"\\","EVI_plots",input$PlotExtent, sep="")
     myPlot = EVIPlots()
     ggsave(filename = paste(NamePlot), plot= EVIPlots())
   })
  ###### Goodness Of Fit Test ######
   GetSKTest = eventReactive(input$KolmogorovS,{
     source("Scripts/EVT.R")
     Dset = DataMax(DS_Input(), input$ColDates, input$ColVar)
     GetAlpha = AlphaFq(Dset[["MaxValue"]])
     GetU = UFq(Dset[["MaxValue"]],GetAlpha)
     EVIMaxi = EVIMax(Dset,GetAlpha,GetU)
     GOFTSK = GoodnessOfFitTestKolmogorov(EVIMaxi,input$Significance)
   })
   output$ShowKolmogorovS = renderPrint({
     GetSKTest()
   })
   
   GetR2Test = eventReactive(input$R2,{
     source("Scripts/EVT.R")
     Dset = DataMax(DS_Input(), input$ColDates, input$ColVar)
     GetAlpha = AlphaFq(Dset[["MaxValue"]])
     GetU = UFq(Dset[["MaxValue"]],GetAlpha)
     EVIMaxi = EVIMax(Dset,GetAlpha,GetU)
     CoefD= R2(EVIMaxi)
   })
   output$ShowR2 = renderPrint({
     GetR2Test()
   })
   
   # Estimations
   output$Estimations = renderTable({
     source("Scripts/EVT.R")
     
     #if(is.null(Dset)){return(NULL)}
     
     if(input$ColDates == "MyDatesName" || input$ColVar == "MyVarName"){ cat("_^_^_^_^_^_^_^_^_")}
     else{
     Dset = DataMax(DS_Input(), input$ColDates, input$ColVar)
     
     GetAlpha = AlphaFq(Dset[["MaxValue"]])
     GetU = UFq(Dset[["MaxValue"]],GetAlpha)
     EVIMaxi = EVIMax(Dset,GetAlpha,GetU)
     
     DFd = data.frame("T"=c(input$ReturnPeriod,NA,NA,input$Selectxt,NA,NA,NA), 
                      "xt"=c(NA,input$Selectxt,NA,input$Selectxt,input$Selectxt,NA,NA), 
                      "P"=c(NA,NA,input$POcurrence,input$POcurrence,NA,input$POcurrence,NA), 
                      "Time"=c(NA,NA,NA,input$POcurrenceY,NA,NA,NA), 
                      "yt"=c(input$Selectyt,input$Selectyt,NA,input$Selectyt,input$Selectyt,input$Selectyt,NA))
     DFr = data.frame("Est."=c("xt =","P =", "T =", "Pn =", "xt =", "xt =", "a = ; u ="), 
                      "Val."=c(MaxValByT(input$ReturnPeriod,GetAlpha,GetU),
                               POcur(input$Selectxt,GetAlpha,GetU),
                               RetP(input$POcurrence),
                               POcurUnaVez(input$Selectxt,GetAlpha,GetU,input$POcurrenceY),
                               XtConYt(input$Selectyt,GetAlpha,GetU),
                               XtConPOcur(input$POcurrence,GetAlpha,GetU),
                               paste("a= ",GetAlpha, " u= ",GetU,sep="")
                               ))
     DReq = data.frame("Eqs." = c("yt= -log{log[T/(T-1)]} --> xt = u + a*yt",
                                  "yt=(xt-u)/a --> P= e(-e(-yt))",
                                  "T = 1/(1-P)",
                                  "yt=(xt-u)/a --> P= e(-e(-yt)) --> T= 1/P --> Pn= 1-[1-(1/T)]^N",
                                  "xt= u + a*yt",
                                  "yt= -log{log[1/(P)]} --> xt = u + a*yt",
                                  "a= [sqrt(6)*s]/pi --> u= mean - 0.5772*a"))
     
     DF = cbind(DFd,DFr,DReq)
     DF}
   })
   
   
   } #close server
#######################################################################################################################
shinyApp(ui=ui, server=server)

#runApp()
