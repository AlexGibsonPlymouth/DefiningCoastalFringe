
library(dplyr)
library(shiny)
library(leaflet)
library(sf)
library(shinyWidgets)
library(leaflet.extras)       # needed for addFullscreenControl
library(leaflet.extras2)      # used to run 'working' spinner whilst maps loading data
library(shinybusy)            # Adds spinner for when shiny is busy - doesn't include map render wait, for which I have another spinner
library(rlang)                # to use !!sym() for dynamic naming

# Initialise by reading in data 
#print(getwd)

tile_url <- "https://tile.thunderforest.com/landscape/{z}/{x}/{y}.png?apikey=3206247c209a4857be76555683f31cc4"
England_Outline <- read_sf(dsn = "./England_Outline_BSC", layer = "England_Outline_BSC")
WalesOutline <- read_sf(dsn = "./WalesOutline_BUC", layer = "WalesOutline_BUC")
ScotlandOutline <- read_sf(dsn = "./ScotlandOutline_BUC", layer = "ScotlandOutline_BUC")

LSOA21Map = st_read("./BSC_LSOA21_England_ForShiny.gpkg")                ### <<<<<<<<<<<<<<< WON'T NEED THIS

BlueLSOAs_All = read.csv("./BlueGreenCategorisation.csv")

Centroids = st_read("./LSOA21_Centroids_England.gpkg")


ui <- fluidPage(
  
  tags$style(HTML("
    .two-col .shiny-options-group {
      column-count: 2;
      -moz-column-count: 2;
      -webkit-column-count: 2;
    }
  ")),
  
  tags$style(HTML("
    .row {
      margin-bottom: 0px !important;
    }
  ")),
  
    tags$style(HTML("
    .control-label {
      margin-bottom: 2px;   /* default is 10px */
    }
  ")),
  
  tags$style(HTML("
    .form-group {
      margin-bottom: 5px !important;   /* default is ~15px */
    }
  ")),
  
  add_busy_spinner(spin = "pixel",color= 	"red", position = "top-right", margins = c(10,100)), # Note position = "full-page" plays spinner until page ready
  # "fading-circle" is the traditional spin, "pixel" is prominant
  
  
  # Page title
  titlePanel("Defining the coastal fringe"),
  
  sidebarLayout(
    sidebarPanel(width=4,
                   fluidRow(
                     column(width = 8,
                       sliderInput("distDef", 
                                   h5(HTML("<strong>Centroid distance from coast (km)</strong>")),           
                                   min=4, max = 40, value = 10, step = 1),
                       fluidRow(
                         column(width = 5,
                              h6(HTML("<strong>Include centroids?</strong>")) ,  
                         ),
                         column(width = 2,
                                div(style = "height:6px;"),
                                prettyToggle(
                                  inputId = "distCentroids",
                                  label_on = "Yes!", icon_on = icon("check"),
                                  status_on = "info", status_off = "warning",
                                  label_off = "No..", icon_off = icon("xmark")
                                )
                                
                        )
                       )
                      ),
                       column(width =4,
                              radioButtons(
                                inputId = "distResolution",
                                label = h5(HTML("<strong>Resolution</strong>")),
                                choices = c("BGC (Tidal rivers)",
                                            "BSC (Generalised)"),
                                selected = "BSC (Generalised)"
                              ),
                              
                             actionButton("go1", "Go!",
                                          style="color: #ffffff; background-color: #337ab7; border-color: #2e6da4")
                            ),
                   ),
                 fluidRow(
                   h5("Note: LSOAs are excluded if within the London Region or if, at the selected distance, can only
                   reach the coast via a straight-line which passes through the London Region.",
                      style = "text-align: left; margin-right:40px; margin-left:15px; margin-top:20px;"),
                   
                 ),
                   fluidRow(
                     #div(style = "height:10px;"),
                     tags$hr(style = "border-top: 2px solid black;
                                      width: 80%;
                                      margin-left: 0;
                                      margin-right: 0;
                                    "),
                     #div(style = "height:10px;"),
                     column(width = 7,
                            radioButtons(
                             inputId = "blueDefinition",
                             label = h5(HTML("<strong>Blue LSOA defintion</strong>")),
                             choices = c("Blue LSOAs no holes",
                                         "Blue LSOAs with holes",
                                         "Blue Proximate no holes",
                                         "Blue Proximate with holes"),
                             selected = "Blue LSOAs no holes"
                            ),
                            fluidRow(
                              column(width = 5,
                                     h6(HTML("<strong>Include centroids?</strong>")) ,  
                              ),
                              column(width = 2,
                                     div(style = "height:6px;"),
                                     prettyToggle(
                                       inputId = "blueCentroids",
                                       label_on = "Yes!", icon_on = icon("check"),
                                       status_on = "info", status_off = "warning",
                                       label_off = "No..", icon_off = icon("xmark")
                                     )
                                     
                              ),
                            ),
                       ),
                     column(width =5,
                            radioButtons(
                              inputId = "blueResolution",
                              label = h5(HTML("<strong>Resolution</strong>")),
                              choices = c("BGC (Tidal rivers)",
                                          "BSC (Generalised)"),
                              selected = "BSC (Generalised)"
                            ),
                            actionButton("go2", "Go!",
                                         style="color: #ffffff; background-color: #337ab7; border-color: #2e6da4")
                     )
                 ),
                 fluidRow(
                   h5("Note: LSOAs are excluded if within the London Region.",
                      style = "text-align: left; margin-right:40px; margin-left:15px; margin-top:20px;"),
                   
                 ),
                 fluidRow(
                   #div(style = "height:10px;"),
                   tags$hr(style = "border-top: 2px solid black;
                                      width: 80%;
                                      margin-left: 0;
                                      margin-right: 0;
                                    "),
                   #div(style = "height:10px;"),
                   h4("Alternatively, select from above and combine!", style = "text-align: left; margin-left:20px; margin-top:2px;"),
                  column(width = 4,                   
                         div(
                           actionButton("combine", "Combine", 
                                        style="color: #ffffff; background-color: #337ab7; border-color: #2e6da4"),
                           style = "text-align: left; margin-left:60px;"
                         )
                  ),
                  column(width = 3,
                        h6(HTML("<strong>Include centroids?</strong>")) ,  
                  ),
                  column(width = 2,
                        div(style = "height:6px;"),
                        prettyToggle(
                          inputId = "combineCentroids",
                          label_on = "Yes!", icon_on = icon("check"),
                          status_on = "info", status_off = "warning",
                          label_off = "No..", icon_off = icon("xmark")
                        )
                        
                 ),
                 
                 ),
                 fluidRow(
                   h5("Distance-based LSOAs will be yellow, blue-based LSOAs will be blue, and those which are both come out a dirty brown!", style = "text-align: left; margin-right:20px; margin-left:20px; margin-top:20px;"),
                   
                 )
    ),  # end of sidebarPanel
    
    mainPanel(width=8,
              h2("Defined Coastal Fringe", style = "margin-top:-45px;"),
              # Leaflet output container
              leafletOutput("map", height = "800px", width = "1200px")
    ) # end of mainPanel
  )   # end of sidebarLaout 
 )    # end of fluidPage

server <- function(input, output, session) {
  
  SubsetMap = reactiveVal(NULL) 
  CentroidMap = reactiveVal(NULL) 
  DistSubsetMap = reactiveVal(NULL) 
  BlueSubsetMap = reactiveVal(NULL) 
  

  # Render the Leaflet map
  output$map <- renderLeaflet({                               # 1818
    leaflet(options = leafletOptions(zoomControl=TRUE, zoomSnap=0)) %>%
      addSpinner() %>%
      startSpinner(options = list("lines" = 7, "length" = 20)) %>%
      #setView(-3.7,50.4,zoom=10.0) %>%
      setView(-0.3,53.7,zoom=9.0) %>%
      addTiles(tile_url,group="TileGroup") %>% 
      clearGroup("GreyGroup") %>%
      clearGroup("CoastalGroup") %>%
      clearGroup("ScopeGroup") %>%
      addPolygons(data=WalesOutline,color = "#444444", weight = 0.2, smoothFactor = 0.5,
                  opacity = 1, fillColor="lightgrey",
                  fillOpacity = 1,
                  layerId = ~LAD23CD,
                  group="GreyGroup")  %>% 
      addPolygons(data=ScotlandOutline,color = "#444444", weight = 0.2, smoothFactor = 0.5,
                  opacity = 1, fillColor="lightgrey",
                  fillOpacity = 1,
                  layerId = ~LAD23CD,
                  group="GreyGroup") %>%
      stopSpinner()
  })
  
  
# What to do when the "LSOA Centroid distance from coast" GO button is pressed  ##################
observeEvent(input$go1,{ 
      #print(paste0("input$distDef = ",input$distDef))
      #print(paste0("input$distResolution = ",input$distResolution))
      #print(paste0("input$distCentroids = ",input$distCentroids))

      if (input$distResolution == "BGC (Tidal rivers)"){
        #print("BGC (Tidal rivers)")
        Colname = paste0("BGC_",as.character(as.numeric(input$distDef)*1000))
        #print(paste0("Colname = ",Colname))
        WorkingData = BlueLSOAs_All %>% select(LSOA21CD,!!sym(Colname))
        SubsetMap(LSOA21Map %>% dplyr::left_join(WorkingData, by = "LSOA21CD") %>% dplyr::filter(!!sym(Colname) == 1))
        
      } else if (input$distResolution == "BSC (Generalised)"){
        #print("BSC (Generalised)")
        Colname = paste0("BSC_",as.character(as.numeric(input$distDef)*1000))
        #print(paste0("Colname = ",Colname))
        WorkingData = BlueLSOAs_All %>% select(LSOA21CD,!!sym(Colname))
        SubsetMap(LSOA21Map %>% dplyr::left_join(WorkingData, by = "LSOA21CD") %>% dplyr::filter(!!sym(Colname) == 1))
       
       }

      if (input$distCentroids) {
        CentroidMap(Centroids)
      }
      
  map <- leafletProxy("map")
      if (input$distCentroids){
        map %>% 
          addSpinner() %>%
          startSpinner(options = list("lines" = 7, "length" = 20)) %>%
          clearGroup("ScopeGroup") %>%
          clearGroup("CentroidGroup") %>%
          clearGroup("DistScopeGroup") %>%
          clearGroup("BlueScopeGroup") %>%
          addPolygons(data=SubsetMap(),
                      color = "black", 
                      weight = 0.5, 
                      smoothFactor = 0.5,
                      opacity = 0.7, 
                      fillColor="blue",
                      fillOpacity = 0.3,
                      group="ScopeGroup") %>%
          addCircleMarkers(data=CentroidMap(),
                           radius = 1,
                           color = "red",
                           fill = TRUE,
                           fillOpacity = 0.7,
                           group="CentroidGroup") %>%
          stopSpinner()
      } else {
        map %>% 
          addSpinner() %>%
          startSpinner(options = list("lines" = 7, "length" = 20)) %>%
          clearGroup("ScopeGroup") %>%
          clearGroup("CentroidGroup") %>%
          addPolygons(data=SubsetMap(),
                      color = "black", 
                      weight = 0.5, 
                      smoothFactor = 0.5,
                      opacity = 0.7, 
                      fillColor="blue",
                      fillOpacity = 0.3,
                      group="ScopeGroup") %>%
          stopSpinner()
      }
})
  
  
  

  
# What to do when the "Blue LSOA definition" GO button is pressed  ##################
observeEvent(input$go2,{ 
  #print(paste0("input$blueDefinition = ",input$blueDefinition))
  #print(paste0("input$blueResolution = ",input$blueResolution))
  #print(paste0("input$blueCentroids = ",input$blueCentroids))


  if (input$blueDefinition == "Blue LSOAs no holes" & input$blueResolution == "BGC (Tidal rivers)"){
    #print("Blue LSOAs no holes -&- BGC (Tidal rivers)")
    WorkingData = BlueLSOAs_All %>% select(LSOA21CD,BGC_Blue_noGaps_NoLondon)
    SubsetMap(LSOA21Map %>% dplyr::left_join(WorkingData, by = "LSOA21CD") %>% 
                dplyr::filter(BGC_Blue_noGaps_NoLondon == 1))
    
  } else if (input$blueDefinition == "Blue LSOAs with holes" & input$blueResolution == "BGC (Tidal rivers)"){
    #print("Blue LSOAs holes -&- BGC (Tidal rivers)")
    WorkingData = BlueLSOAs_All %>% select(LSOA21CD,BGC_Blue_Gaps_noLondon)
    SubsetMap(LSOA21Map %>% dplyr::left_join(WorkingData, by = "LSOA21CD") %>% 
                dplyr::filter(BGC_Blue_Gaps_noLondon == 1))
    
  } else   if (input$blueDefinition == "Blue Proximate no holes" & input$blueResolution == "BGC (Tidal rivers)"){
    #print("Blue Proximate no holes -&- BGC (Tidal rivers)")
    WorkingData = BlueLSOAs_All %>% select(LSOA21CD,BGC_BlueProximate_noGaps_NoLondon)
    SubsetMap(LSOA21Map %>% dplyr::left_join(WorkingData, by = "LSOA21CD") %>% 
                dplyr::filter(BGC_BlueProximate_noGaps_NoLondon == 1))
    
  } else if (input$blueDefinition == "Blue Proximate with holes" & input$blueResolution == "BGC (Tidal rivers)"){
    #print("Blue Proximate holes -&- BGC (Tidal rivers)")
    WorkingData = BlueLSOAs_All %>% select(LSOA21CD,BGC_BlueProximate_Gaps_noLondon)
    SubsetMap(LSOA21Map %>% dplyr::left_join(WorkingData, by = "LSOA21CD") %>% 
                dplyr::filter(BGC_BlueProximate_Gaps_noLondon == 1))

 
  } else if (input$blueDefinition == "Blue LSOAs no holes" & input$blueResolution == "BSC (Generalised)"){
    #print("Blue LSOAs no holes -&- BSC (Generalised)")
    WorkingData = BlueLSOAs_All %>% select(LSOA21CD,BSC_Blue_noGaps_NoLondon)
    SubsetMap(LSOA21Map %>% dplyr::left_join(WorkingData, by = "LSOA21CD") %>% 
                dplyr::filter(BSC_Blue_noGaps_NoLondon == 1))
 
  } else if (input$blueDefinition == "Blue LSOAs with holes" & input$blueResolution == "BSC (Generalised)"){
    #print("Blue LSOAs holes -&- BSC (Generalised)")
    WorkingData = BlueLSOAs_All %>% select(LSOA21CD,BSC_Blue_Gaps_noLondon)
    SubsetMap(LSOA21Map %>% dplyr::left_join(WorkingData, by = "LSOA21CD") %>% 
                dplyr::filter(BSC_Blue_Gaps_noLondon == 1))
    
  } else if (input$blueDefinition == "Blue Proximate no holes" & input$blueResolution == "BSC (Generalised)"){
    #print("Blue Proximate no holes -&- BSC (Generalised)")
    WorkingData = BlueLSOAs_All %>% select(LSOA21CD,BSC_BlueProximate_noGaps_NoLondon)
    SubsetMap(LSOA21Map %>% dplyr::left_join(WorkingData, by = "LSOA21CD") %>% 
                dplyr::filter(BSC_BlueProximate_noGaps_NoLondon == 1))

  } else if (input$blueDefinition == "Blue Proximate with holes" & input$blueResolution == "BSC (Generalised)"){
    #print("Blue Proximate holes -&- BSC (Generalised)")
    WorkingData = BlueLSOAs_All %>% select(LSOA21CD,BSC_BlueProximate_Gaps_noLondon)
    SubsetMap(LSOA21Map %>% dplyr::left_join(WorkingData, by = "LSOA21CD") %>% 
                dplyr::filter(BSC_BlueProximate_Gaps_noLondon == 1))
  } 
  
  
  if (input$blueCentroids) {
    CentroidMap(Centroids)
  }
  
  map <- leafletProxy("map")
  if (input$blueCentroids){
    map %>% 
      addSpinner() %>%
      startSpinner(options = list("lines" = 7, "length" = 20)) %>%
      clearGroup("ScopeGroup") %>%
      clearGroup("CentroidGroup") %>%
      clearGroup("DistScopeGroup") %>%
      clearGroup("BlueScopeGroup") %>%
      addPolygons(data=SubsetMap(),
                  color = "black", 
                  weight = 0.5, 
                  smoothFactor = 0.5,
                  opacity = 0.7, 
                  fillColor="blue",
                  fillOpacity = 0.3,
                  group="ScopeGroup") %>%
      addCircleMarkers(data=CentroidMap(),
                       radius = 1,
                       color = "red",
                       fill = TRUE,
                       fillOpacity = 0.7,
                       group="CentroidGroup") %>%
      stopSpinner()
  } else {
    map %>% 
      addSpinner() %>%
      startSpinner(options = list("lines" = 7, "length" = 20)) %>%
      clearGroup("ScopeGroup") %>%
      clearGroup("CentroidGroup") %>%
      addPolygons(data=SubsetMap(),
                  color = "black", 
                  weight = 0.5, 
                  smoothFactor = 0.5,
                  opacity = 0.7, 
                  fillColor="blue",
                  fillOpacity = 0.3,
                  group="ScopeGroup") %>%
      stopSpinner()
  }
})



# What to do when the "Combine" button is pressed
  observeEvent(input$combine,{ 
    #print(paste0("input$distDef = ",input$distDef))
    #print(paste0("input$distResolution = ",input$distResolution))
    #print(paste0("input$distCentroids = ",input$distCentroids))
    #print(paste0("input$combineCentroids = ",input$combineCentroids))
    #print(paste0("input$blueDefinition = ",input$blueDefinition))
    #print(paste0("input$blueResolution = ",input$blueResolution))
    #print(paste0("input$blueCentroids = ",input$blueCentroids))
    
    
    
    if (input$distResolution == "BGC (Tidal rivers)"){
        #print("BGC (Tidal rivers)")
        CombineColname = paste0("BGC_",as.character(as.numeric(input$distDef)*1000))
        #print(paste0("CombineColname >  ",CombineColname))
        DistWorkingData = BlueLSOAs_All %>% select(LSOA21CD,!!sym(CombineColname))
        
     } else if (input$distResolution == "BSC (Generalised)"){
         #print("BSC (Generalised)")
         CombineColname = paste0("BSC_",as.character(as.numeric(input$distDef)*1000))
         #print(paste0("CombineColname >  ",CombineColname))
         DistWorkingData = BlueLSOAs_All %>% select(LSOA21CD,!!sym(CombineColname))

     }
    
    
    if (input$blueDefinition == "Blue LSOAs no holes" & input$blueResolution == "BGC (Tidal rivers)"){
       #print("Blue LSOAs no holes -&- BGC (Tidal rivers)")
       BlueWorkingData = BlueLSOAs_All %>% select(LSOA21CD,BGC_Blue_noGaps_NoLondon) %>% 
         rename(BlueColumn = BGC_Blue_noGaps_NoLondon)
       
    } else if (input$blueDefinition == "Blue LSOAs with holes" & input$blueResolution == "BGC (Tidal rivers)"){
       #print("Blue LSOAs holes -&- BGC (Tidal rivers)")
       BlueWorkingData = BlueLSOAs_All %>% select(LSOA21CD,BGC_Blue_Gaps_noLondon) %>% 
         rename(BlueColumn = BGC_Blue_Gaps_noLondon)
       
    } else if (input$blueDefinition == "Blue Proximate no holes" & input$blueResolution == "BGC (Tidal rivers)"){
       #print("Blue Proximate no holes -&- BGC (Tidal rivers)")
       BlueWorkingData = BlueLSOAs_All %>% select(LSOA21CD,BGC_BlueProximate_noGaps_NoLondon) %>% 
         rename(BlueColumn = BGC_BlueProximate_noGaps_NoLondon)
       
    } else if (input$blueDefinition == "Blue Proximate with holes" & input$blueResolution == "BGC (Tidal rivers)"){
       #print("Blue Proximate holes -&- BGC (Tidal rivers)")
       BlueWorkingData = BlueLSOAs_All %>% select(LSOA21CD,BGC_BlueProximate_Gaps_noLondon) %>% 
         rename(BlueColumn = BGC_BlueProximate_Gaps_noLondon)
       
    } else if (input$blueDefinition == "Blue LSOAs no holes" & input$blueResolution == "BSC (Generalised)"){
       #print("Blue LSOAs no holes -&- BSC (Generalised)")
       BlueWorkingData = BlueLSOAs_All %>% select(LSOA21CD,BSC_Blue_noGaps_NoLondon) %>% 
         rename(BlueColumn = BSC_Blue_noGaps_NoLondon)
       
    } else if (input$blueDefinition == "Blue LSOAs with holes" & input$blueResolution == "BSC (Generalised)"){
       #print("Blue LSOAs holes -&- BSC (Generalised)")
       BlueWorkingData = BlueLSOAs_All %>% select(LSOA21CD,BSC_Blue_Gaps_noLondon) %>% 
         rename(BlueColumn = BSC_Blue_Gaps_noLondon)
       
    } else if (input$blueDefinition == "Blue Proximate no holes" & input$blueResolution == "BSC (Generalised)"){
       #print("Blue Proximate no holes -&- BSC (Generalised)")
       BlueWorkingData = BlueLSOAs_All %>% select(LSOA21CD,BSC_BlueProximate_noGaps_NoLondon) %>% 
         rename(BlueColumn = BSC_BlueProximate_noGaps_NoLondon)
       
    } else if (input$blueDefinition == "Blue Proximate with holes" & input$blueResolution == "BSC (Generalised)"){
       #print("Blue Proximate holes & London -&- BSC (Generalised)")
       BlueWorkingData = BlueLSOAs_All %>% select(LSOA21CD,BSC_BlueProximate_Gaps_noLondon) %>% 
         rename(BlueColumn = BSC_BlueProximate_Gaps_noLondon)
    } 
    
    # Combine DistWorkingData & BlueWorkingData
    #print(head(DistWorkingData))
    #print(head(BlueWorkingData))
    
    DistSubsetMap(LSOA21Map %>% dplyr::left_join(DistWorkingData, by = "LSOA21CD") %>% 
                dplyr::filter(!!sym(CombineColname) == 1))
    
    BlueSubsetMap(LSOA21Map %>% dplyr::left_join(BlueWorkingData, by = "LSOA21CD") %>% 
                    dplyr::filter(BlueColumn == 1))
    
  
    if (input$combineCentroids) {
      CentroidMap(Centroids)
    }
    
    map <- leafletProxy("map")
    if (input$combineCentroids){
      map %>% 
        addSpinner() %>%
        startSpinner(options = list("lines" = 7, "length" = 20)) %>%
        clearGroup("ScopeGroup") %>%
        clearGroup("CentroidGroup") %>%
        clearGroup("DistScopeGroup") %>%
        clearGroup("BlueScopeGroup") %>%
        addPolygons(data= DistSubsetMap(),
                    color = "black", 
                    weight = 0.5, 
                    smoothFactor = 0.5,
                    opacity = 0.7, 
                    fillColor="yellow",
                    fillOpacity = 0.3,
                    group="DistScopeGroup") %>%
        addPolygons(data=BlueSubsetMap(),
                    color = "black", 
                    weight = 0.5, 
                    smoothFactor = 0.5,
                    opacity = 0.7, 
                    fillColor="blue",
                    fillOpacity = 0.3,
                    group="BlueScopeGroup") %>%
        addCircleMarkers(data=CentroidMap(),
                         radius = 1,
                         color = "red",
                         fill = TRUE,
                         fillOpacity = 0.7,
                         group="CentroidGroup") %>%
        stopSpinner()
    } else {
      map %>% 
        addSpinner() %>%
        startSpinner(options = list("lines" = 7, "length" = 20)) %>%
        clearGroup("ScopeGroup") %>%
        clearGroup("CentroidGroup") %>%
        clearGroup("DistScopeGroup") %>%
        clearGroup("BlueScopeGroup") %>%
        addPolygons(data= DistSubsetMap(),
                    color = "black", 
                    weight = 0.5, 
                    smoothFactor = 0.5,
                    opacity = 0.7, 
                    fillColor="yellow",
                    fillOpacity = 0.3,
                    group="DistScopeGroup") %>%
        addPolygons(data=BlueSubsetMap(),
                    color = "black", 
                    weight = 0.5, 
                    smoothFactor = 0.5,
                    opacity = 0.7, 
                    fillColor="blue",
                    fillOpacity = 0.3,
                    group="BlueScopeGroup") %>%
        stopSpinner()
    }
    
  }) # end of observeEvent
  
  
  }

shinyApp(ui, server)

