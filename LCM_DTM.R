library(magrittr)
library(leaflet)
library(leaflet.extras2)
library(shiny)
library(stringr)

js <- HTML("
// make sure we keep a reference to the map as part of mapsPlaceholder
var mapsPlaceholder = [];

$(function() {
   // Before map is being initialized.
   L.Map.addInitHook(function () {
     mapsPlaceholder.push(this); // Use whatever global scope variable you like.
   });
})

Shiny.addCustomMessageHandler('fake_a_click', function(coords) {
   let map = mapsPlaceholder[0];
   map.fireEvent('click', {
      latlng: L.latLng(coords.lat, coords.lng),
      layerPoint: map.latLngToLayerPoint(L.latLng(coords.lat, coords.lng)),
      containerPoint: map.latLngToContainerPoint(L.latLng(coords.lat, coords.lng))
   });
})
")

ui <- fluidPage(
  tags$head(tags$script(js)),
  fluidRow(
    column(
      6, 
      textInput("map_coords", "Coordinates (Lng, Lat)", 
                placeholder = "Type in your coordinates here ...", width = "100%"),
      actionButton("map_validate", label = "Go!")
    )
  ),
  fluidRow(
    column(6, leafletOutput("map_habitats")),
    column(6, leafletOutput("map_elevation"))
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  wms_layer <- "https://catalogue.ceh.ac.uk/maps/51bcb92a-dd88-4034-ba65-a9d432dd632a?request=getCapabilities&service=WMS&cache=false&"
  
  rv_habitat <- reactiveValues(coords = list(lng = NULL, lat = NULL))
  
  
  # Add this in your server function
  output$map_elevation <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addWMS(
        "https://map.bgs.ac.uk/arcgis/services/UKSO/UKSO_BGS_Surface/MapServer/WMSServer", 
        layers = "OS.Terrain.50",  # Replace with the actual layer name for elevation data
        options = WMSTileOptions(
          format = "image/png",
          version = "1.3.0",
          transparent = TRUE,
          opacity = 0.5
        )
      ) %>%
      setView(lng = -2.55, lat = 54, zoom = 6)
  })
  
  observeEvent(input$show_elevation, {
    showModal(modalDialog(
      title = "Elevation Data",
      leafletOutput("map_elevation", width = "100%", height = "600px"),
      size = "l"
    ))
  })
  
  
  output$map_habitats <- renderLeaflet ({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery", 
                       group = "Esri.WorldImagery",
                       options = providerTileOptions(zIndex = 0)) %>%
      setView(lng = -2.55, lat = 54, zoom = 6) %>%
      addWMS(
        wms_layer,
        layers = "LC.10m.GB", # Or "LC.10m.NI" for northern Ireland
        options = WMSTileOptions(
          format = "image/png",
          version = "1.3.0",
          transparent = TRUE,
          opacity = 0.5,# Add some transparency so that we can still see the satellite image
          info_format = "application/vnd.ogc.gml"
        ),
        popupOptions = popupOptions(maxWidth = 300, closeOnClick = T))
  })
  
  observeEvent(input$map_validate, ignoreInit  = TRUE, 
               label = "Submit map coordinates", {
                 value2check <- str_split(input$map_coords, ",")[[1]] %>% 
                   as.numeric()
                 if (length(value2check) != 2){
                   updateTextInput(session, inputId = "map_coords", 
                                   value = "", 
                                   placeholder = "Type in 2 numeric values separated by a comma")
                 } else {
                   value2check <- value2check %>% 
                     set_names(c("lng", "lat")) 
                   if (!any(is.na(value2check))){
                     rv_habitat$coords <- as.list(value2check) 
                     session$sendCustomMessage("fake_a_click", as.list(value2check))
                   } else {
                     updateTextInput(session, inputId = "map_coords", value = "", 
                                     placeholder = "Type in 2 numeric values separated by a comma")
                   }
                 }
               })
}

# Run the application
shinyApp(ui = ui, server = server)
