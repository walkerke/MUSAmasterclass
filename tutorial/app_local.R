library(shiny)
library(leaflet)
library(mapboxapi)
library(sf)

# Read in the polling place data
ev_sites <- readr::read_rds("data/tarrant_EV_sites.rds") 

# Set up a sidebar panel with a text box for an input address, 
# and a placeholder to print out the driving instructions
ui <- fluidPage(
  sidebarPanel(
    textInput("address_text", label = "Address",
              placeholder = "Type an address or place name"),
    actionButton("action", "Find the nearest polling place"),
    htmlOutput("instructions"),
    width = 3
  ),
  mainPanel(
    leafletOutput(outputId = "map", width = "100%", height = 600)
  )
)

# Set up reactive elements to generate routes when the action button is clicked,
# then map the routes and print out the driving directions
server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addMapboxTiles(style_id = "streets-v11",
                     username = "mapbox")  %>%
      addMarkers(data = ev_sites, popup = ~name)
      
    
  })
  
  # Find the closest polling location with mb_matrix()
  # when the action button is clicked
  closest_location <- eventReactive(input$action, {
    
    input_sf <- mb_geocode(input$address_text, output = "sf") 
    
    st_crs(input_sf) <- 4326
    
    min_index <- mb_matrix(
      origins = input_sf,
      destinations = ev_sites
    ) %>%
      as.vector() %>%
      which.min()
    
    min_coords <- ev_sites[min_index, ] %>%
      st_coordinates() %>%
      as.vector()
    
    return(min_coords)
    
  })
  
  # Once the closest location is determined,
  # calculate a driving route to that location and 
  # fly to the route linestring on the map
  observeEvent(closest_location(), {
    
    route <- mb_directions(
      origin = input$address_text,
      destination = closest_location(),
      profile = "driving",
      output = "sf",
      steps = TRUE
    ) 
    
    st_crs(route) <- 4326

    flyto_coords <- route %>%
      st_union() %>%
      st_centroid() %>%
      st_coordinates() %>%
      as.vector()

    leafletProxy(mapId = "map") %>%
      clearShapes() %>%
      addPolylines(data = route, color = "black",
                   opacity = 1) %>%
      flyTo(lng = flyto_coords[1],
            lat = flyto_coords[2],
            zoom = 14)
    
    # Print out driving instructions in the sidebar
    output$instructions <- renderUI({
      HTML(paste0(
        paste("&bull;", route$instruction, sep = ""),
        collapse = "<br/>"))
    })
  })
  
}

shinyApp(ui = ui, server = server)

