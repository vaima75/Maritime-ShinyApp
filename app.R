library(shiny)
library(dplyr)
library(readr)
library(DT)
library(leaflet)

##### Function Declaration #####
hori_dist = function(speed = speed, theta = theta, g = 9.8){
    theta = theta * pi/180 # in radians
    return(abs((speed^2 * sin(2*theta)) / g))
}

dest_latlong = function(lat_src = lat_src, long_src = long_src, bear = bear, dist = dist, r = 6378137){
    # convert to radians
    lat_rad = lat_src * pi/180
    long_rad = long_src * pi/180
    ang_dis = (dist/r) * (pi/180)
    bear_rad = bear * pi/180
    dest_lat = asin( sin(lat_rad)*cos(ang_dis) + cos(lat_rad)*sin(ang_dis)*cos(bear_rad) )
    dest_long = long_rad + atan2(y =  sin(bear_rad)*sin(ang_dis)*cos(lat_rad),  x = cos(ang_dis) - sin(lat_rad)*sin(dest_lat) )
    # back to degrees
    dest_lat = dest_lat * 180/pi
    dest_long = dest_long * 180/pi
    return(cbind(lat_dest = dest_lat, long_dest = dest_long))
}

##### load and transform Dataset #####
ship = read_csv("ships.csv")
ship$SPEED = ship$SPEED * 0.514 # Convert m/s
ship$HEADING = if_else(ship$HEADING > 359, ship$HEADING - 360, ship$HEADING) # fix heading or bearing
ship$distance = hori_dist(speed = ship$SPEED, theta = ship$COURSE) # calculate distance
# calculate and append destination lat and long
latlon_dest = as_tibble( dest_latlong(lat_src = ship$LAT, long_src = ship$LON, bear = ship$HEADING, dist = ship$distance) )
ship = bind_cols(ship, latlon_dest)
# convert to lower for space
ship$DESTINATION = tolower(ship$DESTINATION)
ship$SHIPNAME = tolower(ship$SHIPNAME)
ship$FLAG = tolower(ship$FLAG)
ship$port = tolower(ship$port)
ship$ship_type = tolower(ship$ship_type)

# UI for application
ui <- fluidPage(

    # Application title
    titlePanel(span(tagList(icon("ship"), "Mari-time Route Analysis"))),
    
    fluidRow(
        column(3, selectInput("shiptype", 
                              label = "Select Ship Type", 
                              choices = unique(ship$ship_type)),
               offset = 2),
        column(4, uiOutput("shipname"),
               offset = 3)
        # 
    ),
    fluidRow(
        column(8, leafletOutput("routemap",width = "100%"), offset = 2)
    ),
    fluidRow(
        column(8, withTags({
            h3("Ship Route Table Info.")
        }), offset = 2)
    ),
    fluidRow(
        column(8, dataTableOutput("shiproutes"), offset = 2)
    )

)

# server logic
server <- function(input, output) {

    output$shipname <- renderUI({
        selectizeInput(inputId = "shipname", label = "Ship Name",
                       choices = ship %>% select(SHIPNAME) %>% 
                           filter(ship$ship_type == input$shiptype) %>% unique())
    })
    
    output$routemap <- renderLeaflet({
        
        ship %>%
            filter(ship_type == input$shiptype & SHIPNAME == input$shipname) %>%
            slice_max(distance) %>% slice_max(DATETIME) %>% 
            leaflet() %>%
            addTiles() %>%
            addMarkers(lat = ~ LAT, lng = ~ LON, 
                       popup = ~paste("<h4>Vessels Longest Distance Info. (Through Source)</h4>",
                                      "<b>Name :</b>", input$shipname,
                                      '</br>',
                                      "<b>ID :</b>", SHIP_ID,
                                      '</br>',
                                      "<b>LAT :</b>", LAT,
                                      '</br>',
                                      "<b>LONG :</b>", LON,
                                      '</br>',
                                      "<b>Status :</b>", if_else(is_parked == 0,"On move", "Parked"),
                                      '</br>',
                                      "<b>Datetime obs.:</b>", DATETIME,
                                      '</br>',
                                      "<b>PORT :</b>", port)) %>%
            addCircleMarkers(lat = ~lat_dest, lng = ~long_dest,
                             popup = ~paste("<h4>Vessels Longest Distance Info. (Towards Destination)</h4>",
                                            "<b>Name :</b>", input$shipname,
                                            '</br>',
                                            "<b>ID :</b>", SHIP_ID,
                                            '</br>',
                                            "<b>LAT :</b>", sprintf(lat_dest,fmt = "%#.5f"),
                                            '</br>',
                                            "<b>LONG :</b>", sprintf(long_dest,fmt = "%#.5f"),
                                            '</br>',
                                            "<b>Distance Covered :</b>", sprintf(distance,fmt = "%#.3f mts")))
    })
    
    output$shiproutes <- renderDataTable({
        
        DT::datatable(data = ship %>% 
                          filter(ship_type == input$shiptype, SHIPNAME == input$shipname) %>% 
                          select(DATETIME, LAT, LON, lat_dest, long_dest ,week_nb, port, is_parked, DESTINATION, distance),
                      options = list(pageLength = 5,
                                     lengthMenu = c(5, 10, 15, 20), 
                                     autoWidth = TRUE, 
                                     scrollX = TRUE),
                      caption = paste(toupper(input$shipname)," routes"))
    })

}

# Run the application 
shinyApp(ui = ui, server = server)