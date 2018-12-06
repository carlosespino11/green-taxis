library(shiny)
library(tidyverse)
library(leaflet)
library(RColorBrewer)
library(lubridate)
library(dplyr)
library(sf)
library(maptools)
library(ggplot2)
library(ggthemes)
library(scales)
library(geosphere)
library(forcats)
library(glue)

## Load data and shapefiles
trips_df = read_rds('app-data/trips_df_app.rds')
ny_shapes_lonlat = read_rds("app-data/ny_shapes_longlat.rds") 
ny_borough_geo =read_rds('app-data/ny_boroughs_longlat.rds')

# Define palettes
borough_options = c('Manhattan', 'Brooklyn', 'Queens', 'Bronx', 'Staten Island')
borough_pal = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00")
route_pal = c('#FAACFF', "#99FFCC")


ui <- bootstrapPage(
  tags$head(
    includeCSS("styles.css")
  ),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  # Map output
  leafletOutput("map", width = "100%", height = "100%"),
  # Controls panel
  absolutePanel(top = 130, left = 10, id = "controls", class = "panel panel-default", 
                fixed = TRUE, draggable = TRUE,
                sliderInput("hour", "Pick an hour range", 
                            min=0, max=23, value=c(0,23)),
                sliderInput("day", "Pick a day of week range. Mon: 1 - Sun:7", 
                            min=1, max=7, value=c(1,7)),
                selectInput("borough", "Pick a borough.",
                            choices = c('All',borough_options), 
                            selected = 1),
                h5("Intra vs Inter borough traffic:"),
                plotOutput("boroughplot", height = 250)
                

  ),
  # Title and subtitle
  absolutePanel(top = 10, left = 35,headerPanel("NYC Taxi Trips Network"),  h4('Green Taxis. September 2015.')),
  # Annotations
  absolutePanel(bottom = 10, left = 35,h6('Trips aggregated by NTA neighborhoods. Edge weight by number of trips. Showing only top 5% edges.'))
  
)

server <- function(input, output, session) {
  # Reactive expression to subset data
  filteredData <- reactive({
    
    selected_hours = as.numeric(input$hour)
    selected_days = as.numeric(input$day)
    
    hours = selected_hours[1]:selected_hours[2]
    days = selected_days[1]:selected_days[2]
    
    # Filter data by hours and days selected
    filtered = trips_df %>% 
      dplyr::filter(trip_hour %in% hours, trip_weekday %in% days) 

    filtered

  })
  
  # Render base map
  output$map <- renderLeaflet({
    # Bounding box to zoom it at the beginning
    bbox_ny = ny_shapes_lonlat%>% sf::st_bbox() %>% unlist %>% unname
    
    # Aspects of the map that  won't need to change dynamically
    leaflet() %>%
      addProviderTiles("CartoDB.DarkMatter")%>%
      fitBounds(bbox_ny[1],bbox_ny[2],bbox_ny[3],bbox_ny[4])

  })
  
  # Changes in the map and graphics
  observe({
    # Get filtered data and selected borough
    sel_borough = input$borough
    filtered = filteredData()
    
    # If a specific borough is selected, apply the correspondonding filter to the data
    if(sel_borough %in% borough_options){
      bor_filt = filtered %>%
        filter( (pickup_BoroName == sel_borough) | (dropoff_BoroName == sel_borough))
    } else {
      bor_filt = filtered
    }
    
    # Build the edges of the network.
    route_network = bor_filt %>% 
      mutate(is_inter = ifelse(pickup_BoroName == dropoff_BoroName, 'intra-borough', 'inter-borough'))%>%
      group_by(pickup_NTACode, dropoff_NTACode) %>%
      summarize(n=n(), is_inter = first( is_inter)) %>%
      ungroup() %>%
      rename(src = pickup_NTACode,  dest = dropoff_NTACode) %>%
      mutate(w = rescale(n,to=c(.2, 2))) %>% 
      dplyr::filter(w > quantile(w,.95)) %>% 
      left_join(ny_shapes_lonlat, by = c('dest'= 'NTACode' )) %>%
      left_join(ny_shapes_lonlat, by = c('src'= 'NTACode' )) %>%
      dplyr::filter(dest!=src) %>%
      na.omit() %>% 
      mutate(id = 1:n()) 
    
    # Convert the edge information into geo-referenced Spatial Lines 
    lines_list = route_network %>%
      transpose() %>%
      map( function(row){
        Lines(
          Line(cbind(as.numeric(c(row["centroid_long.x"], row["centroid_long.y"])), 
                     as.numeric(c(row["centroid_lat.x"], row["centroid_lat.y"]))
            )
          ),
          ID= row['id']
        )
      })
    
    route_lines = lines_list %>% SpatialLines() %>% 
      SpatialLinesDataFrame(route_network)#%>%

    # Labels to show on map hover
    labels = glue::glue_data(ny_shapes_lonlat, "{NTAName}")
    
    # Palette functions
    pal_route_fn = colorFactor(route_pal, domain = route_network$is_inter)
    pal_bor_fn = colorFactor(borough_pal, domain = ny_borough_geo$boro_name)
    
    # Render lines and boroughs
    leafletProxy("map", data=route_lines) %>%
      clearShapes() %>%
      clearControls() %>%      
      addPolygons(data=ny_borough_geo, weight = .8, color = pal_bor_fn(ny_borough_geo$boro_name),
                                           fillOpacity =  0.0001)%>%
      addPolygons(data = ny_shapes_lonlat, fill = 'black', weight = 0.1, color = 'white',
                  fillOpacity = 0.001,
                  highlightOptions = highlightOptions(color = "white", weight = 1, bringToFront = TRUE),
                  label = labels) %>%
      addLegend("bottomright", pal = pal_bor_fn, values = ny_borough_geo$boro_name, title ='')%>%
      addPolylines(color = pal_route_fn(route_lines$is_inter), weight =route_lines$w) %>%
      addLegend("bottomright", pal = pal_route_fn, values = ~is_inter, title ='')
    
    
    # Render inter/intra-borough plot
    output$boroughplot = renderPlot({
      if(!sel_borough %in% borough_options){
        traffic_data = filtered %>% 
          mutate(traffic = ifelse(pickup_BoroName == dropoff_BoroName, 'intra-borough\ntraffic', 
                                  'inter-borough\ntraffic')) %>% 
          group_by(traffic) %>%
          summarize(n = n())
        
        pl = ggplot(traffic_data) + 
          geom_bar(aes(x = traffic, y = n, fill=traffic), stat='identity') +
          scale_fill_manual(values = c('#FAACFF', "#99FFCC"))+
          labs(title = 'Overall')+
          theme_fivethirtyeight()+
          theme(
            legend.position="none",
            panel.background = element_rect(fill = "black"),
            plot.background = element_rect(fill = "black"),
            axis.title = element_text(colour = "white"),
            axis.text = element_text(colour = "white"),
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(colour = "white"),
            legend.background= element_rect(fill = "black"),
            legend.box.background = element_rect(fill = "black"),
            legend.text = element_text(colour = "white"),
            legend.title = element_text(colour = "white"),
            plot.title =  element_text(colour = "white")
          )
      }else{
        # Compute number of trips coming from borough
        from_data = filtered %>% 
          filter(pickup_BoroName == sel_borough ) %>%
          group_by(dropoff_BoroName) %>% 
          summarize(n = n()) %>% 
          rename(borough = dropoff_BoroName) %>% 
          filter(borough != sel_borough) %>%
          mutate(direction = 'from')
        
        # Compute trips coming to borough
        to_data = filtered %>% 
          filter(dropoff_BoroName == sel_borough ) %>%
          group_by(pickup_BoroName) %>% 
          summarize(n = n())%>% 
          rename(borough = pickup_BoroName) %>% 
          filter(borough != sel_borough) %>%
          mutate(direction = 'to')
        
        # Compute trips intra-borough
        intra_data = filtered %>%
          filter(dropoff_BoroName == sel_borough, pickup_BoroName == sel_borough) %>% 
          summarize(n = n()) %>%
          mutate(direction = 'inter', borough = sel_borough)
        
        
        # Bind the information computed
        boro_trips_data = bind_rows(from_data, to_data,intra_data)  
        pl = ggplot(boro_trips_data) + 
          geom_bar(aes(x = direction, y = n, fill = borough), stat='identity') +
          scale_fill_manual(values = borough_pal) +
          labs(title = sel_borough)+
          theme_fivethirtyeight()+
          scale_x_discrete(labels= c(glue('traffic from\n other borough\nto {sel_borough}'), 
                                     glue('{sel_borough}\nintra-borough\ntraffic'), 
                                     glue('traffic from\n{sel_borough}\nto other borough')))+
          theme(
            legend.position="none",
            legend.box = "vertical",
            panel.background = element_rect(fill = "black"),
            plot.background = element_rect(fill = "black"),
            axis.title = element_text(colour = "white"),
            axis.text = element_text(colour = "white"),
            strip.background = element_rect(fill = "black"),
            strip.text = element_text(colour = "white"),
            legend.background= element_rect(fill = "black"),
            legend.box.background = element_rect(fill = "black"),
            legend.text = element_text(colour = "white"),
            legend.title = element_text(colour = "white"),
            plot.title =  element_text(colour = "white")
          )
      }
      pl 
    })
    
  })
}

shinyApp(ui, server)
