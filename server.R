#------------------------------------------------------------------------
#                       ---- Server Script ----
#  this script runs all internal operations to make your shiny app work
#------------------------------------------------------------------------


# Load packages
library(dplyr)
library(stringr)
library(countrycode)
library(here)
library(sf)
library(rgdal)
library(ggplot2)
library(rgeos)
library(htmlwidgets)
#library(shinyalert)
# library(write.table)
# library(utils)


function(input, output, session) {
  
  # Import and clean data  ----------------------------------------
    
    pp_all_clus_RoG_wgs_clus<-readRDS("data/pp_all_clus_RoG_wgs_clus.rds")
    RoG_Reaches_wgs<-readRDS("data/RoG_Reaches_wgs.rds")
    pp_ext_metrics_all_df<-readRDS("data/pp_ext_metrics_all_df.rds")
    pp_all_Met_wide<-readRDS("data/pp_all_Met_wide.rds")
    var_des<-read.csv("data/var_des.csv")
    
#------------ end data cleaning -----------------
  
  # Create Reactive Subset ------------------------------------------------ 
  # Make a subset of the data as a reactive value
  
  ### Reaches #####  
  # this subset pulls reach rows only in the selected types of reaches from the UI checkbox
  selected_reaches <- reactive({
    
    # start with full dataset
    RoG_Reaches_wgs %>% 
      
      # select only volcanoes in the selected volcano type (from checkboxes in the UI)
      filter(Reach %in% input$RoG_reach_select) %>%
      
      # change reach into factor (this makes plotting it more consistent re order of colors)
      mutate(Reach = factor(Reach, levels =  unique(RoG_Reaches_wgs$Reach) ) )
  })
    
 ### Pinch-points polygons ####
  selected_pp_reach <- reactive({
      
        # start with full dataset
    selected_pp_reach <-pp_all_clus_RoG_wgs_clus%>% 
        
        # # select only pinch-points in the selected season
        filter(season %in% input$season_select) %>%
        
        # select only pinch-points in the selected Reach (from checkboxes in the UI)
        filter(Reach %in% input$RoG_reach_select)  %>%
      
        #select pinch-ponts that are within the area range
      filter(between(as.numeric(area), input$pp_area_select[1], input$pp_area_select[2])) %>%
      filter(hcpc_clust %in% input$cluster_select)
    # if(nrow(selected_pp_reach)==0){
    #   shinyalert("Oops!", "No data returned", type = "error")
    #   selected_pp_reach <- NULL
    # }
    #  
    # selected_pp_reach
    })
    
    
### Metrics ###
selected_metrics<-reactive({
     pp_ext_metrics_all_df%>%
    filter(season %in% input$season_select) %>% 
    filter(between(as.numeric(pp_area),input$pp_area_select[1], input$pp_area_select[2]))%>% 
    filter(Reach %in% input$RoG_reach_select) %>%
    filter(metric %in% input$metric_select) %>%
    filter(hcpc_clust %in% input$cluster_select)
})








#------------------------------------------------------------
#Output elements
#------------------------------------------------------------


# # Downloadable csv of selected dataset ----
# output$downloadData <- downloadHandler(
#   filename = "pp_all_metrics.csv",
#   content = function(file) {
#     write.csv(pp_all_Met_wide, file, row.names = FALSE)
#   }
# )

# Header image
output$myImage <- renderImage({
  filename<-"images/RoG_photo2.png"

  # Generate the PNG
  # Return a list containing the filename and alt text

  list(src = filename,
       width="100%",
       autosize=TRUE,
       alt = paste("Image number", input$n))
}, deleteFile = FALSE)


# #cross image N
output$crossN <- renderImage({
  filename<-"images/n_cross_section_map.png"

  # Generate the PNG
  # Return a list containing the filename and alt text
  list(src = filename,
       width="100%",
       alt = paste("Image number", input$n))

}, deleteFile = FALSE)
# 
# #cross image S
output$crossS <- renderImage({
  filename<-"images/s_cross_section_map.png"

  # Generate the PNG
  # Return a list containing the filename and alt text
  list(src = filename,
       width="100%",
       alt = paste("Image number", input$n))

}, deleteFile = FALSE)


#variable table
output$var_tab<-renderTable({var_des})
  
  
  # make output element for contents barplot 
  #------------------------------------------------------------
  output$multivariateplot <- renderPlot({
    
    #don't try to render if nrow is filtered to zero
    req(nrow(selected_metrics())>0, cancelOutput = TRUE)
    
    # create basic barplot
    ggplot(data=selected_metrics(),
                     aes(x=as.factor(cluster),
                         y= proportion,
                         fill=class
                         ))+
      geom_bar(stat="identity") +
      labs(x="pinch-point", y="% of pinch-pont")+
      theme(panel.spacing = unit(1, "lines"),
            panel.background = element_blank(),
            axis.ticks=element_blank())+
      scale_y_continuous(labels=scales::percent, position = "left")

  }) # end renderplot command


  #------------------------------------------------------------
  # make output element for pinch-point map
  #------------------------------------------------------------
  pal <- colorFactor(palette = c("red", "blue"),
                     domain = c("winter", "summer"))

  output$pinchPoint_map <- renderLeaflet({
    # add blank leaflet map
    leaflet(options = leafletOptions(minZoom = 2, maxZoom = 15, zoomControl = TRUE)) %>%
      # add map tiles from CartoDB.
      addProviderTiles("CartoDB.VoyagerNoLabels") %>%
      # set lat long and zoom to start
      setView(lng = -113.4938, lat = 53.5461, zoom = 11)
  })

#add pinchpoint polygons
observeEvent(selected_pp_reach(),{
  req(nrow(selected_pp_reach())>0, cancelOutput = TRUE)
  # showNotification("This is a notification.")%>%
  leafletProxy("pinchPoint_map") %>%
    #clearShapes()%>%
    # clearGroup('season')%>%
    clearGroup('pp_reach')%>%
    #removeShape(selected_pp_season)%>%   # clear points from last selected options
    addPolygons(        # add new points from "selected_volcanoes()" reactive object
      data = selected_pp_reach(),
      color = ~pal(season),
      #fill = season,
      stroke = TRUE,
      fillOpacity = 0.3,
      opacity = 1,
      weight = .8,
      group='pp_reach',
      highlight = highlightOptions(
        weight = 3,
        fillOpacity = 0.4,
        color = "yellow",
        opacity = 1.0,
        bringToFront = TRUE))  %>%
    addLabelOnlyMarkers(data = selected_pp_reach(),label= ~cluster,
                      lng = ~x, lat = ~y,
                      labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE),
                      group="pp_reach")
}) # end observe

# add reach polygons
observeEvent(selected_reaches(),{
  req(nrow(selected_reaches())>0, cancelOutput = TRUE)
  proxy<-leafletProxy("pinchPoint_map") %>%
    clearGroup('reach')%>%
    # clearShapes() %>%   # clear points from last selected options
    addPolygons(        # add new points from "selected_volcanoes()" reactive object
      data = selected_reaches(),
      color = "black",
      stroke = TRUE,
      fillOpacity = 0.0,
      opacity = 1,
      weight = .7,
      group= 'reach')%>%
  setView(
        lng = selected_reaches()$x,
        lat = selected_reaches()$y,
        zoom = 13)
}) # end observe

  
} # end the server page



