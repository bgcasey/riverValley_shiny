#------------------------------------------------------------------------

#                   UI, or "User Interface" Script

# this script designs the layout of everything the user will see in this Shiny App
#------------------------------------------------------------------------


library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(ggplot2)


sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
        menuItem("Explore pinch-ponts", icon = icon("th"), tabName = "explore",
                 badgeLabel = "new", badgeColor = "green"),
        menuItem("Cross-section", icon = icon("th"), tabName = "cross")
    )
)



# make dashboard header
header <- dashboardHeader(
    title ="Characterizing Connectivity Pinch-Points in Edmonton's Ribbon of Green",
    titleWidth = 800 # since we have a long title, we need to extend width element in pixels
)



# create dashboard body - this is the major UI element
body <- dashboardBody(
    # tabItem(tabName = "overview",
    #         h2("Widgets tab content")
    # ),
    # 
    # 
    # tabItem(tabName = "explore",
    # h2("Explore Pinch-Points"),

# make first row of elements (actually, this will be the only row)
fluidRow(
    #useShinyalert(),
    
    # make first column, 25% of page - width = 3 of 12 columns
    column(width = 3,
           
           
           # Box 1: text explaining what this app is
           #-----------------------------------------------
           box( width = NULL,
                status="primary", # this line can change the automatic color of the box. options are "info", "primary","warning","danger', and "success"
                title = NULL,
                
                # background = "black",
                
                
                
                # add some text in bold
                strong("River Valley Connectivity Project"  ,
                # linebreak
                
                br(),       
                       
                       a("A sustainability scholars project", href="https://www.ualberta.ca/sustainability/experiential/sustainability-scholars/index.html", target = "_blank"),
                       ),
                
                # linebreak
                br(),
                br(),
                
                # text in normal
                p("Use this application to visualize the conditions of
                  movement pinch-points in Edmonton's Ribbon of Green."),
                p("Created by Brendan Casey",  
                  br(),
                  br(),
                
                  strong(a("See application code", href="https://github.com/bgcasey/riverValley_shiny",  target = "_blank")),
                br(),
                strong(a("See project workflow", href="https://bookdown.org/bgcasey/RV_connectivity",  target = "_blank"))),
                # \
           ), # end box 1
           
           
    
           
           
           
           
           # box 2 : input for selecting pinch-points and variables
           #-----------------------------------------------
           box(width = NULL, status = "primary",
               title  = "Selection Criteria", solidHeader = T, 
               collapsible = T,
               
               # Widget specifying the seasonal pinch-points to be included on the plot
               # prettyRadioButtons(
               #     inputId = "season_select",
               #     label = "Season",
               #     choices = c("winter" , "summer"),
               #     selected = "summer",
               #     inline = TRUE,
               #     # checkIcon = list(
               #     #     yes = tags$i(class = "fa fa-check-square", 
               #     #                  style = "color: steelblue"),
               #     #     no = tags$i(class = "fa fa-square-o", 
               #     #                 style = "color: steelblue"))
               # ), # end radioButtons
               
               checkboxGroupButtons(
                   inputId = "season_select",
                   label = "Season",
                   choices = c("winter" , "summer"),
                   #selected = "summer",
                   checkIcon = list(
                       yes = tags$i(class = "fa fa-check-square",
                                    style = "color: steelblue"),
                       no = tags$i(class = "fa fa-square-o",
                                   style = "color: steelblue"))
               ), # end checkboxGroupButtons
               
               # widget for selecting minimum area of pinchponts
               sliderInput(
                           inputId = "pp_area_select", 
                           label = "Minimum pinch-point area (m2)",
                           min = 0, max = 350000,
                           value = c(0, 350000),
                           step=200
                   
                   ), #end slider

               # Widget specifying the Ribbon of Green reach
               
              selectInput(
                   inputId = "RoG_reach_select",
                   label = "RoG reach",
                   choices = c("Big Island Woodbend", "Big Lake", "Blackmud", "Cameron Oleskiw River Valley",
                               "Confluence", "East Ravines", "Edmonton East", "Horsehills North", "Horsehills South",
                               "Irvine Creek to Blackmud South", "Marquis River Valley", "Mill Creek North", "Mill Creek South",
                               "North Saskatchewan Central", "North Saskatchewan East", "North Saskatchewan West",
                               "SW Annex", "Wedgewood Ravine", "Whitemud", "Whitemud North", "Whitemud South Annex"),
                   selected = "North Saskatchewan Central"
                   #
               ), # end radioButtons
               # 
               
               
               # checkboxGroupButtons(
               #     inputId = "RoG_reach_select",
               #     label = "RoG reach",
               #     choices = c("Big Island Woodbend", "Big Lake", "Blackmud", "Cameron Oleskiw River Valley",
               #                 "Confluence", "East Ravines", "Edmonton East", "Horsehills North", "Horsehills South",
               #                 "Irvine Creek to Blackmud South", "Marquis River Valley", "Mill Creek North", "Mill Creek South",
               #                 "North Saskatchewan Central", "North Saskatchewan East", "North Saskatchewan West",
               #                 "SW Annex", "Wedgewood Ravine", "Whitemud", "Whitemud North", "Whitemud South Annex"),
               #     checkIcon = list(
               #         yes = tags$i(class = "fa fa-check-square",
               #                      style = "color: steelblue"),
               #         no = tags$i(class = "fa fa-square-o",
               #                     style = "color: steelblue"))
               # ), # end checkboxGroupButtons
               
               # Widget specifying the metrics to plot
               selectInput(
                   inputId = "metric_select",
                   label = "Variable of interest",
                   choices = c("Proportion near roads"="road_prop", "Proportion near trails"="trail_prop", "Vegetation type"="Vegetation_type" ,  "UPLVI PRIMECLAS"= "PRIMECLAS",  "UPLVI LANDCLAS"="LANDCLAS", "UPLVI STYPE"="STYPE"),
                   #choiceValues = c("road_prop", "trail_prop", "Vegetation_type", "PRIMECLAS", "LANDCLAS", "STYPE", "cell_use"),
                   selected = "STYPE",  
               ),# end prettyRadiochoices = 
              
     
              checkboxGroupButtons(
                  inputId = "cluster_select",
                  label = "HCPC Cluster",
                  choices = c(1, 2, 3,  4),
                  selected = c(1,2,3,4)
              ),
              
              # end prettyRadiochoices = 
              #  
               # strong("Space for your additional widget here:"),
               # 
               # br(), br(), br(), br(), br(), # add a bunch of line breaks to leave space. these can be removed when you add your widget
               # 
               # # space for your addition here:
               #-------------------------------------------
               # --- --- --- ---   HINT   --- --- --- --- 
               # here, you will paste code for another Widget to filter volcanoes on the map.
               # you'll need to paste code for some widget, name it, then call it at the top of the server page
               # when we are filtering the selected_volcanoes() reactive object. 
               
               
               # see the columns in the volcanoes dataset, and add a widget to further filter your selected_volcanoes() server object
               #  --- --- --- some suggestions: --- --- ---
               # 1. slider bar to only show volcanoes population_within_30_km > xxxx 
               # 2. slider input to show volcanoes with last_eruption_year > xxxx
               # 3. slider input to only show volcanoes with elevation > xxxx
               # 4. checkbox input to only show volcanoes in  evidence category c("xx", "xx")
               
               # see available widgets here: http://shinyapps.dreamrs.fr/shinyWidgets/
               # and here: https://shiny.rstudio.com/gallery/widget-gallery.html
               
               
           ), # end box 2
           
           
           # 
           # # # box 3: ggplot of selected volcanoes by continent
           # # #------------------------------------------------
           # box(width = NULL, status = "primary",
           #     solidHeader = TRUE, collapsible = T,
           #     title = "Biophysical charactaristics of pinch-points",
           #     plotOutput("continentplot", # this calls to object continentplot that is made in the server page
           #                height =250)
           # ) # end box 3
           # Box 2: Download all data
           #-----------------------------------------------
           # box( width = NULL,
           #      status="primary", # this line can change the automatic color of the box. options are "info", "primary","warning","danger', and "success"
           #      title = "Downloads",  solidHeader = T,
           #      collapsible = F,
           # 
           #      # Button
           #      downloadButton("downloadData", "Download data"),
           # 
           # 
           #      br(),
           # 
           #      br(),
           # 
           #      # Button
           #      downloadButton("downloadReport", "Download report")
           #     
           # ), # end box 2
           
           box( width = NULL,
                status="primary", # this line can change the automatic color of the box. options are "info", "primary","warning","danger', and "success"
                title = "Variable definitions",  solidHeader = T,
                collapsible = T,
                collapsed= T,

                # background = "black",

                # add some text in bold
                # p("Download pinch-point metrics as a .csv")  ,
                # linebreak

                # br(),
                # linebreak
                # br(),

                # Button
                div(style = 'overflow-y:scroll;height:295px;',
                tableOutput("var_tab")
                )

                # downloadButton("downloadWorkflow", "Download project report")
                # \
           ) # end box 2
    ), # end column 1
    
    

    # second column - 75% of page (8 of 12 columns)
    #--------------------------------------------------
    column(width = 9,
     tabsetPanel(
         # Overview tab
         tabPanel(value="tab1", title="Overview",


                  box( width = NULL,
                       status="primary", # this line can change the automatic color of the box. options are "info", "primary","warning","danger', and "success"
                       title = NULL,
                       # height="100%",
                       # tags$head(tags$style(
                       #     "body { word-wrap: break-word; }")),
                      # imageOutput("myImage"),
                       includeMarkdown("markdown/rv_con_es.md")
                       # background = "black",
                       
                       
                  ),
                 
                  # # div(style = "padding: 0px 0px; margin-bottom:150em",
                   box( width = NULL,
                       status="primary", # this line can change the automatic color of the box. options are "info", "primary","warning","danger', and "success"
                       title = NULL,
                       # height="100%",

                       imageOutput("myImage")
                       # background = "black",


                  )
             
                  
                  ),
           # Box 3: leaflet map
         tabPanel(value="tab2", title="Explore",
           box(width = NULL, background = "light-blue", 
               leafletOutput("pinchPoint_map", height = 650) 
               # this draws element called "pinchPoint_map", which is created in the "server" tab
           
                ), # end box with map
           
           
           #box 4
         
           box(width = NULL, status = "primary",
               solidHeader = TRUE, collapsible = T,
               title = "Biophysical charactaristics of pinch-points",
               plotOutput("multivariateplot",
                          height =350)
           )
           ), # end box 4
         
         tabPanel(value="tab3", title="Cross-section",
                  
                  box( width = NULL,
                       status="primary", # this line can change the automatic color of the box. options are "info", "primary","warning","danger', and "success"
                       title =NULL,
                       height="100%",
                       solidHeader = F,
                       collapsible = F,
                       collapsed= F,
                       includeMarkdown("markdown/profilePlot.md")
                  ),
         #          
         #          
         #          
                  box( width = NULL,
                       status="primary", # this line can change the automatic color of the box. options are "info", "primary","warning","danger', and "success"
                       title = strong("northern profile"),
                      height="100%",
                      solidHeader = T,
                      collapsible = T,
                      collapsed= T,
                      div(style = 'overflow-y:scroll;height:1200px;',
                       imageOutput("crossN"))
                  ),

                  box( width = NULL,
                       status="primary", # this line can change the automatic color of the box. options are "info", "primary","warning","danger', and "success"
                       title = strong("southern profile"),
                       height="100%",
                       solidHeader = T,
                       collapsible = T,
                       collapsed= T,
                       div(style = 'overflow-y:scroll;height:1200px;',
                           imageOutput("crossS"))
                  )

         )
         
         
         #tabPanel(value="tab3", title="Cross-section ")
     )# end tabsetpanel
    ) # end second column
    
), # end fluidrow



# fluidRow(
#     
#     # make first column, 25% of page - width = 3 of 12 columns
#     column(width = 12,
#            
#     
#            # # box 3: ggplot of selected volcanoes by continent
#            # #------------------------------------------------
#            box(width = NULL, status = "primary",
#                solidHeader = TRUE, collapsible = T,
#                title = "Biophysical charactaristics of pinch-points",
#                plotOutput("continentplot", # this calls to object continentplot that is made in the server page
#                           height =250)
#            ) # end box 3
#            
#     ), # end column 1
# 
#     
# ), # end fluidrow




# Make a CSS change so this app shows at 90% zoom on browsers
# only adding this because it looked more zoomed in on my web browser than it did on my RStudio viewer
tags$style(" body {
    -moz-transform: scale(0.9, 0.9); /* Moz-browsers */
    zoom: 0.9; /* Other non-webkit browsers */
    zoom: 90%; /* Webkit browsers */}"),

) # end body


# compile dashboard elements
dashboardPage(
    skin = "blue",
    header = header,
    sidebar = dashboardSidebar(disable = TRUE),
    body = body
)

