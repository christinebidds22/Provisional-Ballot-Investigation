library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(leaflet)
library(rgdal)
library(htmltools)

counties <-  readOGR("ads_counties.shp", layer = "ads_counties", GDAL1_integer64_policy = TRUE)


ui <- fluidPage(theme = shinytheme("flatly"),
   titlePanel("elections by the numbers in our sample jurisdictions"),
   sidebarLayout(
      sidebarPanel(
         p("here we visualize the election figures that drove our inquiry into provisional ballot rejection. Loop
           the slider below, with a focus on New York City, to explore our target variable: the provisional ballot rejection rate."),
         tags$head(
            tags$style(type = "text/css", ".jslider { max-width: 250px; }"),
            tags$style(type = "text/css", ".well { max-width: 450px; }"),
            tags$style(type = "text/css", ".span4 { max-width:450px; }"),
            tags$style(type = "text/css", ".well { background-color: #ffffff; }")
         ),
         br(),
         sliderInput("year", "election year", min = 2012, max = 2016,
                     value = 2012, step = 2, sep = "",
                     animate = animationOptions(loop = TRUE, interval = 3500,
                                                playButton = tags$button("loop", style = "background-color: #3c5c71; color:white; margin-top: 10px; border:solid"),
                                                pauseButton = tags$button("pause", style = "background-color: #3c5c71; color:white; margin-top: 10px; border:solid"))
                     ),
         wellPanel(
            radioButtons("measures",
                         label = h4("explore:"),
                         choices = list("provisional ballot rejection rate (our linear target variable)" = "prop_rejected",
                                        "lowest rejection rate in 2012 (our logit binary target variable)" = "low_reject_prop",
                                        "total number of eligible voters" = "total_eligible_voters",
                                        "total poll workers" = "total_poll_workers",
                                        "pollworkers per voter" = "poll_pp",
                                        "total provisional ballots submitted" = "total_provisional_submitted",
                                        "total provisional ballots rejected" = "total_provisional_rejected",
                                        "any instate provisional executive order" = "exec_order",
                                        "state has in-person early voting (binary)" = "early_voting",
                                        "state has no-excuse absentee balloting (binary)" = "no_excuse_absentee"),
                         selected = "prop_rejected")
         )
   ),

      mainPanel(width = 8,
         leafletOutput("map", width = "100%", height = "600px"))
      )
   )

server <- function(input, output) {

   selected <- reactive({
      counties[counties@data$variable == input$measures  & counties@data$year == input$year, ]
   })

   output$map <-renderLeaflet({

      pal <- reactive({
         colorNumeric(palette = c("#d0d656", "#aedea4", "#73a8a0", "#3c5c71", "#3a2f5a"), domain = selected()$value)
      })

      leaflet(data = selected()) %>%
         setView(lng = -78.430703, lat = 40.824716, zoom = 7) %>%
         addProviderTiles(provider = "CartoDB.Positron") %>%
         addPolygons(
            fillColor = ~pal()(value),
            weight = 1,
            opacity = 0.7,
            color = "navy",
            fillOpacity = 0.8,
            label = paste0("year: ", selected()$year, br(), "county: ", selected()$NAME, br(),
                           sprintf("%s: %g", selected()$variable, selected()$value)) %>%
               lapply(htmltools:: HTML)) %>%
         addLegend("bottomleft", pal = pal(), values = selected()$value,
                   opacity = 1
         )
   })

}

shinyApp(ui = ui, server = server)
