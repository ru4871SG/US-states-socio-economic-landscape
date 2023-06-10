library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(rgdal)
library(geojsonio)
library(janitor)

#####Import Data
# Source ----
source("helpers.R")

geo_data <- geojson_read("https://raw.githubusercontent.com/ru4871SG/US-states-geojson/main/stateswithoutpr.geo.json", what = "sp")

# Ensure that the GeoFips column is character
# This is to make sure that the merge operation later won't fail due to different data types
part2_map1$GeoFips <- as.character(part2_map1$GeoFips)

# ui
ui <- navbarPage(
  title="Socio-Economic Landscape",
  #first page
  tabPanel("Page 1",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 helpText("Scatterplots to see the correlation between income per capita vs. college completion and employment percentage. Choose the y-value to change the plot."),
                 selectInput(
                   #we use input$plot1_input in the server side
                   "plot1_input", h3("Select the y-value"),
                   choices = list(
                     "College Completion Percentage" = "collegecompletion_2017_2021", 
                     "Employment / Population" = "employment_percentage_2021"), 
                   selected = "collegecompletion_2017_2021")
               ),
               mainPanel(
                 plotlyOutput("plot1")
               )
             )
           )
  ),
  #second page
  tabPanel("Page 2",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 helpText("Interactive map and vertical bar graph to see income per capita level by state."),
                 sliderInput(inputId = "selected_year", 
                             label = "Select the Year:", 
                             min = 1970, 
                             max = 2022, 
                             value = 2022, 
                             step = 1,
                             #we need the sep otherwise we will see comma like 1,997 instead of 1997
                             sep = ""),
                 selectInput(inputId = "selected_geoName", 
                             label = "Filter by State:", 
                             #this part is important so you start with all the states, but users can select an individual state
                             choices = c("All", unique(part2_map1$GeoName)),
                             selected = "All",
                             multiple = FALSE)
               ),
               mainPanel(
                 leafletOutput(outputId = "map"),
                 conditionalPanel(
                   condition = 'input.selected_geoName == "All"',
                   plotlyOutput(outputId = "plot2", height = 750)
                 ),
                 conditionalPanel(
                   condition = 'input.selected_geoName != "All"',
                   tags$div(style = "display:inline-block; width:25px;"),
                   plotlyOutput(outputId = "plot3", height = 300)
                 )
               )
               
             )
           )
  ),
  #third page
  tabPanel("Page 3",
           fluidPage(
             mainPanel(
               fluidRow(
                 helpText("Two line graphs to see the growth of income per capita and education level over time."),
                 plotlyOutput("plot4")
               )
             )
           )
  )
)

# server
server <- function(input,output){
  
  output$plot1 <- renderPlotly({
    
    hover_label <- reactive({
      if (input$plot1_input == "collegecompletion_2017_2021") {
        return("College Completion")
      } else if (input$plot1_input == "employment_percentage_2021") {
        return("Employment / Population")
      } else {
        return(input$plot1_input)
      }
    })
    
    color_scale <- grDevices::colorRampPalette(viridis::viridis(100))
    
    part1_merged$color <- color_scale(100)[cut(part1_merged$incomepercapita_2021, breaks = 100)]
    
    plot_ly(data = part1_merged,
            x = ~incomepercapita_2021,
            y = ~part1_merged[[input$plot1_input]],
            text = ~paste("<b>State:</b>", GeoName,
                          "<br><b> Income per Capita:</b>", sprintf("$%s", formatC(incomepercapita_2021, format = "d", big.mark = ",")),
                          "<br><b>", hover_label(), ":</b>", sprintf("%.2f%%", 100 * part1_merged[[input$plot1_input]])),
            type = "scatter",
            mode = "markers",
            marker = list(size = 10, color = ~color),
            hoverinfo = "text") %>%
      layout(title = "Income Per Capita vs. Education and Employment",
             showlegend = FALSE,
             xaxis = list(title = "Income per Capita"),
             yaxis = list(title = hover_label(), tickformat = ".0%")) %>%
      #we need to add the lm line here due to plot_ly() difference with ggplot
      add_trace(x = ~incomepercapita_2021, y = ~lm(part1_merged[[input$plot1_input]] ~ incomepercapita_2021, data = part1_merged)$fitted.values, name = 'lm', type = 'scatter', mode = 'lines', line = list(color = "red", dash = "solid"), inherit=FALSE) %>%
      # add correlation coefficient as annotation
      add_annotations(x = 0.97, y = 0.1, text = paste0("Pearson's correlation coefficient: ", round(cor(part1_merged$incomepercapita_2021, part1_merged[[input$plot1_input]]), 4)), showarrow = FALSE, xref = "paper", yref = "paper")
  })
  
  data_for_year <- reactive({
    # If user has selected "All", we show all states
    if(input$selected_geoName == "All") {
      part2_map1 %>%
        filter(year == input$selected_year)
    }
    # Otherwise, show only the unique and selected state
    else {
      part2_map1 %>%
        filter(year == input$selected_year & 
                 GeoName == input$selected_geoName)
    }
  })
  
  
  #interactive map
  output$map <- renderLeaflet({
    
    # Merge geo_data and data_for_year
    merged_data <- sp::merge(geo_data, data_for_year(), by.x="fips", by.y="GeoFips")
    
    # Check if user selected a single state or "All"
    if (input$selected_geoName != "All") {
      # User selected a single state, use colorNumeric. IMPORTANT! otherwise it will return error
      pal <- colorNumeric("Greens", NULL)
    } else {
      # User selected "All", use colorQuantile
      pal <- colorQuantile("Greens", NULL, n = 5)
    }
    
    #mytext is for the hovertemplate data, which we will call under the label later
    mytext <- paste(
      "<b>State:</b> ", merged_data@data$GeoName, "<br />",
      "<b>Income Per Capita:</b> ", sprintf("$%s", format(merged_data@data$income_per_capita, big.mark = ",")), "<br/>") %>%
      lapply(htmltools::HTML)
    
    # Add a title to the first map
    titlemap1 <- tags$div(
      tags$h2(
        HTML("Income Per Capita Map"),
        style = "font-size: 16px; font-weight: bold;"
      )
    )
    
    # Create the leaflet map
    map <- leaflet(merged_data)
    
    # if-else for the setView, because some states are too small with default setView
    if (input$selected_geoName == "District of Columbia") {
      map <- map %>% setView(lng = -77, lat = 39, zoom = 7)
    } else if (input$selected_geoName == "Alaska") {
      map <- map %>% setView(lng = -150, lat = 60, zoom = 3)
    } else if (input$selected_geoName == "Delaware") {
      map <- map %>% setView(lng = -77, lat = 39, zoom = 7)
    } else if (input$selected_geoName == "Hawaii") {
      map <- map %>% setView(lng = -156, lat = 19, zoom = 5)
    } else if (input$selected_geoName == "Rhode Island") {
      map <- map %>% setView(lng = -72, lat = 41, zoom = 6)
    } else {
      map <- map %>% setView(lng = -110, lat = 40, zoom = 3)
    }
    
    map <- map %>%
      addPolygons(
        fillColor = ~pal(income_per_capita), 
        fillOpacity = 0.7, 
        color = "#BDBDC3", 
        weight = 1, 
        label = ~mytext,
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE)
      ) %>%
      addControl(
        titlemap1,
        position = "topright"
      )
    
    # if-else statement for the Legend based on input$selected_geoName
    if (input$selected_geoName == "All") {
      map <- map %>%
        addLegend(
          pal = pal,
          values = ~ merged_data@data$income_per_capita,
          opacity = 0.8,
          position = "bottomright",
          title = "Income Per Capita Level %"
        )
    }
    
    # Return the map
    map
  })
  
  #vertical bar graph
  output$plot2 <- renderPlotly({
    
    # Define green color theme
    green_theme <- theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#f4f0f0"),
        panel.background = element_rect(fill = "#c4e2c0"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white")
      )
    
    # Create bar plot
    ggplot1 <- ggplot(data_for_year(), 
                      aes(x = reorder(GeoName, income_per_capita), 
                          y = income_per_capita,
                          text = paste(GeoName, ":", sprintf("$%s", formatC(income_per_capita, format = "d", big.mark = ",")))
                          )) +
      geom_bar(stat = "identity", fill = "#428f61") +
      green_theme +
      coord_flip() +
      labs(title = "Income Per Capita Over Time", x = "", y = "")
    
    ggplotly(ggplot1, tooltip = "text") %>%
      layout(hovermode = 'closest') %>% # Hover closest to cursor
      layout(
        hoverlabel = list(bgcolor = "white"
                          ))
  })
  
  #line graph for Page 2
  output$plot3 <- renderPlotly({
    
    # Filter data for selected state
    state_data <- part2_map1 %>%
      filter(GeoName == input$selected_geoName)
    
    # Create line graph
    plot_ly(data = state_data,
            type = "scatter", 
            mode = "lines",
            x = ~year, 
            y = ~income_per_capita, 
            text = ~GeoName,
            hovertemplate = paste(
              "<b>%{text}<br></b>",
              "Income Per Capita: %{y:$,.0f}<br>",
              "Year: %{x:.0f}",
              "<extra></extra>"
            ),
            line = list(color = '#35B779FF')) %>% 
      layout(title = paste("Income Per Capita Over Time for", input$selected_geoName),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Income Per Capita"),
             dragmode = FALSE)
  })
  

  # Define a color palette with 8 colors from Viridis
  color_palette <- c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF", "#1F9D89FF", "#35B779FF", "#6DCD59FF")
  
  output$plot4 <- renderPlotly({
    
    #this is needed if we want to use the Viridis theme from color_palette above
    unique_geo_names <- unique(part4_incomeline$GeoName)
    color_map <- setNames(rep(color_palette, length.out = length(unique_geo_names)), unique_geo_names)
    
    plot_ly(data = part4_incomeline,
            type = "scatter", 
            mode = "lines",
            x = ~year, 
            y = ~income_per_capita, 
            color = ~GeoName, 
            colors = color_map,
            text = ~GeoName,
            hovertemplate = paste(
              "<b>%{text}<br></b>",
              "Income Per Capita: %{y:$,.0f}<br>",
              "Year: %{x:.0f}",
              "<extra></extra>"
            )
            ) %>% 
      layout(title = "Income Per Capita Over Time",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Income Per Capita"),
             dragmode = FALSE)
  })
  
  
  
}

shinyApp(ui,server)
