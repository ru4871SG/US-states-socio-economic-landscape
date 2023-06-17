library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(leaflet)
library(rgdal)
library(geojsonio)
library(janitor)
library(highcharter)
library(shinycssloaders)

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
                 helpText("Interactive map and graph to see income per capita level by state."),
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
                             multiple = FALSE,
                             selectize = FALSE
                 )
               ),
               mainPanel(
                 leafletOutput(outputId = "map"),
                 conditionalPanel(
                   condition = 'input.selected_geoName == "All"',
                   highchartOutput(outputId = "plot2", height = 850)
                 ),
                 conditionalPanel(
                   condition = 'input.selected_geoName != "All"',
                   tags$div(style = "display:inline-block; width:25px;"),
                   withSpinner(highchartOutput(outputId = "plot3"))
                 )
               )
               
             )
           )
  ),
  #second page
  tabPanel("Page 2",
           fluidPage(
             sidebarLayout(
               sidebarPanel(
                 helpText("Highchart scatterplots to see the correlation between income per capita vs. college completion and employment percentage. Choose the y-value to change the plot."),
                 selectInput(
                   #we use input$plot1_input in the server side
                   "plot1_input", h3("Select the y-value"),
                   choices = list(
                     "College Completion Percentage" = "collegecompletion_2017_2021", 
                     "Employment / Population" = "employment_percentage_2021"), 
                   selected = "collegecompletion_2017_2021",
                   selectize = FALSE)
               ),
               mainPanel(
                 withSpinner(highchartOutput("plot1"))
               )
             )
           )
  ),
  #third page
  tabPanel("Page 3",
           fluidPage(
             mainPanel(
               fluidRow(
                 helpText("Highchart line graph to see the growth of income per capita over time for the entire country."),
                 withSpinner(highchartOutput("plot4", height=580))
               )
             )
           )
  )
)

# server
server <- function(input,output){
  #this plot1 is for page 2, while plot2 below is for page 1.
  selected_data <- reactive({
    hover_label <- if (input$plot1_input == "collegecompletion_2017_2021") {
      "College Completion"
    } else if (input$plot1_input == "employment_percentage_2021") {
      "Employment / Population"
    } else {
      input$plot1_input
    }
    
    # Linear Model, needed to make it here for the highchart plot
    lm_model <- lm(part1_merged[[input$plot1_input]] ~ incomepercapita_2021, data = part1_merged)
    x_range <- range(part1_merged$incomepercapita_2021)
    x_values <- seq(from = x_range[1], to = x_range[2], length.out = 100)
    y_values <- predict(lm_model, newdata = data.frame(incomepercapita_2021 = x_values))
    lm_line <- data.frame(x_values, y_values)
    
    
    color_scale <- grDevices::colorRampPalette(viridis::viridis(100))
    
    part1_merged$color <- color_scale(100)[cut(part1_merged$incomepercapita_2021, breaks = 100)]
    
    hchart(part1_merged, "scatter", hcaes(x = incomepercapita_2021, y = .data[[input$plot1_input]], color = color), name = hover_label) %>%
      hc_title(text = "Income Per Capita vs. Education and Employment") %>%
      hc_xAxis(title = list(text = "Income per Capita")) %>%
      hc_yAxis(title = list(text = hover_label)) %>%
      hc_colorAxis() %>%
      hc_tooltip(formatter = JS("function() {
          if (this.series.name == 'Linear Regression') {
              return '<b>X: </b>' + new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(this.x) +
        '<br><b>Y: </b>' + (this.y * 100).toFixed(2) + '%';
          } else {
              return '<b>State: </b>' + this.point.GeoName +
        '<br><b>Income per Capita: </b>' + new Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(this.point.incomepercapita_2021) +
        '<br><b>' + this.series.name + ': </b>' + (this.point.y * 100).toFixed(2) + '%';
          }}")) %>%
      hc_add_series(data = lm_line, type = "line", name = "Linear Regression", hcaes(x = x_values, y = y_values), color = "red")
  })
  
  output$plot1 <- renderHighchart({
    selected_data()
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
    
    # this will create different hovertext depends on user input on selected_geoName
    if (input$selected_geoName == "All") {
      mytext <- paste(
        "<b>State:</b> ", merged_data@data$GeoName, "<br />",
        "<b>Income Per Capita:</b> ", sprintf("$%s", format(merged_data@data$income_per_capita, big.mark = ",")), "<br/>")
    } else {
      mytext <- ifelse(merged_data@data$GeoName == input$selected_geoName,
                       paste("<b>State:</b> ", merged_data@data$GeoName, "<br />",
                             "<b>Income Per Capita:</b> ", sprintf("$%s", format(merged_data@data$income_per_capita, big.mark = ",")), "<br/>"), 
                       "")
    }
    
    mytext <- lapply(mytext, htmltools::HTML)
    
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
      map <- map %>% setView(lng = -95, lat = 40, zoom = 3)
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
  
  #vertical bar graph for Page 1
  output$plot2 <- renderHighchart({
    
    data_ordered <- data_for_year() %>%
      arrange(desc(income_per_capita))
    
    hc <- hchart(data_ordered, "bar", hcaes(x = GeoName, y = income_per_capita, text = paste(GeoName, ": $", formatC(income_per_capita, format = "d", big.mark = ",")))) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_yAxis(title = list(text = "Income Per Capita")) %>%
      hc_tooltip(
        formatter = JS(
          "function() {
          return this.point.text;
        }"
        )
      ) %>%
      hc_title(text = "Income Per Capita for All") %>%
      hc_colors("#428f61") %>%
      hc_credits(enabled = FALSE) %>%
      hc_exporting(enabled = FALSE)
    
    hc
  })
  # output$plot2 <- renderPlotly({
  #   
  #   # Define green color theme
  #   green_theme <- theme_minimal() +
  #     theme(
  #       plot.background = element_rect(fill = "#f4f0f0"),
  #       panel.background = element_rect(fill = "#c4e2c0"),
  #       panel.grid.major = element_line(colour = "white"),
  #       panel.grid.minor = element_line(colour = "white")
  #     )
  #   
  #   # Create bar plot
  #   ggplot1 <- ggplot(data_for_year(), 
  #                     aes(x = reorder(GeoName, income_per_capita), 
  #                         y = income_per_capita,
  #                         text = paste(GeoName, ":", sprintf("$%s", formatC(income_per_capita, format = "d", big.mark = ",")))
  #                         )) +
  #     geom_bar(stat = "identity", fill = "#428f61") +
  #     green_theme +
  #     coord_flip() +
  #     labs(title = "Income Per Capita Over Time", x = "", y = "")
  #   
  #   ggplotly(ggplot1, tooltip = "text") %>%
  #     layout(hovermode = 'closest') %>% # Hover closest to cursor
  #     layout(
  #       hoverlabel = list(bgcolor = "white"
  #                         ))
  # })
  
  #line graph for Page 1
  library(highcharter)
  
  output$plot3 <- renderHighchart({
    
    state_data <- part2_map1 %>%
      filter(GeoName == input$selected_geoName)

    hc <- highchart() %>%
      hc_chart(type = "line") %>%
      hc_xAxis(title = list(text = "Year")) %>%
      hc_yAxis(title = list(text = "Income Per Capita")) %>%
      hc_tooltip(
        formatter = JS(
          "function() {
          var income = Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(this.y);
          return 'Income Per Capita: ' + income + '<br>' +
            'Year: ' + this.x;
        }"
        )
      ) %>%
      hc_title(
        text = paste("Income Per Capita Over Time for", input$selected_geoName)
      ) %>%
      hc_add_series(
        data = state_data,
        type = "line",
        hcaes(x = year, y = income_per_capita),
        name = "Income Per Capita",
        color = "#35B779FF"
      )
    
    hc
  })
  
  
# output$plot3 <- renderHighchart({
# 
#   # Filter data for selected state
#   state_data <- part2_map1 %>%
#     filter(GeoName == input$selected_geoName)
# 
#   # Create line graph
#   hc <- highchart() %>%
#     hc_chart(type = "line") %>%
#     hc_add_series(
#       data = state_data,
#       hcaes(x = year, y = income_per_capita, text = GeoName),
#       color = "#35B779FF",
#       name = "Income Per Capita"
#     ) %>%
#     hc_plotOptions(series = list(marker = list(enabled = FALSE))) %>%
#     hc_tooltip(
#       formatter = JS(
#         "function() {
#           return '<b>' + this.point.text + '</b><br>' +
#             'Income Per Capita: $' + Highcharts.numberFormat(this.y, 0) + '<br>' +
#             'Year: ' + Highcharts.numberFormat(this.x, 0);
#         }"
#       )
#     ) %>%
#     hc_title(
#       text = paste("Income Per Capita Over Time for", input$selected_geoName)
#     ) %>%
#     hc_xAxis(title = list(text = "Year")) %>%
#     hc_yAxis(title = list(text = "Income Per Capita")) %>%
#     hc_credits(enabled = FALSE) %>%
#     hc_exporting(enabled = FALSE)
# 
#   hc
# })
  
  #line graph for Page 3
  output$plot4 <- renderHighchart({
    
    # Color palette is the same as before
    color_palette <- c("#440154FF", "#482878FF", "#3E4A89FF", "#31688EFF", "#26828EFF", "#1F9D89FF", "#35B779FF", "#6DCD59FF")
    
    # Creating a list of data by GeoName
    list_data <- split(part2_map1, part2_map1$GeoName)
    
    hchart <- highchart() 
    
    # Adding data series
    for (i in 1:length(list_data)) {
      hchart <- hchart %>% 
        hc_add_series(
          list_data[[i]], 
          type = "line", 
          hcaes(x = year, y = income_per_capita), 
          name = unique(list_data[[i]]$GeoName),
          color = color_palette[i %% length(color_palette) + 1]  # Selecting color from palette in a cyclical manner
        )
    }
    
    hchart %>% 
      hc_title(text = "Income Per Capita Over Time") %>% 
      hc_xAxis(title = list(text = "Year")) %>% 
      hc_yAxis(title = list(text = "Income Per Capita")) %>% 
      hc_tooltip(useHTML = TRUE, formatter = JS("
      function() {
        var income = Intl.NumberFormat('en-US', { style: 'currency', currency: 'USD' }).format(this.y);
        return '<b>' + this.series.name + '</b><br>Income Per Capita: ' + income + '<br>Year: ' + this.x;
      }
    "))
  })
}

shinyApp(ui,server)
