#### Debug / Improvement list:
# - Comparison mode should be default probably
# - Reading NUTS-shp to "nuts_path = 'path/data'"
# - Additional metadata? Check "SELFNOTE"
# - Default vars
# - Zoom
# - Palette: value interval, colors ok?
# - EFISCEN:
#   - Forest surface area bugged
#   - Not showing NUTS-names
#   -> Change the NUTS-name to be drawn from shp, if not already. If it is: ?

## Larger Changes:
# - Country-level + NUTS 0, 1, 2, 3. "NUTS1 + NUTS2" too.
# - Average, sum, total, etc?
# - Climate data
# - 

## Others' suggestions:
# "y-axis, on your plot below the map"
# "if there is more metadata- e.g. how the variable was derived, would be good to post that somewhere, e.g. underneath the timeseries plot"
# "y-axis, there should be the "long name" of the variable, not the Variable, e.g. "Growing stock" as opposed to "GS" and "Nitrogen leaching" vs "N"."
# " some countries are grey, or white, and  also white within the EU-simulation extent there is some white, it would be good to have that  described what the difference is. I see grey = no data. and white looks like it could be 0 or no data."
#  - On this. Check the input data and NUTS-data on which countries are included etc.
########################################################################################################

library(shiny)
library(dplyr)
library(readr)
library(sf)
library(leaflet)
library(stringr)
library(scales)
library(tidyr)
library(plotly)

# --- Load static NUTS shape ---
# SELFNOTE: Should change the file path to -> path = "path/to/stuff/", and use it in the other cases later too.
nuts_shape <- st_read("data/nuts/NUTS_RG_60M_2021_4326_LEVL_2.shp")

# --- List forest data files and extract metadata ---
# SELFNOTE: Here also might be better to use path = "XX"
available_files <- list.files("data/forest/NUTS-2_averages", pattern = "\\.csv$", full.names = FALSE)

# Parse scenario, case, and forest_model from filenames
file_info <- str_match(available_files, "^(.+?)_(\\d+)_(.+?)\\.csv$")
file_info <- as.data.frame(file_info, stringsAsFactors = FALSE)
colnames(file_info) <- c("filename", "scenario", "case", "forest_model")
file_info <- file_info[!is.na(file_info$filename), ]

# --- UI ---
# Define the options for the user
# - Choose data
# - Choose mode (Comparison Mode ON / OFF)
# - Absolute Difference / Percentual Change
# - Choose Variable for A and B forest data
# - Choose Years for A and B forest data
# - Choose Radar Chart Variables

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .leaflet-tooltip {
        font-size: 16px !important;
      }
    "))
  ),
  titlePanel("Visualization App - Work in Progress"),
  sidebarLayout(
    sidebarPanel(
      selectInput("file_A", "Select Forest File A", 
                  choices = sort(file_info$filename),
                  selected = {
                    lpj_files <- file_info %>% filter(forest_model == "LPJ-GUESS")
                    if (nrow(lpj_files) >= 1) lpj_files$filename[1] else NULL
                  }),
      uiOutput("file_A_info"),
      checkboxInput("compare_mode", "Enable comparison mode", value = FALSE),
      conditionalPanel(
        condition = "input.compare_mode == true",
        tagList(
          selectInput("file_B", "Select Forest File B", 
                      choices = sort(file_info$filename),
                      selected = {
                        lpj_files <- file_info %>% filter(forest_model == "LPJ-GUESS")
                        if (nrow(lpj_files) >= 2) lpj_files$filename[2] else NULL
                      }),
          uiOutput("file_B_info"),
          radioButtons("compare_type", "Show on Map:",
                       choices = c("Absolute difference" = "absolute",
                                   "Percentual change (%)" = "percent"),
                       selected = "absolute")
        )
      ),
      uiOutput("select_variable"),
      uiOutput("select_year"),
      conditionalPanel(
        condition = "input.compare_mode == true",
        uiOutput("select_variable_compare"),
        uiOutput("select_year_compare")
      ),
      checkboxGroupInput("variable_multi", "Variables to compare (radar plot):", choices = NULL)
    ),
    mainPanel(
      div(style = "position: relative;",
          leafletOutput("map", height = 700),
          absolutePanel(
            bottom = 10, left = 10, width = 350,
            draggable = TRUE,
            style = "background-color: rgba(255,255,255,0.95); padding: 10px; border-radius: 8px;",
            h5("Radar Chart: Click NUTS-area to Activate, and Choose Vars from Bottom Left"),
            uiOutput("radar_info"),
            plotlyOutput("radar_plot", height = 300)
          ),
          absolutePanel(
            top = 10, right = 10, width = 300,
            draggable = TRUE,
            style = "background-color: rgba(255,255,255,0.95); padding: 10px; border-radius: 8px; box-shadow: 0px 0px 5px #aaa;",
            h5("🛈 Usage Guide"),
            tags$ul(
              tags$li("Select forest datasets from the left panel."),
              tags$li("Use comparison mode to view differences."),
              tags$li("Hover over map regions to see details."),
              tags$li("Activate radar chart by clicking on NUTS-region and time series data."),
              tags$li("Use the slider to choose the year."),
              tags$li("Choose multiple variables for radar comparison.")
            )
          )
      ),
      br(),
      plotOutput("timeseries_plot", height = 300),
      br(),
      uiOutput("summary_info"),
      tableOutput("summary_stats")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {

  load_forest_data <- function(filename) {
    req(filename)
    # SELFNOTE: This to -> path = "XXXX"
    filepath <- file.path("data/forest/NUTS-2_averages", filename)
    validate(need(file.exists(filepath), paste("File not found:", filename)))

    read_csv(filepath, col_types = cols(
      .default = col_guess(),
      NUTS_NAME = col_character()
    )) %>%
      mutate(across(where(is.character), str_trim))
  }

  # SELFNOTE: Additional metadata could be good. Variable is used in couple of instances, so maybe include that.
  get_metadata <- function(filename) {
    entry <- file_info[file_info$filename == filename, ]
    if (nrow(entry) == 1) {
      paste("Forest Model:", entry$forest_model,
            "| Scenario:", entry$scenario,
            "| Exploratory Case:", entry$case)
    } else {
      "Metadata not found."
    }
  }

  forest_data_A <- reactive({
    data <- load_forest_data(input$file_A)

    # SELFNOTE: Debug-prints
    #cat("=== forest_data_A loaded ===\n")
    #cat("File A:", input$file_A, "\n")
    #print("Column names:")
    #print(names(data))
    #print("First 3 rows:")
    #print(head(data, 3))

    return(data)
  })
  #forest_data_B <- reactive({ if (isTRUE(input$compare_mode)) load_forest_data(input$file_B) else NULL })
  forest_data_B <- reactive({
    if (isTRUE(input$compare_mode)) {
      data <- load_forest_data(input$file_B)

      # SELFNOTE: Debug-prints
      #cat("=== forest_data_B loaded ===\n")
      #cat("File B:", input$file_B, "\n")
      #print("Column names:")
      #print(names(data))
      #print("First 3 rows:")
      #print(head(data, 3))

      return(data)
    } else {
      return(NULL)
    }
  })

  output$file_A_info <- renderUI({
    req(input$file_A)
    HTML(paste("<small>", get_metadata(input$file_A), "</small>"))
  })

  output$file_B_info <- renderUI({
    req(input$compare_mode, input$file_B)
    HTML(paste("<small>", get_metadata(input$file_B), "</small>"))
  })

  output$radar_info <- renderUI({
    req(input$file_A)

    text_a <- paste("A:", get_metadata(input$file_A))

    if (isTRUE(input$compare_mode) && !is.null(input$file_B)) {
      text_b <- paste("B:", get_metadata(input$file_B))
      HTML(paste("<small>", text_a, "<br/>", text_b, "</small>"))
    } else {
      HTML(paste("<small>", text_a, "</small>"))
    }
  })

  output$timeseries_info <- renderUI({
    req(input$file_A)
    HTML(paste("<strong>Time Series Info:</strong><br/>", get_metadata(input$file_A)))
  })

  output$summary_info <- renderUI({
    req(input$file_A, input$variable)
    HTML(paste(
      "<strong>Summary of all NUTS-regions for Chosen Variable  :</strong><br/>",
      get_metadata(input$file_A),
      "| Variable:", input$variable
    ))
  })

  observe({
    updateCheckboxGroupInput(session, "variable_multi", choices = sort(unique(forest_data_A()$variable)))
  })

  # SELFNOTE: Some more clever way to set the default var. 
  # Now just "Stand_C", could be set to only positive vars 
  # (although all vars should be positive in the final outputs?)
  output$select_variable <- renderUI({
    vars <- sort(unique(forest_data_A()$variable))
    default_var <- vars[startsWith(vars, "Stand_C")][1]
    if (is.na(default_var)) default_var <- vars[1]

    selectInput("variable", "Variable A", choices = vars, selected = default_var)
  })

  # SELFNOTE: Same as above
  output$select_variable_compare <- renderUI({
    req(forest_data_B())
    vars <- sort(unique(forest_data_B()$variable))
    default_var <- vars[startsWith(vars, "Stand_C")][1]
    if (is.na(default_var)) default_var <- vars[1]

    selectInput("variable_compare", "Variable B", choices = vars, selected = default_var)
  })

  # SELFNOTE: Similar to above
  output$select_year <- renderUI({
    years <- sort(unique(forest_data_A()$year))
    default_year <- if (2025 %in% years) 2025 else min(years)

    sliderInput("year", "Year A", min = min(years), max = max(years),
                value = default_year, step = 1, sep = "",
                animate = animationOptions(interval = 1500))
  })

  # SELFNOTE: Same
  output$select_year_compare <- renderUI({
    req(forest_data_B())
    years <- sort(unique(forest_data_B()$year))
    default_year <- if (2050 %in% years) 2050 else max(years)

    sliderInput("year_compare", "Year B", min = min(years), max = max(years),
                value = default_year, step = 1, sep = "",
                animate = animationOptions(interval = 1500))
  })

  filtered_data_A <- reactive({
    req(input$variable, input$year)
    forest_data_A() %>% filter(variable == input$variable, year == input$year)
  })

  filtered_data_B <- reactive({
    req(input$compare_mode, input$variable_compare, input$year_compare)
    forest_data_B() %>% filter(variable == input$variable_compare, year == input$year_compare)
  })

  selected_nuts <- reactiveVal(NULL)
  observeEvent(input$map_shape_click, { selected_nuts(input$map_shape_click$id) })

  # SELFNOTE: The zoom should be checked.
  map_state <- reactiveValues(zoom = 4, center = list(lng = 10, lat = 57))
  observeEvent(input$map_zoom, { map_state$zoom <- input$map_zoom })
  observeEvent(input$map_center, { map_state$center <- input$map_center })

  output$map <- renderLeaflet({
    req(nuts_shape)
    isolate({ zoom <- map_state$zoom; center <- map_state$center })

    if (isTRUE(input$compare_mode)) {
      df_diff <- full_join(
        filtered_data_A() %>% select(NUTS_ID, value_A = weighted_average_value, unit, forest_surface_area, surface_area, nuts_name = NUTS_NAME),
        filtered_data_B() %>% select(NUTS_ID, value_B = weighted_average_value),
        by = "NUTS_ID"
      ) %>% mutate(
        difference = value_B - value_A,
        percent_change = ifelse(!is.na(value_A) & value_A != 0, (value_B - value_A) / value_A * 100, NA)
      )

      map_data <- nuts_shape %>% left_join(df_diff, by = "NUTS_ID")

      display_col <- if (input$compare_type == "absolute") "difference" else "percent_change"
      max_abs <- max(abs(map_data[[display_col]]), na.rm = TRUE)
      # SELFNOTE: Is palette ok? The value interval should remain somewhat constant
      pal <- colorNumeric("RdYlBu", domain = c(-max_abs, max_abs), na.color = "#d9d9d9", reverse = TRUE)

      leaflet(map_data) %>%
        setView(lng = center$lng, lat = center$lat, zoom = zoom) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(map_data[[display_col]]),
          fillOpacity = 0.8,
          color = "#333", weight = 0.7, layerId = ~NUTS_ID,
          label = ~lapply(paste0(
            "<strong>NUTS ID:</strong> ", NUTS_ID,
            if (input$compare_type == "absolute") {
              paste0("<br/><strong>Difference:</strong> ", round(difference, 4), " ", unit)
            } else {
              paste0("<br/><strong>% Change:</strong> ", round(percent_change, 2), "%")
            },
            # SELFNOTE: Check this. EFISCEN not ok.
            "<br/><strong>Forest Surface Area:</strong> ",
            formatC(forest_surface_area, format = "f", big.mark = ",", digits = 2), " km²",
            "<br/><strong>NUTS-area Surface Area:</strong> ",
            formatC(surface_area, format = "f", big.mark = ",", digits = 0), " km²",
            "<br/><strong>NUTS Area Name:</strong> ", nuts_name
          ), htmltools::HTML),
          highlightOptions = highlightOptions(weight = 2, color = "#000", bringToFront = TRUE)
        ) %>%
        addLegend("bottomright", pal = pal,
                  values = map_data[[display_col]][!is.na(map_data[[display_col]])],
                  title = ifelse(input$compare_type == "absolute", "Difference (B - A)", "Percentual Change (%)"),
                  opacity = 0.8,
                  labFormat = labelFormat(digits = 2)) %>%
        addLegend("bottomright", colors = "#d9d9d9", labels = "No data", opacity = 0.8, title = NULL)

    } else {
      df <- filtered_data_A() %>%
        filter(!is.na(weighted_average_value)) %>%
        select(NUTS_ID, weighted_average_value, forest_surface_area, surface_area, nuts_name = NUTS_NAME, unit)
      map_data <- nuts_shape %>% left_join(df, by = "NUTS_ID")
      pal <- colorNumeric("YlGn", domain = map_data$weighted_average_value, na.color = "#d9d9d9")

      leaflet(map_data) %>%
        setView(lng = center$lng, lat = center$lat, zoom = zoom) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(weighted_average_value),
          fillOpacity = 0.8,
          color = "#333", weight = 0.7, layerId = ~NUTS_ID,
          label = ~lapply(paste0(
            "<strong>NUTS ID:</strong> ", NUTS_ID,
            "<br/><strong>Value:</strong> ",
            ifelse(weighted_average_value < 0.01,
                   formatC(weighted_average_value, format = "e", digits = 2),
                   round(weighted_average_value, 2)), " ",
            unit,
            # SELFNOTE: Check this. EFISCEN not ok.
            "<br/><strong>Forest Surface Area:</strong> ",
            formatC(forest_surface_area, format = "f", big.mark = ",", digits = 2), " km²",
            "<br/><strong>NUTS-area Surface Area:</strong> ",
            formatC(surface_area, format = "f", big.mark = ",", digits = 0), " km²",
            "<br/><strong>NUTS Area Name:</strong> ", nuts_name
          ), htmltools::HTML),
          highlightOptions = highlightOptions(weight = 2, color = "#000", bringToFront = TRUE)
        ) %>%
        addLegend("bottomright", pal = pal,
                  values = map_data$weighted_average_value[!is.na(map_data$weighted_average_value)],
                  title = paste0(input$variable, " (", input$year, ")"), opacity = 0.8,
                  labFormat = labelFormat(digits = 3)) %>%
        addLegend("bottomright", colors = "#d9d9d9", labels = "No data", opacity = 0.8, title = NULL)
    }
  })

  # SELFNOTE: Fix this:
  # - Normalization not working properly if value == 0
  # - When hovering over: "zoom, download etc." showing on top of other text
  # - Variable names not showing
  # - Probably something else too

  output$radar_plot <- renderPlotly({
    req(input$variable_multi, selected_nuts(), input$year)

    df_all_A <- forest_data_A() %>% filter(variable %in% input$variable_multi)
    variable_ranges <- df_all_A %>% group_by(variable) %>%
      summarise(min_val = min(weighted_average_value, na.rm = TRUE),
                max_val = max(weighted_average_value, na.rm = TRUE), .groups = "drop")

    df_nuts_A <- forest_data_A() %>% filter(NUTS_ID == selected_nuts(),
                                            variable %in% input$variable_multi,
                                            year == input$year)

    df_norm_A <- df_nuts_A %>% left_join(variable_ranges, by = "variable") %>%
      mutate(norm_value = ifelse(max_val != min_val,
                                 (weighted_average_value - min_val) / (max_val - min_val), 0.5),
             variable = factor(variable, levels = input$variable_multi)) %>%
      arrange(variable)

    validate(need(nrow(df_norm_A) > 0, "No data for radar chart."))

    p <- plot_ly(
      type = 'scatterpolar',
      r = df_norm_A$norm_value,
      theta = df_norm_A$variable,
      fill = 'toself',
      mode = "lines+markers",
      name = paste("A:", input$year),
      text = paste0("<b>", df_norm_A$variable, "</b><br/>",
                    "Norm.: ", round(df_norm_A$norm_value, 2), "<br/>",
                    "Raw: ", signif(df_norm_A$weighted_average_value, 4)),
      hoverinfo = "text"
    )

    if (input$compare_mode && !is.null(forest_data_B())) {
      df_nuts_B <- forest_data_B() %>% filter(NUTS_ID == selected_nuts(),
                                              variable %in% input$variable_multi,
                                              year == input$year_compare)

      df_norm_B <- df_nuts_B %>% left_join(variable_ranges, by = "variable") %>%
        mutate(norm_value = ifelse(max_val != min_val,
                                   (weighted_average_value - min_val) / (max_val - min_val), 0.5),
               variable = factor(variable, levels = input$variable_multi)) %>%
        arrange(variable)

      if (nrow(df_norm_B) > 0) {
        p <- p %>% add_trace(
          r = df_norm_B$norm_value,
          theta = df_norm_B$variable,
          fill = 'toself',
          mode = "lines+markers",
          name = paste("B:", input$year_compare),
          text = paste0("<b>", df_norm_B$variable, "</b><br/>",
                        "Norm.: ", round(df_norm_B$norm_value, 2), "<br/>",
                        "Raw: ", signif(df_norm_B$weighted_average_value, 4)),
          hoverinfo = "text"
        )
      }
    }

    p %>% layout(
      polar = list(radialaxis = list(visible = TRUE, range = c(0, 1))),
      showlegend = TRUE,
      title = paste("Radar Chart for", selected_nuts())
    )
  })

  # SELFNOTE: Mikko had something on this.
  output$timeseries_plot <- renderPlot({
    req(selected_nuts(), input$variable)

    df_A <- forest_data_A() %>% 
      filter(NUTS_ID == selected_nuts(), variable == input$variable)

    validate(need(nrow(df_A) > 0, "No data for selected NUTS region."))

    plot(df_A$year, df_A$weighted_average_value, type = "b", pch = 19,
         xlab = "Year", ylab = input$variable,
         main = paste("Time Series for", selected_nuts(), 
                      "\n", get_metadata(input$file_A), 
                      "| Variable:", input$variable),
         col = "darkgreen", ylim = range(df_A$weighted_average_value, na.rm = TRUE),
         lwd = 2)

    if (isTRUE(input$compare_mode) && !is.null(forest_data_B())) {
      df_B <- forest_data_B() %>% 
        filter(NUTS_ID == selected_nuts(), variable == input$variable)

      if (nrow(df_B) > 0) {
        all_values <- c(df_A$weighted_average_value, df_B$weighted_average_value)
        ylim_range <- range(all_values, na.rm = TRUE)
        plot(df_A$year, df_A$weighted_average_value, type = "b", pch = 19,
             xlab = "Year", ylab = input$variable,
             main = paste("Time Series for", selected_nuts(), 
                      "\n", get_metadata(input$file_A), 
                      "| Variable:", input$variable),
             col = "darkgreen", ylim = ylim_range, lwd = 2)
        lines(df_B$year, df_B$weighted_average_value, type = "b", pch = 17,
              col = "blue", lwd = 2)
        legend("topright", legend = c("File A", "File B"),
               col = c("darkgreen", "blue"), pch = c(19, 17), lwd = 2)
      }
    }
  })

  # SELFNOTE: Could do with some work
  output$summary_stats <- renderTable({
    df <- filtered_data_A() %>% filter(!is.na(weighted_average_value))
    validate(need(nrow(df) > 0, "No data available for selected combination."))
    df %>% summarise(
      !!paste0("Number of NUTS regions for year: ", input$year) := n(),
      Mean = round(mean(weighted_average_value), 4),
      Min = round(min(weighted_average_value), 4),
      Max = round(max(weighted_average_value), 4)
    )
  })
}

shinyApp(ui, server)
