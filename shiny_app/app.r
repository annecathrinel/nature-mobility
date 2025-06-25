library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)
library(htmlwidgets)
library(shinyWidgets)
library(scales)

load("C:/Users/acali/OneDrive - Danmarks Tekniske Universitet/nature_mobility/R_workspaces/workspace_DF_ALL_FINAL_MODELS.RData")
load("C:/Users/acali/OneDrive - Danmarks Tekniske Universitet/nature_mobility/R_workspaces/workspace_nat_use_avail_dfs.RData")

urban_labels <- c("30" = "Urban Centre", "23" = "Dense Urban Cluster", "22" = "Semi-Dense Urban Cluster", "21" = "Suburban", "13" = "Rural Cluster", "12" = "Low Density Rural", "11" = "Very Low Density Rural")
urban_order <- c("Suburban", "Very Low Density Rural", "Semi-Dense Urban Cluster", "Low Density Rural", "Dense Urban Cluster", "Rural Cluster", "Urban Centre")

HS_no_built_df$gender <- factor(HS_no_built_df$gender, levels = c("Male", "Female"))
HS_no_built_df$urban_label <- factor(HS_no_built_df$urban_label, levels = urban_order)
HS_no_built_df$urban_group <- factor(HS_no_built_df$urban_group)

plot_m10_facets <- function(df, response_type) {
  df$urban_code <- as.integer(as.character(df$facet))
  df$urban_label <- factor(urban_labels[as.character(df$urban_code)], levels = urban_order)
  ylabel <- if (response_type == "HD") "Habitat diversity index" else "Habitat selection index"
  ggplot(df, aes(x = x, y = predicted, fill = group)) +
    geom_col(position = position_dodge(0.95)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(0.95)) +
    facet_wrap(~urban_label, ncol = 2) + 
    labs(x = "Age group", y = ylabel, fill = "Sex") +
    scale_fill_manual(values = c("Female" = "#fbb4ae", "Male" = "#89bbe5")) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    #guides(fill = guide_legend(nrow = 1, theme = theme(legend.title = element_text(hjust = 0.5), legend.position.inside = c(0.7, 0.01)))) +
    theme_classic(base_size = 12) +
    theme(strip.background = element_blank(), panel.spacing.y = unit(2, "lines"), axis.title.y = element_text(margin = margin(b = 30, unit = 'mm'), size = 12), legend.title = element_text(size = 10), legend.text = element_text(size = 8), axis.text.y = element_text(margin = margin(l = 10)))
}

plot_m5_bar <- function(df, response_type) {
  ylabel <- if (response_type == "HD") "Habitat diversity index" else "Habitat selection index"
  ggplot(df, aes(x = x, y = predicted, fill = Group)) +
    geom_col(position = position_dodge(0.95)) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, position = position_dodge(0.95)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(x = "Age group", y = ylabel, fill = "Urbanization + Sex") +
    scale_fill_manual(values = c("Urban.Male" = "#f5964e", "Urban.Female" = "#f5b07b", "Rural.Male" = "#80c280", "Rural.Female" = "#bfe3bf")) +
    #guides(fill = guide_legend(nrow = 2, theme = theme(legend.title = element_text(hjust = 0.5)))) +
    theme_classic(base_size = 12) +
    theme(axis.title.y = element_text(margin = margin(b = 30, unit = 'mm'), size = 11), axis.title.x = element_text(margin = margin(t = 5), size = 11), legend.title = element_text(hjust = 0.5, size = 10), legend.text = element_text(size = 8), axis.text.y = element_text(margin = margin(l = 10)))
}

plot_zero_bar <- function(df, response_type) {
  ylabel <- if (response_type == "HD") "Probability of H = 0" else "Probability of HS = 0"
  ggplot(df, aes(x = x, y = predicted)) +
    geom_col(position = position_dodge(0.9), fill = c("#fbb4ae", "#89bbe5")) +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.05, position = position_dodge(0.9)) +
    labs(x = "Sex", y = ylabel) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme_classic(base_size = 12)+
    theme(axis.title.y = element_text(margin = margin(b = 30, unit = 'mm'), size = 11), axis.title.x = element_text(margin = margin(t = 5), size = 11), axis.text.y = element_text(margin = margin(l = 10)))
}

ui <- navbarPage("Nature Mobility Explorer",

  # Tab 1: Model Predictions
  tabPanel("Model Predictions",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        h4("Main Plot Controls"),
        selectInput("response", "Response Type", choices = c("Habitat Diversity" = "HD", "Habitat Selection" = "HS"), selected = "HS"),
        selectInput("buffer", "Buffer",
                    choices = c("5km" = "5km", "10km" = "10km", "Outside 5km" = "5km_out", "Outside 10km" = "10km_out"), selected = "5km"),
        selectInput("model", "Model Version", choices = c("m10", "m5"), selected = "m5"),
        checkboxInput("combine", "Show Fixed + Zero Panels Together", value = TRUE),
        downloadButton("downloadPlot", "Download Fixed Plot"),
        downloadButton("downloadZeroPlot", "Download Zero Plot"),
        tags$hr(),
        checkboxInput("enable_compare", "Enable Comparison", value = FALSE),
        conditionalPanel(
          condition = "input.enable_compare == true",
          h4("Compare With Another Plot"),
          selectInput("response_cmp", "Response Type (Comparison)",
                      choices = c("Habitat Diversity" = "HD", "Habitat Selection" = "HS"), selected = "HS"),
          selectInput("buffer_cmp", "Buffer (Comparison)",
                      choices = c("5km" = "5km", "10km" = "10km", "Outside 5km" = "5km_out", "Outside 10km" = "10km_out"),
                      selected = "10km"),
          selectInput("model_cmp", "Model Version (Comparison)", choices = c("m10", "m5"), selected = "m5"),
          checkboxInput("cmp_fixed", "Compare Fixed Effects", value = TRUE),
          checkboxInput("cmp_zero", "Compare Zero Effects", value = TRUE)
        )
      ),
      mainPanel(uiOutput("dynamicPlots"))
    )
  ),

  # Tab 2: Data Summary Dashboard
  tabPanel("Data Summary Dashboard",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        h4("Filter Data"),
        pickerInput("summary_country", "Country", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)),
        pickerInput("summary_age", "Age Group", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)),
        pickerInput("summary_gender", "Sex", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)),
        pickerInput("summary_urban_group", "Urban Group", choices = NULL, multiple = TRUE, options = list(`actions-box` = TRUE)),
        tags$hr(),
        selectInput("summary_buffer", "Buffer Scope", choices = c("5km" = "5km", "10km" = "10km", "Outside 5km" = "5km_out", "Outside 10km" = "10km_out"))

      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Univariate",
            downloadButton("download_hist_hs", "Download HS Histogram"),
            plotlyOutput("hist_hs"),
            downloadButton("download_hist_div", "Download Diversity Histogram"),
            plotlyOutput("hist_div")
          ),

          tabPanel("Faceted Views",
            plotlyOutput("facet_hs_by_age_gender"),
            plotlyOutput("facet_div_by_region")
          )
        )
      )
    )
  ),

  # Tab 3: Grouped Nature Use
  tabPanel("Grouped Nature Use",
    sidebarLayout(
      sidebarPanel(
        width = 2,
        selectInput("range_select", "Select Home Range",
                    choices = c("5km" = "5km", "10km" = "10km")),
        pickerInput("grouped_gender", "Gender", choices = NULL, multiple = TRUE),
        pickerInput("grouped_age", "Age Group", choices = NULL, multiple = TRUE),
        pickerInput("grouped_urban_group", "Urbanization", choices = NULL, multiple = TRUE),
        checkboxGroupInput("facet_vars", "Facet by:",
                           choices = c("Sex" = "gender", "Age Group" = "age_group", "Urbanization" = "urban_group"),
                           selected = c("gender", "age_group"))
      ),
      mainPanel(
        plotlyOutput("grouped_bar_plot", height = "600px")
      )
    )
  )
)



server <- function(input, output, session) {

  build_name <- function(response, buffer, model, suffix) {
    buffer <- switch(buffer,
      "5km_out" = "5km_out",
      "10km_out" = "10km_out",
      buffer
    )
    paste0(response, "_", buffer, "_", model, "_pred_", suffix)
  }

  fixed_plot <- reactive({
    name <- build_name(input$response, input$buffer, input$model, "fixed")
    if (!exists(name, inherits = TRUE)) return(NULL)
    df <- get(name, inherits = TRUE)
    if (input$model == "m10") plot_m10_facets(df, input$response) else plot_m5_bar(df, input$response)
  })

  zero_plot <- reactive({
    name <- build_name(input$response, input$buffer, input$model, "zero")
    if (!exists(name, inherits = TRUE)) return(NULL)
    df <- get(name, inherits = TRUE)
    plot_zero_bar(df, input$response)
  })

  compare_plot <- reactive({
    req(input$enable_compare && input$cmp_fixed)
    name <- build_name(input$response_cmp, input$buffer_cmp, input$model_cmp, "fixed")
    if (!exists(name, inherits = TRUE)) return(NULL)
    df <- get(name, inherits = TRUE)
    if (input$model_cmp == "m10") plot_m10_facets(df, input$response_cmp) else plot_m5_bar(df, input$response_cmp)
  })

  compare_zero_plot <- reactive({
    req(input$enable_compare && input$cmp_zero)
    name <- build_name(input$response_cmp, input$buffer_cmp, input$model_cmp, "zero")
    if (!exists(name, inherits = TRUE)) return(NULL)
    df <- get(name, inherits = TRUE)
    plot_zero_bar(df, input$response_cmp)
  })

  raw_fixed_plot <- reactive({
    name <- build_name(input$response, input$buffer, input$model, "fixed")
    if (!exists(name, inherits = TRUE)) return(NULL)
    df <- get(name, inherits = TRUE)
    if (input$model == "m10") plot_m10_facets(df, input$response) else plot_m5_bar(df, input$response)
  })

  raw_zero_plot <- reactive({
    name <- build_name(input$response, input$buffer, input$model, "zero")
    if (!exists(name, inherits = TRUE)) return(NULL)
    df <- get(name, inherits = TRUE)
    plot_zero_bar(df, input$response)
  })




  # Titles
  mainModel <- reactive({ if (input$model == "m10") {"~ age group + sex + urban level + (1 | home country), zi = ~ sex + (1 | home country)"} else {"~ age group + sex + urban group + (1 | home country), zi = ~ sex + (1 | home country)"}})
  cmpModel <- reactive({ if (input$model_cmp == "m10") {"~ age group + sex + urban level + (1 | home country), zi = ~ sex + (1 | home country)"} else {"~ age group + sex + urban group + (1 | home country), zi = ~ sex + (1 | home country)"}})

  mainResponseTitle <- reactive({ if (input$response == "HD") "Habitat Diversity" else "Habitat Selection"})
  cmpResponseTitle <- reactive({ if (input$response_cmp == "HD") "Habitat Diversity" else "Habitat Selection"})
  
  mainBufferTitle <- reactive({ if (input$buffer == "5km_out") "Outside 5km" else if (input$buffer == "10km_out") "Outside 10km" else input$buffer })
  cmpBufferTitle <- reactive({ if (input$buffer_cmp == "5km_out") "Outside 5km" else if (input$buffer_cmp == "10km_out") "Outside 10km" else input$buffer_cmp })

  output$mainModelTitle <- renderText({paste("", input$response, mainModel()) })
  output$cmpModelTitle <- renderText({paste("", input$response_cmp, cmpModel()) })
  output$mainTitle <- renderText({ paste("Predicted ", mainResponseTitle(), " ", "(", mainBufferTitle(), ")", sep = "") })
  output$mainTitleZero <- renderText({ paste(" ") })
  output$cmpTitle <- renderText({ paste("Predicted ", cmpResponseTitle(), " ", "(", cmpBufferTitle(), ")", sep = "") })
  output$cmpTitleZero <- renderText({ paste(" ") })

  # Dynamic layout
  output$dynamicPlots <- renderUI({
    if (input$combine) {
      tagList(
        fluidRow(
          column(width = 12,
                 div(
                  h4(textOutput("mainTitle"), style = "font-size: 18px;"),
                  h6(textOutput("mainModelTitle"), style = "font-style: italic; color: #666; font-size: 15px;"), 
                  style = "text-align: center; margin-bottom: 20px;"
                )
            )
        ),
        fluidRow(
          column(width = 8,
               plotlyOutput("fixedPlot", height = "400px")),
          column(width = 4,
               div(style = "width: 95%; margin-top: 95px; margin-left: 20px;", plotlyOutput("zeroPlot", height = "300px")))
      ))
    } else if (!input$enable_compare) {
      fluidRow(
        column(width = 12,
               div(
                h5(textOutput("mainTitle"), style = "font-size: 18px;"),
                h6(textOutput("mainModelTitle"), style = "font-style: italic; color: #666; font-size: 15px;"),
                style = "text-align: center;"),
               plotlyOutput("modelPlot", height = "400px")
              )
          )
    } else {
      fluidRow(
        column(width = 6,
               if (input$cmp_fixed) {
                 tagList(
                   div(h5(textOutput("mainTitle"), style = "font-size: 15px;"), 
                       h6(textOutput("mainModelTitle"), style = "font-style: italic; color: #666; font-size: 13px;"),
                       style = "text-align: center;"),
                   plotlyOutput("modelPlot", height = "400px")
                 )
               },
               if (input$cmp_zero) {
                 tagList(
                   div(h5(textOutput("mainTitleZero")), style = "text-align: center; margin-top: 50px; font-size: 20px;"),
                   div(style = "width: 60%;", plotlyOutput("zeroPlotAlt", height = "300px"))
                 )
               }
        ),
        column(width = 6,
               if (input$cmp_fixed) {
                 tagList(
                   div(h5(textOutput("cmpTitle"), style = "font-size: 15px;"),
                       h6(textOutput("cmpModelTitle"), style = "font-style: italic; color: #666; font-size: 13px;"),
                       style = "text-align: center;"), 
                   plotlyOutput("comparePlot", height = "400px")
                 )
               },
               if (input$cmp_zero) {
                 tagList(
                   div(h5(textOutput("cmpTitleZero")), style = "text-align: center; margin-top: 50px; font-size: 20px;"),
                   div(style = "width: 60%;", plotlyOutput("compareZeroPlot", height = "300px"))
                 )
               }
        )
      )
    }
  })

  # Outputs
  output$fixedPlot <- renderPlotly({ req(fixed_plot()); ggplotly(fixed_plot()) })
  output$zeroPlot <- renderPlotly({ req(zero_plot()); ggplotly(zero_plot()) })
  output$zeroPlotAlt <- renderPlotly({ req(zero_plot()); ggplotly(zero_plot()) })
  output$modelPlot <- renderPlotly({ req(fixed_plot()); ggplotly(fixed_plot()) })
  output$comparePlot <- renderPlotly({ req(compare_plot()); ggplotly(compare_plot()) })
  output$compareZeroPlot <- renderPlotly({ req(compare_zero_plot()); ggplotly(compare_zero_plot()) })

  # Downloads
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste0("model_fixed_plot_", input$response, "_", input$model, "_", input$buffer, ".png")
    },
    content = function(file) {
      ggsave(file, plot = raw_fixed_plot(), device = "png", width = 8, height = 6, dpi = 1000)
    }
  )

  output$downloadZeroPlot <- downloadHandler(
    filename = function() {
      paste0("model_zero_plot_", input$response, "_", input$model, "_", input$buffer, ".png")
    },
    content = function(file) {
      ggsave(file, plot = raw_zero_plot(), device = "png", width = 4, height = 3, dpi = 1000)
    }
  )


  # 1. Dynamic filter updates
  observe({
    updatePickerInput(session, "summary_country", choices = sort(unique(HS_no_built_df$home_country)))
    updatePickerInput(session, "summary_age", choices = unique(HS_no_built_df$age_group))
    updatePickerInput(session, "summary_gender", choices = unique(HS_no_built_df$gender))
    updatePickerInput(session, "summary_urban_group", choices = unique(HS_no_built_df$urban_group))
    updatePickerInput(session, "grouped_gender", choices = unique(summary_combined_df$gender))
    updatePickerInput(session, "grouped_age", choices = unique(summary_combined_df$age_group))
    updatePickerInput(session, "grouped_urban_group", choices = unique(summary_combined_df$urban_group))


  })

  # 2. Reactive filtered dataset
  filtered_summary_df <- reactive({
    df <- HS_no_built_df
    if (!is.null(input$summary_country)) df <- df[df$home_country %in% input$summary_country, ]
    if (!is.null(input$summary_age)) df <- df[df$age_group %in% input$summary_age, ]
    if (!is.null(input$summary_gender)) df <- df[df$gender %in% input$summary_gender, ]
    #if (!is.null(input$summary_urban_label)) df <- df[df$urban_label %in% input$summary_urban_label, ]
    if (!is.null(input$summary_urban_group)) df <- df[df$urban_group %in% input$summary_urban_group, ]
    df
  })

  grouped_data <- reactive({
    req(input$range_select)
    df <- summary_combined_df %>% filter(buffer_range == input$range_select)
    df <- df %>%
      filter(
        is.null(input$grouped_gender) | gender %in% input$grouped_gender,
        is.null(input$grouped_age) | age_group %in% input$grouped_age,
        #is.null(input$grouped_urban_label) | urban_label %in% input$grouped_urban_label,
        is.null(input$grouped_urban_group) | urban_group %in% input$grouped_urban_group
      )
    df
  })

  # 3. Column selector based on buffer
  get_col <- function(metric, type) {
    switch(metric,
      "HS" = paste0("HS_", input$summary_buffer, "_sum"),
      "div" = paste0("HD_", input$summary_buffer, "_use")
    )
  }

  # 4. Histogram plotting helper
  make_histogram_plot <- function(metric, title, xlab) {
    df <- filtered_summary_df()
    col <- get_col(metric, "hist")
    # if (metric == "HS") {
    #   df <- df %>% mutate(log_x = ifelse(.data[[col]] == 0, 0, log10(.data[[col]])))
    #   x_mapping <- "log_x"
    # } else {
    #   x_mapping <- col
    # }
    p <- ggplot(df, aes_string(x = col, fill = "gender", color = "gender")) +
      geom_histogram(bins = 60, alpha = 0.4, position = "identity") +
      scale_fill_manual(values = c("Male" = "#89bbe5", "Female" = "#fbb4ae")) + 
      scale_color_manual(values = c("Male" = "#89bbe5", "Female" = "#fbb4ae")) +
      theme_classic() + labs(title = title, fill = "Sex", y = "Count", x = xlab, color = "Sex") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
      theme(axis.title.x = element_text(margin = margin(t = 10)), axis.title.y = element_text(margin = margin(r = 20)), plot.title = element_text(hjust = 0.5, size = 12, margin = margin(t = 10)))
      if (metric == "HS") {
      p <- p + scale_x_continuous(trans = pseudo_log_trans(base = 10, sigma = 0.05), breaks = c(0, 0.1, 1, 10),  labels = c("0", "0.1", "1", "10"), expand = c(0, 0))
      } else {
      p <- p + scale_x_continuous(expand = c(0, 0))
      }
    return(p)
  }
  render_histogram <- function(metric, title, xlab) {
    renderPlotly({
      ggplotly(make_histogram_plot(metric, title, xlab))
    })
  }


  output$hist_hs <- render_histogram("HS", "Habitat Selection", "HS")
  output$hist_div <- render_histogram("div", "Habitat Diversity", "H")


  # 7. Faceted
  output$facet_hs_by_age_gender <- renderPlotly({
    df <- filtered_summary_df()
    hs_col <- get_col("HS", "facet")
    p <- ggplot(df, aes_string(x = hs_col)) +
      geom_histogram(bins = 100) +
      facet_grid(gender ~ age_group) +
      theme_minimal() + labs(title = "Habitat Selection by Age & Sex") +
      theme(axis.title.x = element_text(margin = margin(t = 10)), axis.title.y = element_text(margin = margin(r = 10)), plot.title = element_text(hjust = 0.5, size = 12, margin = margin(t = 10)))
    ggplotly(p)
  })

  output$facet_div_by_region <- renderPlotly({
    df <- filtered_summary_df()
    hs_col <- get_col("HS", "facet")
    p <- ggplot(df, aes_string(x = hs_col, fill = "gender")) +
      geom_histogram(position = "stack", bins = 100, alpha = 1, boundary = 0, color = "#fffefe21") +
      scale_fill_manual(values = c("Male" = "#89bbe5", "Female" = "#fbb4ae")) + 
      facet_wrap(~region) +
      theme_minimal() + labs(title = "Habitat Selection by Region", fill = "Sex") +
      theme(axis.text.x = element_text(size = 11), plot.title = element_text(hjust = 0.5, size = 12, margin = margin(b = 10)), axis.title.x = element_text(margin = margin(t = 10)), axis.title.y = element_text(margin = margin(r = 5)), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), legend.position = "right")
    ggplotly(p)
  })

    # 7. Grouped Nature Use
  output$grouped_bar_plot <- renderPlotly({
    df <- grouped_data()
    req(nrow(df) > 0)

    df <- df %>%
      filter(type %in% c("Actual Use in Home Range", "Availability in Home Range", "Use Outside Home Range"))
    # Determine facet layout
    n_facets <- length(input$facet_vars)

    if (n_facets == 0) {
      facet_layer <- NULL
    } else if (n_facets == 1) {
      facet_layer <- facet_wrap(as.formula(paste("~", input$facet_vars[1])))
    } else if (n_facets == 2) {
      facet_layer <- facet_grid(as.formula(paste(input$facet_vars[1], "~", input$facet_vars[2])))
    } else {
      # Use facet_wrap with interaction for 3 variables
      df$facet_combo <- interaction(df[input$facet_vars], drop = TRUE, sep = " | ")
      facet_layer <- facet_wrap(~facet_combo)
    }

    nature_labels <- c(
      "10" = "Tree cover",
      "20" = "Shrubland",
      "30" = "Grassland",
      "40" = "Cropland",
      "50" = "Built-up",
      "60" = "Bare / sparse vegetation",
      "70" = "Snow and ice",
      "80" = "Water bodies",
      "90" = "Herbaceous wetland",
      "95" = "Mangroves",
      "100" = "Moss and lichen"
    )
    p <- ggplot(df, aes(x = factor(nature_class), y = mean_prop, fill = type)) +
      geom_col(position = position_dodge(0.8), width = 0.7) +
      scale_fill_manual(values = c("Actual Use in Home Range" = "#4C4CAD",
                                 "Availability in Home Range" = "#C5E3ED",
                                 "Use Outside Home Range" = "#A64CA6")) +
      labs(x = "Nature Class", y = "Mean Proportion",
           title = paste("Nature Use and Avalability (", input$range_select, ")", sep = ""), fill = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
      scale_x_discrete(labels = nature_labels) +
      theme_classic(base_size = 13) +
      theme(strip.background = element_blank(), plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 10)), axis.title.x = element_text(margin = margin(t = 30, b = 60)), axis.title.y = element_text(margin = margin(r = 5)), axis.text.x = element_text(angle = 45, size = 8, hjust=1, vjust = 0.9), panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(), legend.position = "top", plot.margin = margin(b = 60, l = 20), panel.spacing.y = unit(15, "mm"), axis.ticks.x = element_line())
    
    if (!is.null(facet_layer)) {
      p <- p + facet_layer
    }
    
    ggplotly(p)
  })
 
  # 8. Downloads
    # Downloads
  output$download_hist_hs <- downloadHandler(
    filename = function() {
      paste0("histogram_HS_", input$summary_buffer, ".png")
    },
    content = function(file) {
      ggsave(file, plot = make_histogram_plot("HS", "Habitat Selection", "HS"), width = 8, height = 5)
    }
  )

  output$download_hist_div <- downloadHandler(
    filename = function() {
      paste0("histogram_div_", input$summary_buffer, ".png")
    },
    content = function(file) {
      ggsave(file, plot = make_histogram_plot("div", "Habitat Diversity", "H"), width = 8, height = 5)
    }
  )

  
  # lapply(c("hist_hs", "hist_div"), function(id) {
  #   output[[paste0("download_", id)]] <- downloadHandler(
  #     filename = function() paste0(id, ".png"),
  #     content = function(file) {
  #       g <- switch(id,
  #         "hist_hs" = ggplot(filtered_summary_df(), aes_string(x = get_col("HS", "hist"), fill = "gender")) + geom_histogram(),
  #         "hist_div" = ggplot(filtered_summary_df(), aes_string(x = get_col("div", "hist"), fill = "gender")) + geom_histogram()
  #       )
  #       ggsave(file, plot = g + theme_classic())
  #     }
  #   )
  # })
}


shinyApp(ui, server)