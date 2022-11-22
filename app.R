
# SPECIAL REQUIREMENTS ----
# accordion is used from bslib, same as layout_sidebar
# remotes::install_github("mdancho84/bslib")
# remotes::install_github("mdancho84/histoslider")


# LIBRARIES ----
library(shiny)
library(bslib)
library(shinyWidgets)
library(plotly)
library(htmltools)
library(shinyjs)
library(histoslider)
library(plotly)
library(gridlayout)
library(bsicons)
library(ggthemes)

# For RainCloud Plot
library(chronicle)
library(gghalves)

# Core
library(here)
library(tidyverse)

# Table
library(gt)
library(gtExtras)

# Modeling
library(xgboost)
library(parsnip)

# Custom CSS Loaders
library(shinycssloaders)



# IMPORT FUNCTIONS ----
source(here::here("00_themes/app_theme.R"))
source(here::here("00_vars/variables.R"))
source("01_functions/functions_web_app.R")


# Import XGBoost Model
model_xgboost <- read_rds("00_models/model_xgboost.rds")


# Palettes
theme_base_palette <- c("#000000cc", "#FF6647", "#2C3E50")


# Define UI for application that draws a histogram
ui <- page_navbar(
  tags$head(
    tags$style(HTML("
    .navbar-brand {
    display: flex;
    }
                    "))
  ),
  theme = app_theme_base,
  title = list(
    tags$img(
      src   = LOGO,
      style = "width:64px;height:64px;margin-right:24px;-webkit-filter: drop-shadow(5px 5px 5px #222);"
    ),
    h4(TITLE)
  ),
  fluid = TRUE,
  useShinyjs(),
  # Introduction Tab ----
  nav(
    h3(FIRST_TAB), includeCSS("css/styles.css"),
    page_fluid(
      h1("Wages in the Data Science Industry: A Quick Glimpse!"),
      br(),
      p(strong(em("\ Happiness is not in the mere possession of money;",
                  br(),
      "it lies in the joy of achievement,",
                  br(),
      "in the thrill of creative effort. --Franklin D. Roosevelt"))),
      hr(),
      br(),
      h3(em("Summary")),
      p("This web app is based on a popular dataset on Kaggle called", a("Data Science Job Salaries.", href = "https://www.kaggle.com/datasets/ruchi798/data-science-job-salaries"), 
        br(),
        "The dataset gives a", strong("glimpse"), "at the salaries given to data scientists in different roles,",
        br(),
        "locations, job and remote statuses.The goal of this web app is to display the different",
        br(),
        " levels of salaries by these dimensions and to come with an approximation what would",
        br(),
        "be the approximate salary for a given position, location and status using regression models",
        br(),
        " (Located is in the Regression tab).",
        br(),
      h3(em("Dimensions"), style="text-align: center; padding-left: 400px"),
      div(img(src = "dimensions_table.PNG", height = 500, width = 800), style="text-align: right;"),
      br(),
      h3(em("Thank you note")),
      "Special thanks to:",
      br(),
      "1)", a("ai-jobs.net", href = "ai-jobs.net"), "for aggregating the data.",
      br(),
      "2)", a("Ruchi Bhatia", href = "https://www.kaggle.com/ruchi798"), "for uploading the dataset in", strong("Kaggle"),
      br(),
      "3)", a("David Smale", href = "https://davidsmale.netlify.app/portfolio/"), "for being a source of inspiration with your amazing work!",
      br(),
      "4)", a("Matt Dancho (Business Science)", href = "https://university.business-science.io/"), "for teaching and guiding me throughout all these years.")
    ),
    div(id = "sources",
    strong("Developed by"), a("Janio Martinez Bachmann", href = "https://www.linkedin.com/in/janio-martinez-bachmann-26040ba1/"), ".",
    br(),
    HTML(
    "<b> R Packages: </b>", "Tidyverse, gt, gtExtras, bslib, <br>  
    shinyWidgets, shinyjs, histoslider, gridLayout,<br> 
    bsicons, chronicle, gghalves and here"),
    br(),
    strong("Sources:"), a("ai-jobs.net", href = "ai-jobs.net"), "and", a("Kaggle", href = "Kaggle.com")
  )),
  # Data Exploration Tab ----
  nav(
    h3(SECOND_TAB),
    layout_sidebar(
      bg_colors = "#DADADA",
      side_width = 400,
      side = list(
        accordion(
          selected = I("none"),
          accordion_item(
            title = "HQ Location",
            icon = icon("location-pin"),
            input_check_search(
              id = "company_location",
              choices = unique(data$company_location) %>% sort(),
              selected = unique(data$company_location) %>% sort(),
              width = '100%',
              height = "325px"
            )
            
          ),
          accordion_item(
            title = "Employment Type",
            icon  = icon("briefcase"),
            input_check_search(
              id = "employment_type",
              choices = unique(data$employment_type),
              selected = unique(data$employment_type),
              width = "100%",
              height = "600px"
            )
            ),
          accordion_item(
            title = "Experience Level",
            icon  = icon("lightbulb"),
            input_check_search(
              id = "experience_level",
              choices = unique(data$experience_level) %>% sort(),
              selected = unique(data$experience_level) %>% sort(),
              width = '100%',
              height = "200px"
            )
          ),
          accordion_item(
            title = "Company Size",
            icon = icon("building"),
            input_check_search(
              id = "company_size",
              choices = unique(data$company_size) %>% sort(),
              selected = unique(data$company_size) %>% sort(),
              width = '100%',
              height = "150px"
            )
          ),
          accordion_item(
            title = "Salary Distribution",
            icon = icon("dollar-sign"),
            input_histoslider(
              id    = "histoslider_salary",
              label = "Salary USD",
              values = data$salary_in_usd,
              height = 125,
              options = list(
                selectedColor = "#000000cc",
                handleLabelFormat = "$,.0f"
              )
            )
          ),
          
          hr(),
          
          actionButton(
            inputId = "submit",
            label   = "Submit",
            icon    = icon("play"),
            class   = "btn-primary"
            ),
          
          actionButton(
            inputId = "reset",
            label   = "Reset",
            icon    = icon("rotate"),
            class   = "btn-primary"
          ),
          hr(),
          
          input_switch(
            id    = "dark_mode",
            label = strong("Switch Theme"),
            value = F
          )
        )
      ),
      
      main = list(
        uiOutput("score_cards") %>% withSpinner(),
        card_grid(
          card_width = "200px",
          class = "my-3 border-0",
          
          # First Plot
          card(
            class = "card mb-4 border-0",
            width = "100%",
            card_body(
              stretch = TRUE,
              plotOutput(outputId = "plot_1",
                         height = "650px") %>% withSpinner()
              
            ),
            full_screen = TRUE
          ),
          card(
            class = "card mb-4 border-0",
            card_body(
              stretch = TRUE,
              shinyWidgets::radioGroupButtons(
                inputId = "year",
                size    = "lg",
                choices = c("2020" = "2020", "2021" = "2021", "2022" = "2022", "All" = "All"),
                selected = "All",
                status = "primary",
                justified = TRUE,
                checkIcon = list(
                  yes = icon("ok", lib = "glyphicon"), 
                  no  = NULL
                )
              ),
              gt_output(outputId = "table_1") %>% withSpinner()
            )
          )
        )
      )
    )
  ),
  # Regression Tab ----
  nav(
    title = h3(THIRD_TAB),
  sidebarLayout(
    sidebarPanel(
     HTML("<b> Pick your Features! </b>"),
      width = 2,
      # Job Title
      selectInput(
        inputId = "job_title_regression",
        label   =  tags$span(
          "Job Title", 
          tags$i(
            class = "glyphicon glyphicon-sort-by-alphabet", 
            style = "color:#333333;",
            title = "Job Title "
          )),
        choices = unique(regression_data$job_title) %>% sort(),
      ),
      
      # Location Checkbox
      selectInput(
        inputId = "location_regression",
        label   = tags$span(
          "Where would you like to work?", 
          tags$i(
            class = "glyphicon glyphicon-briefcase", 
            style = "color:#333333;",
            title = "Further information "
          )),
        choices = unique(regression_data$company_location) %>% sort(),
        selected = NULL
      ),
      br(),
      selectInput(
        inputId = "employment_type_regression",
        label   =  tags$span(
          "Type of Contract?", 
          tags$i(
            class = "fontawesome glyphicon-pencil", 
            style = "color:#333333;",
            title = "Further information "
          )),
        choices = unique(regression_data$employment_type) %>% sort()
      ),
      br(),
      selectInput(
        inputId = "experience_level_regression",
        label   =  tags$span(
          "Level of experience", 
          tags$i(
            class = "glyphicon glyphicon-star", 
            style = "color:#333333;",
            title = "Further information "
          )),
        choices = unique(regression_data$experience_level) %>% sort()
      ),
      br(),
      selectInput(
        inputId = "company_size_regression",
        label   =  tags$span(
          "Company Size", 
          tags$i(
            class = "glyphicon glyphicon-plus", 
            style = "color:#333333;",
            title = "Further information "
          )),
        choices = unique(regression_data$company_size) %>% sort()
      ),
      br(),
      selectInput(
        inputId = "remote_status_regression",
        label   =  tags$span(
          "Remote Preference", 
          tags$i(
            class = "glyphicon glyphicon-globe", 
            style = "color:#333333;",
            title = "Further information "
          )),
        choices = unique(regression_data$remote_status) %>% sort()
      ),
      hr(),
      
      actionButton(
        inputId = "submit_regression",
        label   = "Submit",
        icon    = icon("play"),
        class   = "btn-primary"
      ),
      
      actionButton(
        inputId = "reset_regression",
        label   = "Reset",
        icon    = icon("rotate"),
        class   = "btn-primary"
      ),
      hr()
    ),
    
    mainPanel(HTML("<b> Salary Prediction Plot </b>"),
              height = 9,
              splitLayout(cellWidths = c("60%", "40%"), 
                          plotlyOutput(outputId = "plotly_2", height = "800px") %>% withSpinner(),
                                         tableOutput(outputId = "table_2") %>% withSpinner(),
                          cellArgs = list(style = "padding-left: 100px")) 
              )
  )
  )
)


server <- function(input, output, session) {
  
  # Observers Theme ----
  observeEvent(input$dark_mode, handlerExpr = {
    if(input$dark_mode){
      session$setCurrentTheme(app_theme_cyberpunk)
    } else {
      session$setCurrentTheme(app_theme_base)
    }
  })
  
  
  # Reset Events - Explore 
  observeEvent(eventExpr = input$reset, handlerExpr = {
    
    
    update_check_search(
      session = session,
      id      = "company_location",
      selected = unique(data$company_location) %>% sort()
    )
    
    updateCheckboxGroupButtons(
      session = session,
      inputId = "employment_type",
      selected = unique(data$employment_type) %>% sort()
    )
    
    
    update_check_search(
      session = session,
      id      = "experience_level",
      selected = unique(data$experience_level) %>% sort()
    )
    
    
    update_check_search(
      session = session,
      id      = "company_size",
      selected = unique(data$company_size) %>% sort()
    )
    
    update_histoslider(
      session = session,
      id = "histoslider_salary",
      values = data$salary_in_usd
    )
    
    updateRadioGroupButtons(
      session = session, 
      inputId = "year", 
      selected = "All"
    )
  }) 
  
  # Reset Regression
  observeEvent(input$reset_regression, handlerExpr = {
    
    updateSelectInput(
      session = session,
      inputId = "location_regression",
      selected = "AT"
    )
    
    updateSelectInput(
      session = session,
      inputId = "employment_type_regression",
      selected = "Freelance"
    )
    
    updateSelectInput(
      session = session,
      inputId = "experience_level_regression",
      selected = "Entry Level"
    )
    
    updateSelectInput(
      session = session,
      inputId = "company_size_regression",
      selected = "Large"
    )
    
    updateSelectInput(
      session = session,
      inputId = "remote_status_regression",
      selected = "Hybrid Work"
    )
    
    updateSelectInput(
      session = session,
      inputId = "job_title_regression",
      selected = "AI Scientist"
    )
    
  })
  
  # Submit after clicking reset: Explore Tab
  observeEvent(input$reset, {
    delay(300, click(id = "submit"))
  }, once = FALSE)
  
  # Submit after clicking reset: Regression Tab
  observeEvent(input$reset_regression, {
    delay(300, click(id = "submit_regression"))
  }, once = FALSE)
    
    # Data Processing ----
    # Main filters are applied.
    
    main_filtered_tbl <- eventReactive(
      eventExpr = input$submit,
      valueExpr = {
        data %>% 
          filtering_year_tbl(year_date = input$year) %>% 
          filter(company_location %in% input$company_location) %>% 
          filter(employment_type %in% input$employment_type) %>% 
          filter(experience_level %in% input$experience_level) %>% 
          filter(company_size %in% input$company_size) %>% 
          filter(salary_in_usd %>% between(
            left  = input$histoslider_salary[1],
            right = input$histoslider_salary[2]
          ))
      },
      ignoreNULL = FALSE
    )
  
  
  main_regression_tbl <- eventReactive(
    eventExpr = input$submit_regression,
    valueExpr = {
      new_salary_pred <- generate_salary_prediction(
        company_location = input$location_regression,
        employment_type = input$employment_type_regression, 
        experience_level = input$experience_level_regression,
        company_size = input$company_size_regression,
        remote_status = input$remote_status_regression,
        job_title = input$job_title_regression,
        .ml_model = model_xgboost
      ) %>% 
        append_pred_tbl(employee_tbl = regression_data)
    },
    ignoreNULL = FALSE
  )
  
    
    # Main Table for Scorecards ----
    
    main_summary_tbl <- reactive({
      
      main_filtered_tbl() %>% 
        summarize(
          avg_usd_salary      = mean(salary_in_usd, na.rm = TRUE),
          total_employees     = n(),
          remote_ratio        = (sum(str_detect(remote_status, "Work from Home")) / n()) %>% round(1)
        )
      
    })
  
  # Second Plot Table
  avg_salaries_tbl <- reactive({
    main_filtered_tbl() %>% 
      group_by(job_title) %>% 
      summarise(
        avg_salary = mean(salary_in_usd, na.rm = TRUE)
      ) %>% 
      ungroup() %>% 
      mutate(
        label_txt = avg_salary %>% scales::dollar()
      )
  })
  
    
    
    output$score_cards <- renderUI({
      
    req(main_summary_tbl())
    
    # Scorecards ----
    s1 <- value_box(
      title = "Avg USD Salary",
      value = main_summary_tbl()$avg_usd_salary %>% scales::dollar(),
      case_when(
        main_summary_tbl()$avg_usd_salary < 55000 ~ "Low Pay",
        main_summary_tbl()$avg_usd_salary < 100000 ~ "Descent Pay",
        TRUE ~ "Amazing Pay"
      ),
      showcase = icon("sack-dollar"),
      class = case_when(
        main_summary_tbl()$avg_usd_salary < 55000 ~ "bg-danger",
        main_summary_tbl()$avg_usd_salary < 100000 ~ "bg-warning text-light",
        TRUE ~ "bg-success text-light"
      )
    )
    
    s2 <- value_box(
      title = "Sample Size",
      value = main_summary_tbl()$total_employees %>% scales::number(big.mark = ","),
      case_when(
        main_summary_tbl()$total_employees < 100 ~ "Low Sample Size",
        main_summary_tbl()$total_employees < 200 ~ "Descent Sample Size",
        TRUE ~ "Good Sample Size"
      ),
      showcase = icon("users"),
      class = case_when(
        main_summary_tbl()$total_employees < 100 ~ "bg-danger",
        main_summary_tbl()$total_employees <200 ~ "bg-warning text-light",
        TRUE ~ "bg-success text-light"
      )
    )
    
    s3 <- value_box(
      title = "Remote Ratio",
      value = main_summary_tbl()$remote_ratio %>% scales::percent(),
      case_when(
        main_summary_tbl()$remote_ratio < 0.2 ~ "Low Remote Ratio",
        main_summary_tbl()$remote_ratio < 0.5 ~ "Descent Remote Ratio",
        TRUE ~ "High Remote Ratio"
      ),
      showcase = icon("house"),
      class = case_when(
        main_summary_tbl()$remote_ratio < 0.2 ~ "bg-danger",
        main_summary_tbl()$remote_ratio < 0.5 ~ "bg-warning text-light",
        TRUE ~ "bg-success text-light"
      )
    )
    
    card_grid(
      s1, s2, s3,
      card_width = 1/3
    )
})  
    
    # Relationship Plot
    output$plot_1 <- renderPlot(expr = {
      main_filtered_tbl() %>% 
        ggplot(aes(x = remote_status, y = salary_in_usd, fill = remote_status)) + 
        ggdist::stat_halfeye(
          adjust = .5, 
          width = .6, 
          .width = 0, 
          justification = -.3, 
          point_colour = NA) + 
        geom_boxplot(
          width = .25, 
          outlier.shape = NA
        ) +
        geom_point(
          size = 1.3,
          alpha = .3,
          color = "#E31A1C",
          position = position_jitter(
            seed = 1, width = .1
          )
        ) + 
        coord_cartesian(xlim = c(1.2, NA), clip = "off") + 
        scale_fill_manual(values = theme_base_palette) + theme_tufte() + coord_flip() +
        scale_y_continuous(labels = scales::dollar) + 
        labs(
          title = "Salary by Remote Status",
          y = "",
          x = ""
        ) + 
        theme(legend.position = "none",
              plot.title = element_text(size = 26),
              text = element_text(size = 20))

    })
    
    # Year Occurred Observe Event
    
    # Table 1
    output$table_1 <- render_gt(
      expr = main_filtered_tbl() %>% 
        select(job_title, remote_ratio, salary_in_usd) %>% 
        group_by(job_title) %>% 
        summarise(
          `Salary (USD)`   = mean(salary_in_usd),
          `Remote Ratio` = mean(remote_ratio, na.rm = TRUE)
          
        ) %>% 
        ungroup() %>% 
        arrange(desc(`Salary (USD)`)) %>% 
        head(10) %>% 
        mutate(
          Rank = row_number()
        ) %>% 
        select(Rank, job_title, `Salary (USD)`, `Remote Ratio`) %>% 
        gt(rowname_col = "job_title") %>% 
        tab_header(
          title = md("Summary of Metrics by Job Title for the past **three** years"),
          subtitle = md("Click **Submit** after selecting specific year to see results")
        ) %>% 
        opt_align_table_header(align = "left") %>% 
        cols_label(
          `Remote Ratio` = md("Remote <br>Ratio"),
          `Salary (USD)` = md("Salary <br> (USD)")
        ) %>% 
        fmt_currency(
          columns = `Salary (USD)`
        ) %>% 
        fmt_percent(
          columns = `Remote Ratio`,
          scale_values = FALSE
        ) %>% 
        cols_width(
          `Remote Ratio` ~ px(120),
          `Salary (USD)` ~ px(120)
        ) %>% 
        tab_footnote(
          footnote = md("Top 10 **Highest Paid** Job Titles")
        ) %>% 
        tab_options(
          table.width = pct(100),
          container.height = px(800),
          heading.title.font.size = "large",
          heading.subtitle.font.size = "large",
          table.font.size = px(10L),
          table.font.names = "Courier Prime",
          footnotes.font.size = "normal"
        ) %>% 
        gt_theme_nytimes() %>% 
        gt_color_rows(`Salary (USD)`, palette = "ggsci::grey_material")
    )
    
    
    output$plotly_2 <- renderPlotly(expr = {
     
       main_regression_tbl() %>% 
       plot_salary_predictions(interactive=TRUE) %>% 
        layout(legend = list(orientation = "h"))
      
    })
    
    output$table_2 <- render_gt(expr = {
      
      main_regression_tbl() %>% 
        filter(status == "Prediction") %>% 
        gt_tbl() %>% 
        select(name, value) %>% 
        mutate(
          ID = row_number()
        ) %>% 
        select(ID, everything()) %>% 
        gt(rowname_col = "ID") %>% 
        tab_style(
          style = list(
            cell_fill(color = "#FF6647")
          ),
          locations = cells_body(
            columns = c(name, value), # not needed if coloring all columns
            rows = 4)
        ) %>% 
        tab_header(
          title = md("**Expected Salary** based on Inputs"),
          subtitle = md("For the years 2020 - 2022")
        ) %>% 
        opt_align_table_header(align = "left") %>% 
        cols_label(
          name = md("User <br> Features"),
          value = md("User <br> Inputs")
        ) %>% 
        tab_footnote(
          footnote = md("Salary based on **6 User Inputs**")
        ) %>% 
        tab_options(
          table.width = pct(100),
          container.height = px(800),
          heading.title.font.size = "large",
          heading.subtitle.font.size = "large",
          table.font.size = px(10L),
          table.font.names = "Courier Prime",
          footnotes.font.size = "normal"
        ) %>% 
        gt_theme_nytimes() 
    })
    

}
# Run the application 
shinyApp(ui = ui, server = server)
