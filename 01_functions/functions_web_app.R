remote_status_fnc <-
function(data){
  
  main_tbl <- data %>% 
    mutate(
      remote_status = case_when(
        remote_ratio == 0 ~ "Work from Office",
        remote_ratio == 50 ~ "Hybrid Work",
        TRUE ~ "Work from Home"
      ),
      company_size = case_when(
        company_size == "L" ~ "Large",
        company_size == "M" ~ "Medium",
        TRUE ~ "Small"
      ),
      experience_level = case_when(
        experience_level == "EN" ~ "Entry Level",
        experience_level == "MI" ~ "Junior Mid-Level",
        experience_level == "SE" ~ "Senior Level",
        TRUE ~ "Executive Level"
      ),
      employment_type = case_when(
        employment_type == "PT" ~ "Part-Time",
        employment_type == "CT" ~ "Contract",
        employment_type == "FL" ~ "Freelance",
        TRUE ~ "Full-Time"
      )
    )
  
  return(main_tbl)
  
}
filtering_year_tbl <-
  function(data, year_date) {
    
    
    if(year_date == "All"){
      output_tbl <- data
    } else {
      output_tbl <- data %>% 
        filter(work_year == year_date)
    }
    
    return(output_tbl)
  }
clean_tbl <- function(data){
  
  clean_tbl <- data %>% 
    select(-c(...1, remote_ratio, salary)) %>% 
    select(salary_in_usd, everything()) %>% 
    mutate(
      work_year = work_year %>% as.character()
    ) %>% 
    select(salary_in_usd, work_year, experience_level, employment_type, job_title, 
           remote_status, company_size, company_location)
  
  return(clean_tbl)
  
  
}
split_to_train <- function(data){
  
  # Split tables
  sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.80,0.20))
  train_tbl  <- data[sample, ]
  test_tbl   <- data[!sample, ]
  
  return(train_tbl)
  
  
}


split_to_test <- function(data){
  
  # Split tables
  sample <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.80,0.20))
  train_tbl  <- data[sample, ]
  test_tbl   <- data[!sample, ]
  
  return(test_tbl)
  
  
}
generate_salary_prediction <- function(company_location,
                                       employment_type,
                                       experience_level,
                                       company_size,
                                       remote_status,
                                       job_title,
                                       .ml_model,
                                       seed = 123
){
  
  set.seed(seed)
  
  # Add features to a table
  
  pred_features_tbl <- tibble(
    company_location   = company_location,
    employment_type    = employment_type,
    experience_level   = experience_level,
    company_size       = company_size,
    remote_status      = remote_status,
    job_title          = job_title
  )
  
  # Preditcions
  pred_features_tbl %>%
    predict(.ml_model, new_data = .) %>% 
    bind_cols(pred_features_tbl) %>% 
    rename(salary_in_usd = .pred )
  
  
  
}
gt_tbl <-
  function(pred_tbl){
    
    # Formatted table
    
    long_pred_tbl <-  pred_tbl %>% 
      rename(
        "Experience Level" = "experience_level",
        "Employment Type"  = "employment_type",
        "Job Title"        = "job_title",
        "Location"         = "company_location",
        "Company Size"     = "company_size",
        "Remote Staus"     = "remote_status"
      ) %>% 
      mutate(
        salary_in_usd = scales::dollar(salary_in_usd)
      ) %>% 
      pivot_longer(1:7)
    
    long_pred_tbl$name[long_pred_tbl$name == "salary_in_usd"] <- "Salary ($USD)"
    
    return(long_pred_tbl)
    
    
  }
append_pred_tbl <-
function(employee_tbl, new_employee_tbl){
  
  final_tbl <- employee_tbl %>% 
    mutate(
      status = "Actual"
    ) %>% 
    bind_rows(
      new_employee_tbl %>% mutate(status = "Prediction")
    )
  
  
  return(final_tbl)
  
}
plot_salary_predictions <-
function(data, interactive = TRUE){
  
  g <- data %>% 
    mutate(
      job_title = fct_reorder(job_title, salary_in_usd),
      label_txt = str_glue("Salary: {scales::dollar(salary_in_usd)}
                         Experience Level: {experience_level}
                         Location: {company_location}
                         Size:     {company_size}")) %>% 
    ggplot(aes(x = job_title, y = salary_in_usd, color = status)) + geom_point() + coord_flip()  + 
    geom_jitter(aes(text = label_txt),
                width = .3,
                alpha = 0.3) + 
    geom_violin(alpha = 0.3) + 
    facet_wrap(~ remote_status) + 
    scale_y_continuous(labels = scales::dollar, breaks = seq(0, 400000, by = 200000)) + 
    scale_color_manual(values = c("#2C3E50", "#FF6647")) + 
    theme_tufte() + 
    theme(strip.text.x = element_text(margin = margin(5,5,5,5)),
          strip.background = element_rect(fill = "#333333"),
          strip.text       = element_text(color = "white", size = 14),
          legend.position = "bottom") + 
    labs(
      title = "", x     = "", y     = "Salary Scaled")
  
  
  if(interactive){
    return(ggplotly(g, tooltip = "text"))
  } else {
    return(g)
  }
  
}
transform_job_titles <- function(data){
  
  
  transformed_tbl <- data %>% 
    mutate(
      job_title = case_when(
        str_detect(str_to_lower(job_title), "data analyst") ~ "Data Analyst",
        str_detect(str_to_lower(job_title), "data analytics lead") ~ "Data Analyst",
        str_detect(str_to_lower(job_title), "data analytics engineer") ~ "Data Engineer",
        str_detect(str_to_lower(job_title), "data analytics manager") ~ "Data Analyst",
        str_detect(str_to_lower(job_title), "data scientist") ~ "Data Scientist",
        str_detect(str_to_lower(job_title), "data science") ~ "Data Scientist",
        str_detect(str_to_lower(job_title), "data engineer") ~ "Data Engineer",
        str_detect(str_to_lower(job_title), "machine learning") ~ "Machine Learning Engineer",
        str_detect(str_to_lower(job_title), "software") ~ "Software Engineer",
        str_detect(str_to_lower(job_title), "nlp") ~ "NLP Engineer",
        str_detect(str_to_lower(job_title), "research") ~ "Research Scientist",
        str_detect(str_to_lower(job_title), "researcher") ~ "Research Scientist",
        str_detect(str_to_lower(job_title), "architect") ~ "Data Architect",
        TRUE ~ job_title
      )
    )
  
  return(transformed_tbl)
  
  
}


plot_regression <- function(data){
  
  
  main_plot <- data %>% 
    mutate(
      job_title = fct_reorder(job_title, salary_in_usd),
      label_txt = str_glue("Salary: {scales::dollar(salary_in_usd)}
                         Experience Level: {experience_level}
                         Location: {company_location}
                         Size:     {company_size}")) %>% 
    ggplot(aes(x = job_title, y = salary_in_usd, color = status)) + geom_point() + coord_flip()  + 
    geom_jitter(aes(text = label_txt),
                width = .1,
                alpha = 0.3) + 
    geom_violin(alpha = 0.3) + 
    facet_wrap(~ remote_status) + 
    scale_y_continuous(labels = scales::dollar, breaks = seq(0, 400000, by = 200000)) + 
    scale_color_manual(values = c("#2C3E50", "#FF6647")) + 
    theme_minimal() + 
    theme(strip.text.x = element_text(margin = margin(5,5,5,5)),
          strip.background = element_rect(fill = "#333333"),
          strip.text       = element_text(color = "white"),
          legend.position = "bottom") + 
    labs(
      title = "", x     = "", y     = "Salary Scaled")
  
  plotly_plot <- ggplotly(main_plot, tooltip = "text")
  
  
  return(plotly_plot)
}
