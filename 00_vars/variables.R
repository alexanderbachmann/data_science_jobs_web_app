library(tidyverse)

source("01_functions/functions_web_app.R")

countries_lst <- c("US", "GB", "CA", "DE", "IN", "FR", "ES", "GR", "JP", "AT")
job_lst <- c("Data Scientist", "Big Data Engineer", "Data Analyst", "Machine Learning Engineer",
             "Research Scientist", "Data Science Manager", "Data Architect", "Big Data Engineer",
             "Machine Learning Scientist", "AI Scientist")

original_data <-  read_csv("00_data/ds_salaries.csv")

data <- original_data %>% 
  remote_status_fnc() %>% 
  filter(company_location %in% countries_lst) %>% 
  mutate(work_year = work_year %>% as.character()) %>% 
  select(-c(...1, salary))


regression_data <- original_data %>% 
  remote_status_fnc() %>% 
  filter(company_location %in% countries_lst) %>% 
  # filter(job_title %in% job_lst)
  transform_job_titles() %>% 
  select(-c(...1, salary, remote_ratio,  salary, work_year, salary_currency, employee_residence))