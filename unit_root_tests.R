rm(list = ls())
source('requirement.R')
options(max.print = 100000)
library(tidyverse)

# get data ----
COVID_data_statistic <- readRDS("data/COVID_data_statistic.rds")
COVID_data_quarter_d <- readRDS("data/COVID_data_quarter_d.rds") %>%
  filter(date >= as.Date("2012-01-01"))
COVID_data_quarter_l <- readRDS("data/COVID_data_quarter_l.rds") %>%
  filter(date >= as.Date("2012-01-01"))

COVID_data_montly_d <- readRDS("data/COVID_data_montly_d.rds")
COVID_data_montly_l <- readRDS("data/COVID_data_montly_l.rds")

# a bit of wrangling ----
series_names <- names(df)[!(names(df) %in% c("date", "D_COVID"))]

# results
unit_root_results_s <- COVID_data_statistic %>%
  dplyr::select(where(is.numeric)) %>%             
  dplyr::select(-D_COVID) %>%                      
  imap_dfr(~ {
    result <- unit_root_table(.x)
    result$Variable <- .y
    result
  }) %>%
  dplyr::select(Variable, everything())   

unit_root_results_qd <- COVID_data_quarter_d %>%
  dplyr::select(where(is.numeric)) %>%             
  dplyr::select(-D_COVID) %>%                      
  imap_dfr(~ {
    result <- unit_root_table(.x)
    result$Variable <- .y
    result
  }) %>%
  dplyr::select(Variable, everything()) 

unit_root_results_ql <- COVID_data_quarter_l %>%
  dplyr::select(where(is.numeric)) %>%             
  dplyr::select(-D_COVID) %>%                      
  imap_dfr(~ {
    result <- unit_root_table(.x)
    result$Variable <- .y
    result
  }) %>%
  dplyr::select(Variable, everything())      

unit_root_results_md <- COVID_data_montly_d %>%
  dplyr::select(where(is.numeric)) %>%             
  dplyr::select(-D_COVID) %>%                      
  imap_dfr(~ {
    result <- unit_root_table(.x)
    result$Variable <- .y
    result
  }) %>%
  dplyr::select(Variable, everything())     

unit_root_results_ml <- COVID_data_montly_l %>%
  dplyr::select(where(is.numeric)) %>%             
  dplyr::select(-D_COVID) %>%                      
  imap_dfr(~ {
    result <- unit_root_table(.x)
    result$Variable <- .y
    result
  }) %>%
  dplyr::select(Variable, everything())            

print(unit_root_results_md)
print(unit_root_results_ml)
print(unit_root_results_qd)
print(unit_root_results_ql)
print(unit_root_results_s)
