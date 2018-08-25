# Purpose: Gather data and combine from public sources

# libraries ---------------------------------------------------------------


library(tidyverse)
library(readxl)

# read data in ------------------------------------------------------------

county <- read_excel("data/countygrowth_cert_2016.xls", skip = 3) %>% 
  setNames(tolower(names(.))) %>% 
  setNames(make.names(names(.))) %>% 
  setNames(gsub(pattern = "\\.", replacement = "_", x = names(.)))


path <- "data/SFY_2019_Enrollment_Counts_by_County_and_Budget_Groups_0_2.xlsx"

path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)-> medicaid_data_raw

data_range <- names(medicaid_data_raw)

for(i in seq_along(medicaid_data_raw)){
  medicaid_data_raw[[i]]$month <-data_range[[i]] 
}

medicaid_data_raw %>% 
  bind_rows() %>% 
  setNames(tolower(names(.))) %>% 
  setNames(make.names(names(.))) %>% 
  setNames(gsub("\\.", "_", names(.))) %>% 
  mutate(year = str_extract(string = month, pattern = "\\d+"),
         month = str_remove(string = month, pattern = "\\d+"))-> medicaid_format
