# Import Wake Forest Baptist Financial Statements


# libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)

# import data -------------------------------------------------------------

df_1 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQ7WgYmAts46m1xQDLFpE3wLiDXzPi_1PKLosDQfFZPSlho_WHWpjg8oAoM88-Eo6lXjgHS_F2wmpdd/pub?gid=0&single=true&output=csv")

#fix names
df_2 <- df_1 %>% 
  setNames(make.names(names(.))) %>% 
  setNames(tolower(names(.))) %>% 
  setNames(str_replace_all(string = names(.), pattern = "\\.", "_")) %>% 
  mutate(date = mdy(date))

fix_rolling_agg <- function(df){
  df %>% 
    arrange(quarter) %>% 
    mutate(diff = rvus - lag(rvus, n = 1, default = 1)) %>% 
    select(quarter, rvus, diff)
}

# convert to quarterly rather than aggregated
df_3 <- df_2 %>% 
  group_by(fiscal_year) %>% 
  mutate(diff_rvus = rvus - lag(rvus, n = 1, default = 1)) %>% 
  mutate(diff_case_mix_adjusted_equivalent_discharges = 
           case_mix_adjusted_equivalent_discharges - lag(case_mix_adjusted_equivalent_discharges, n = 1, default = 1)) %>% 
  mutate(diff_patient_days = 
           patient_days - lag(patient_days, n = 1, default = 1)) %>% 
  mutate(diff_inpatient_admissions = 
           inpatient_admissions - lag(inpatient_admissions, n = 1, default = 1)) %>% 
  mutate(diff_inpatient_operating_room_cases = 
           inpatient_operating_room_cases- lag(inpatient_operating_room_cases, n = 1, default = 1)) %>% 
  mutate(diff_outpatient_operating_room_cases = 
           outpatient_operating_room_cases- lag(outpatient_operating_room_cases, n = 1, default = 1)) %>% 
  mutate(diff_total_operating_room_cases = 
           total_operating_room_cases- lag(total_operating_room_cases, n = 1, default = 1)) %>% 
  mutate(diff_emergency_department_visits = 
           emergency_department_visits- lag(emergency_department_visits, n = 1, default = 1))
  
df_3 %>% 
  ungroup() %>% 
  select(date, contains("diff_")) %>% 
  gather(parameter, value, -date) %>% 
  mutate(title_param = str_replace_all(string = parameter, pattern = "_", replacement = " ") %>% 
           str_to_title(.)) %>% 
  ggplot(aes(date, value))+
  geom_line() +
  geom_smooth(se = FALSE)+
  facet_wrap(~title_param, scales = "free_y")+
  theme_minimal()+
  labs(
    "Quarter by Quarter Reporting for WFUBMC",
    caption = "Data from https://www.wakehealth.edu/About-Us/Financial-Statement"
  ) -> quarterly_hospital

  ggsave(quarterly_hospital, "2018_08-Hospital_Numbers_Quarter_by_Quarter.pdf", 
         height = 8, width = 10, device = "pdf")
