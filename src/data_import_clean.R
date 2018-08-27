# Purpose: Gather data and combine from public sources

# libraries ---------------------------------------------------------------


library(tidyverse)
library(readxl)
library(tidycensus)
library(modelr)
library(brms)
library(rvest)
# read data in ------------------------------------------------------------

county <- read_excel("data/countygrowth_cert_2016.xls", skip = 3) %>% 
  setNames(tolower(names(.))) %>% 
  setNames(make.names(names(.))) %>% 
  setNames(gsub(pattern = "\\.", replacement = "_", x = names(.)))


path <- "data/SFY_2019_Enrollment_Counts_by_County_and_Budget_Groups_0_2.xlsx"

path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)-> medicaid_data_raw_2019

path <- "data/SFY_2018_Enrollment_Counts_by_County_and_Budget_Groups.xlsx"

path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)-> medicaid_data_raw_2018

path <- "data/SFY_2017_Enrollment_Counts_by_County_and_Budget_Groups_0_2.xlsx"

path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)-> medicaid_data_raw_2017

path <- "data/SFY 2016_Monthly_Enrollment_Counts_by_County_and_Budget_Groups.xlsx"

path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)-> medicaid_data_raw_2016

path <- "data/SFY2015_Annual_Unduplicated_Enrollment_Counts_by_County_and_Budget_Groups.xlsx"

path %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map(read_excel, path = path)-> medicaid_data_raw_2015


medicaid_data_raw <- c(medicaid_data_raw_2018, medicaid_data_raw_2019, medicaid_data_raw_2017,
                       medicaid_data_raw_2016, medicaid_data_raw_2015)

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
         month = str_trim(str_remove(string = month, pattern = "\\d+")) )%>% 
  mutate(county_name = str_to_title(tolower(county_name))) %>% 
  filter(year != 2019)-> medicaid_format

medicaid_format %>% 
  mutate(my_date = lubridate::ymd(glue::glue("{year}-{month}-{1}"))) -> medicaid_format


# census data -------------------------------------------------------------

tidycensus::census_api_key(census_key)

county_pov <- get_acs(geography = "county",
                      state = "NC",
                      variables = "B17001_002",
                      summary_var = "B17001_001",
                      geometry = TRUE) %>% 
  mutate(pctpov = 100* estimate/ summary_est)

county_pov %>%
  mutate(my_county = str_replace(
    string = NAME,
    pattern = " County, North Carolina",
    replacement = ""
  ))->county_pov_2


# number of hospitals -----------------------------------------------------

my_url <- "https://en.wikipedia.org/wiki/List_of_hospitals_in_North_Carolina"

hospitals <- my_url %>%
  html_session() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table()

hospitals <- hospitals[[1]]

n_hospitals <- hospitals %>% 
  mutate(`Hospital beds` = str_remove(string = `Hospital beds`, pattern = "\\[\\d+\\]")) %>% 
  mutate(`Hospital beds` = as.numeric(`Hospital beds`)) %>% 
  mutate(County = ifelse(County =="" & City == "Jacksonville", "Onslow", County)) %>% 
  group_by(County) %>% 
  summarise(n_hospitals = n(),
            n_beds = sum(`Hospital beds`, na.rm = TRUE))
# join data ---------------------------------------------------------------


  
county_pov_2 %>% 
  as_data_frame() %>% 
  left_join(., county, by = c("my_county" = "county")) %>% 
  mutate(pov_percent = estimate/july_2016_estimate) %>% 
  select(my_county, july_2016_estimate, estimate, pov_percent, births, -geometry) %>% 
  as_data_frame()->poverty

medicaid_format %>% 
  filter(county_name != "Totals") %>% 
  filter(month ==  "AUG") %>% 
  left_join(poverty, by = c("county_name" = "my_county"))->poverty_county_1

poverty_county_1 %>% 
  left_join(n_hospitals, by = c("county_name" = "County")) %>% 
  mutate(n_hospitals = ifelse(is.na(n_hospitals),0, n_hospitals))->poverty_hospitals



# modeling ----------------------------------------------------------------


fit <- lm(county_total~ july_2016_estimate + estimate + births, data = poverty_hospitals)

fit_mle <- lme4::lmer(county_total~ july_2016_estimate + estimate + births + (1|n_hospitals), 
                 data = poverty_hospitals)

fit_bayes <- brm(county_total~ july_2016_estimate + estimate + births + (1|n_hospitals), 
                 data = poverty_hospitals,
                iter = 2000, chains = 3, cores = 3)
summary(fit)
summary(fit_bayes)
plot(marginal_effects(fit_bayes))

predict(fit, se.fit = T)$se.fit
library(modelr)
predict(fit_bayes, robust = TRUE)
poverty_county_1 %>% 
  filter(!is.na(births)) %>% 
  add_predictions(data = ., model = fit) %>%
  mutate(resids = (county_total - pred)/1000) %>% 
  add_column(error = predict(fit, se.fit = T)$se.fit/1000)->poverty_county_predictions

poverty_county_predictions %>% 
  mutate(county_name = reorder(county_name, -resids)) %>% 
  arrange(-resids) %>% 
  mutate(index = row_number()) %>% 
  mutate(facet_number = case_when(
    index <= 30~ "Top 30",
    index >= 70~ "Bottom 30",
    TRUE~ "Middle 40"
  )) %>% 
  ggplot(aes(x = county_name, y = resids))+
  geom_point()+
  geom_hline(yintercept = 0, color = "red")+
  geom_errorbar(aes(ymin = resids - error, ymax = resids + error))+
  coord_flip()+
  theme_minimal()+
  facet_wrap(~facet_number, nrow = 1, scales = "free_y")+
  labs(
    title = "There is an opportunity for 7.5k more people \n to receive benefits in Forsyth County",
    subtitle = "Regression used to predict Total Medicaid Rec. as a function of Population, Poverty and Birth Estimates",
    caption = "Data from: American Community Survey \nAug 2018 NCDHHS Enrollment Reports \n 2016 NC OSBM Population Estimates"
  )+
  xlab("")+
  ylab("Delta vs Prediction (1000s) \n(Positive = Greater Than Predicted, Negative = Less than Predicted)")+
  ggsave("2018-08_predicted_medicaid_nc.pdf", width = 11, height = 8)


# time series forecasting -------------------------------------------------
library(fpp2)

medicaid_format %>% 
  filter(county_name == "Forsyth") %>% 
  filter(county_total >0) %>% 
  arrange(my_date)->medicaid_forsyth

forsyth_ts <- ts(data = medicaid_forsyth$county_total, frequency = 12, start = c(2015,7) )
forsyth_ts

#Graph Time Series
autoplot(forsyth_ts)

# Seasonal Decomp for Forsyth
forsyth_ts %>% 
  decompose(type="additive") %>%
  autoplot() 

fc <- ses(forsyth_ts, h=8)

#Forecast
forsyth_ts %>% forecast() %>%
  autoplot() + ylab("Number of Medicaid Recipients")

#exponsential smoothing method
autoplot(fc) +
  autolayer(fitted(fc), series="Fitted") +
  ylab("Number of Medicaid Recipients") + xlab("Year")+
  theme_minimal()
