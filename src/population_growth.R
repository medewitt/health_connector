# Purpose: Gather data and combine from public sources

# libraries ---------------------------------------------------------------


library(tidyverse)
library(readxl)
library(tidycensus)
library(modelr)
library(brms)
library(rvest)
library(ggrepel)
# read data in ------------------------------------------------------------

population_file <- list.files("data", pattern = "^countygrowth", full.names = TRUE)

population_file<-population_file[!grepl(pattern = "(2005)|(2006)|(2007)|(2008)", 
                                        x = population_file)]

# Helper function to extract and rename
read_population <- function(df){
  county <- read_excel(df, skip = 3) %>% 
    setNames(tolower(names(.))) %>% 
    setNames(make.names(names(.))) %>% 
    setNames(gsub(pattern = "\\.", replacement = "_", x = names(.))) %>% 
    mutate(estimate_year = str_extract(string = df, pattern = "\\d{4}"))
  names(county)[[2]]<-"base"
  names(county)[[3]]<-"estimate"
  names(county)[[4]]<-"delta"
  county
}

# Read in all the excel files
all_population <- map(.x = population_file, .f = read_population)

# Filter the junk files
all_population <- bind_rows(all_population) %>% 
  filter(!grepl(pattern = "(State)|(Source)", ignore.case = TRUE, x = .$county))

# Filter out the others
all_population %>% 
  group_by(county) %>% 
  arrange(county, estimate_year) %>% 
  mutate(my_delta = estimate - lag(estimate, n = 1L, default = 1)) %>% 
  filter(!estimate_year %in% c(2009, 2010))->all_population

# Look at summaries
county_pop_trends <- all_population %>% 
  group_by(county) %>% 
  summarise(avg_delta = mean(my_delta))

county_labels <- all_population %>% 
  filter(estimate_year == max(estimate_year)) %>% 
  ungroup() %>% 
  top_n(5,base)

population_growth_nc <-all_population %>% 
  mutate(my_color = ifelse(county == "Forsyth", "black", "grey"),
         my_color = as_factor(my_color)) %>%
  ggplot(aes(estimate_year, my_delta, group = county, color = my_color))+
  geom_line(alpha = .25)+
  scale_color_manual(values = c("blue", "grey"))+
  theme_minimal()+
  theme(legend.position = "none", panel.grid = element_blank())+
  geom_label_repel(data = county_labels, aes(estimate_year, my_delta, label = county), color = "black")+
  labs(
    title = "Population Growth Each Year",
    caption = "Based on NC NC OSBM Population Estimates",
    y = "Population Growth Year Over Year",
    x = NULL
  )

forsyth_growth_nc <-all_population %>% 
  filter(county == "Forsyth") %>% 
  mutate(my_color = ifelse(county == "Forsyth", "black", "grey"),
         my_color = as_factor(my_color)) %>%
  ggplot(aes(estimate_year, my_delta, group = county, color = my_color))+
  geom_line(alpha = 1)+
  scale_color_manual(values = c("blue", "grey"))+
  theme_minimal()+
  theme(legend.position = "none", panel.grid = element_blank())+
  labs(
    title = "Forsyth Population Growth Each Year",
    caption = "Based on NC NC OSBM Population Estimates",
    y = "Population Growth Year Over Year",
    x = NULL
  )

library(patchwork)
population_growth_nc + forsyth_growth_nc + ggsave("nc_population_dynamics.pdf")


# extra analysis ----------------------------------------------------------

all_population %>% 
  filter(county == "Forsyth") %>% 
  summarise(avg = mean(my_delta),
            min = min(my_delta),
            max = max(my_delta),
            n())

medicaid_format %>% 
  ungroup() %>% 
  filter(county_name == "Forsyth") %>% 
  filter(county_total>0) %>% 
  select(county_name, my_date, county_total) %>% 
  arrange(my_date) %>% 
  mutate(my_delta = county_total - lag(county_total, n = 1, default = 1)) %>% 
  mutate(my_year = lubridate::year(my_date))%>% 
  group_by(my_year) %>% 
  filter(my_delta < 10000) %>% 
  summarise(yearly = sum(my_delta)) %>% 
  mutate(avg = mean(yearly))
