# Spatially Autocoreelated error modeling
library(sp)
library (sf)
library(mgcv)
library (brms)
library(maptools)

nc <- sf::st_read(system.file("shape/nc.shp", package = "sf"))

nc_2 <- nc %>% filter(NAME != "McDowell")

spare_mtx <- st_intersects(nc, nc)

w <- matrix (0, nrow=nrow (nc), ncol=nrow (nc))

for (i in 1:nrow (w)) {
  for (j in spare_mtx[[i]]) {
    w[i, j] <- 1
    w[j, i] <- 1
  }
}

rownames (w) <- str_to_title(nc$NAME)

poverty_hospitals %>% filter(my_date=="2018-08-01") %>% 
  select(county_total, july_2016_estimate, estimate, births, n_hospitals, county_name) %>% 
  filter(county_name != "Mcdowell")-> poverty_august


b1 <- brm (county_total~ july_2016_estimate + s(estimate) + births + (1| n_hospitals), 
           data=poverty_august, 
           family=gaussian (), iter=30000, warmup=30000/2, thin = 10,
           autocor=cor_car (w, ~ 1 | county_name), control=list (adapt_delta=0.999,
                                                                 max_treedepth = 15),
           cores = 3, chains = 3)

summary(b1)
plot(b1)
pairs(b1)
rownames(w) %in% poverty_august$county_name %>% sum()
f1 <- fitted(b1, probs = c(.1, .9)) %>% as_data_frame()

plot(f1$Estimate, poverty_august$county_total)
abline(0,1)


combined_df <- poverty_august %>% 
  bind_cols(f1) %>% 
  mutate(resids = estimate - county_total,
         ul = `Q10` - county_total,
         ll = `Q90` - county_total)


combined_df %>% 
  mutate(county_name = reorder(county_name, -resids)) %>% 
  arrange(-resids) %>% 
  mutate(index = row_number()) %>% 
  mutate(facet_number = case_when(
    index <= 30~ "Top 30",
    index >= 70~ "Bottom 30",
    TRUE~ "Middle 40"
  )) %>% 
  ggplot(aes(x = county_name, y = resids/1000))+
  geom_point()+
  geom_hline(yintercept = 0, color = "red")+
  geom_errorbar(aes(ymin = (resids - Est.Error)/1000, ymax = (resids + Est.Error)/1000))+
  coord_flip()+
  theme_minimal()+
  facet_wrap(~facet_number, nrow = 1, scales = "free_y")+
  labs(
    title = "There is an opportunity for 7.5k more people \n to receive benefits in Forsyth County",
    subtitle = "Regression used to predict Total Medicaid Rec. as a function of Population, Poverty and Birth Estimates",
    caption = "Data from: American Community Survey \nAug 2018 NCDHHS Enrollment Reports \n 2016 NC OSBM Population Estimates"
  )+
  xlab("")+
  ylab("Delta vs Prediction (1000s) \n(Positive = Greater Than Predicted, Negative = Less than Predicted)") 

