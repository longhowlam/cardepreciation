library(dplyr)
library(ggplot2)
library(nlme)
library(splines)
library(purrr)
library(broom)
library(tidyr)
library(plotly)
library(forcats)


###############################################################################################
##  analysis of car data

AllCars = readRDS( "AllCarsGasPedaal.Rds")

### simple counts of make and model to get a feeling for the number of cars per make

AllCounts = AllCars %>%
  group_by(Merk, Model) %>% 
  summarise(n=n()) %>%
  arrange(desc(n))
 

####### Plot of age vs KM #####################

### remove some outliers

FilteredCars = AllCars %>% filter(
  Prijs < 150000,
  KM < 300000,
  OuderdomMaanden < 200,
  Merk %in% c("BMW", "Volkswagen", "Audi", "Renault", "Opel", "Citroen", "Mercedes-Benz", "Peugeot", "Jaguar", "Porsche")
)

ggplot(FilteredCars, aes(OuderdomMaanden, KM, color = Merk)) +
  geom_smooth(method = "lm", formula = y ~ ns(x,7), se = FALSE, span=0.5, size=1.25) +  
  scale_y_continuous(breaks = seq(0,300000,by=20000), limits = c(0,300000)) + 
  scale_x_continuous(breaks = seq(0, 90, by=12), limits = c(0,90)) + 
  xlab("Age of car (months)") + 
  ylab("Kilometers driven")


################ Modeling the depreciation of cars #####################

### remove some outliers 
FilteredCars = AllCars %>% filter(
  Prijs < 150000,
  KM < 300000
)

### Take only car models with 'enough' observations
Counts = FilteredCars %>% 
  group_by(Merk, Model , Transmissie) %>% 
  summarise(n=n()) %>% 
  filter(n > 300)

SelectedCars = Counts %>% left_join(FilteredCars)

########## using a data frame of data frames
by_model <- SelectedCars %>% 
  group_by(Merk, Model, Transmissie) %>% 
  nest()

########## fit classical linear regression model by group

by_modelLM <- by_model %>% 
  mutate(
    model = purrr::map(data, ~ lm(Prijs ~ KM, data = .))
  )

### Extract intercept and slope from models
model_resultsLMDeprSlope = by_modelLM %>%
  unnest(model %>% purrr::map(broom::tidy)) %>%
  filter(term == "KM") %>%
  rename(depreciation = estimate)

model_resultsLMDeprIntercept = by_modelLM %>%
  unnest(model %>% purrr::map(broom::tidy)) %>%
  filter(term == "(Intercept)") %>%
rename(Intercept = estimate)


model_LinearDepreciation = model_resultsLMDeprIntercept %>% 
  left_join(model_resultsLMDeprSlope,  by = c("Merk", "Model", "Transmissie")) %>%
  select(Merk, Model, Transmissie, Intercept, depreciation)


################## fit better regression models 

## use natural cubic spline
by_modelNS <- by_model %>% 
  mutate(
    model = purrr::map(data, ~ lm(Prijs ~ ns(KM,4), data = .))
  )

## use additional variable Age (OuderdomMaanden)
by_modelNS2 <- by_model %>% 
  mutate(
    model = purrr::map(data, ~ lm(Prijs ~ ns(KM,4) + ns(OuderdomMaanden,4), data = .))
  )

## Try an exponential model
by_modelExp <- by_model %>% 
  mutate(
    model = purrr::map(data, ~ lm( log(Prijs) ~ KM, data = .))
  )

head(by_modelLM,n=20)
head(by_modelExp)


#### extract r-squared of the models
model_resultsLM = by_modelLM %>%
  unnest(model %>% purrr::map(broom::glance)) %>%
  rename(adj.r.squaredLM = adj.r.squared)

model_resultsNS = by_modelNS %>% 
  unnest(model %>% purrr::map(broom::glance)) %>%
  rename(adj.r.squaredNS = adj.r.squared)

model_resultsEXP = by_modelExp %>% 
  unnest(model %>% purrr::map(broom::glance)) %>%
  rename(adj.r.squaredEXP = adj.r.squared)

model_resultsNS2 = by_modelNS2 %>% 
  unnest(model %>% purrr::map(broom::glance)) %>%
  rename(adj.r.squaredNS2 = adj.r.squared)


#### predict price of the different cars at 0 kilometers 0 age and at 50.000 Kilometers and 24 months
model_resultsPred = by_modelNS2 %>%
  unnest( 
    PriceAt0KKM = model %>% 
      purrr::map(predict, newdata = data.frame(KM = 0, OuderdomMaanden = 0)),
    PriceAt50KKM = model %>%
      purrr::map(predict, newdata = data.frame(KM = 50000, OuderdomMaanden = 24))
  )

### calculate loss of value at 50.000 km 
model_results = model_resultsLM %>%
  left_join(model_resultsNS,   by = c("Merk", "Model", "Transmissie")) %>%
  left_join(model_resultsEXP,  by = c("Merk", "Model", "Transmissie"))  %>%
  left_join(model_resultsNS2,  by = c("Merk", "Model", "Transmissie")) %>%
  left_join(model_resultsPred, by = c("Merk", "Model", "Transmissie")) %>%
  select(
    Merk, Model, Transmissie, adj.r.squaredLM, adj.r.squaredNS, adj.r.squaredEXP, adj.r.squaredNS2,
    PriceAt0KKM, PriceAt50KKM
  ) %>%
  mutate(
    deprec50K =  1 - PriceAt50KKM / PriceAt0KKM
  )





###################################################################################

## Look at one specific car
SpecificCars = AllCars %>% filter(
  Merk %in% c("Renault"),
  
  Model %in% c("911", "Q7", "Clio"),
  Prijs < 30000,
  KM < 300000
)


ggplot(SpecificCars, aes(KM, Prijs, color = Transmissie)) +
  geom_point(alpha=.1) +
  geom_smooth(method = "lm", formula = y ~ ns(x,7), se = FALSE, span=0.75, size=1.8) +
  facet_grid(~Merk+Model)


############################################################################

model_resultsLMDepr = by_modelLM %>%
  unnest(model %>% purrr::map(broom::tidy)) %>%
  filter(term == "KM")


### linear model per Model can be created with lmList from the nlme package 
carDeprec = lmList(object = Prijs ~ KM | Model , data = SelectedCars, na.action = na.omit)
carDeprec[[1]]


saveRDS(SelectedCars,"SelectedCars.Rds")



