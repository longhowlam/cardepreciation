####################################################################### 
#
# Create some extra variables based on the scraped car text

library(dplyr)
library(stringr)
library(lubridate)

### scraping results are in two files that I combine to one file in this script
### I scraped the site from 0% to 50% and simultanausly from 100% tot 50%

AllCars1 = readRDS("D:\\Algemene_R_Projecten\\Autotrader2\\ResultsGP.Rds")
AllCars2 = readRDS("D:\\Algemene_R_Projecten\\Autotrader2\\ResultsGP2.Rds")

AllCars = bind_rows(AllCars1, AllCars2)

### The interesting car information needs to be pulled out from the scraped texts

### Price van de auto
AllCars$tmp = str_extract( AllCars$prijs, "\\s[:digit:]+[\\.]*[:digit:]*")
AllCars$Prijs = AllCars$tmp %>%
  str_replace_all("\\.","") %>%
  as.numeric()

### Kilometers 
AllCars$tmp = str_extract(AllCars$bjkm, "[:digit:]*[\\.]*[:digit:]*\\skm")
AllCars$KM = AllCars$tmp %>% 
  str_replace("km","") %>% 
  str_replace_all("\\.","") %>%
  as.numeric()

### Fuel type
AllCars$Brandstof = ifelse(
  !is.na(str_extract(AllCars$info, "Benzine")),
  "Benzine",
  ifelse(
    !is.na(str_extract(AllCars$info, "Diesel")),
    "Diesel",
    ifelse(
      !is.na(str_extract(AllCars$info, "LPG")),
      "LPG",
      "Anders"  
    )
  )
)


### Age
AllCars$tmp =  str_extract(AllCars$bjkm, "[:digit:]{2}[\\-][:digit:]{4}") 
zz = str_split_fixed(AllCars$tmp, "[\\-]",2)
AllCars$bouwjaar = as.numeric(zz[,2])
AllCars$bouwmaand = zz[,1]


AllCars$tmp =  str_extract(AllCars$bjkm, "Bouwjaar[\\:][\\s]*[:digit:]{4}") 
AllCars$bouwjaar= ifelse(
  is.na(AllCars$bouwjaar),
  as.numeric(str_extract(AllCars$tmp, "[:digit:]{4}")) ,
  AllCars$bouwjaar
)


AllCars = AllCars %>%
  mutate(
    maand = as.numeric( bouwmaand),
    maand = ifelse(is.na(maand),6,maand),
    Bouwdatum = make_datetime(year = bouwjaar, month = maand),
    OuderdomMaanden = floor(as.integer((make_datetime(2016,10,1) - Bouwdatum)/ (365*24*60*60/12)))
  )


### Transmission
AllCars$Transmissie =  ifelse(
  !is.na(str_extract(AllCars$info, "Automaat")),
  "Automaat",
  "Handgeschakeld"
)


### Make (Merk) and Model
AllCars$auto = str_replace_all(AllCars$auto, "\r","") %>% str_replace_all("\n","") %>% str_trim()
zz = str_split_fixed(AllCars$auto, "[\\s]+",4)
AllCars$Merk  = zz[,1]
AllCars$Model = zz[,2]


### Engine Displacement CC (Motor inhoud) 
AllCars$Motor = str_extract(AllCars$info, "[:digit:]*[\\.]*[:digit:]+[\\s]*[c][c]") %>% 
  str_replace("[c][c]", "") %>% str_replace("\\.","") %>% as.numeric()

AllCars$tmp = NULL

saveRDS(AllCars, "AllCarsGasPedaal.Rds")






write.csv(AllCars, file="Cars_NL_GP.csv", row.names = FALSE )
