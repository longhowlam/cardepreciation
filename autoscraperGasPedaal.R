library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)
library(splines)
library(forcats)
library(lubridate)

#### scraping the website www.gaspedeaal.nl with used cars ###########

baseURL = "http://www.gaspedaal.nl/zoeken.html?srt=df&p="

for( i in 1:4336)
{
  linkie = paste0(baseURL,i)
  out = read_html(linkie)
  print(i)
  print(linkie)
  
  ### building year and amount of kilometers
  bjkm = html_nodes(out, xpath = '//div[@class="occ_bouwjaar_kmstand2b"]') %>%
    html_text() %>%
    str_replace_all("\n","") %>%
    str_replace_all("\r","") %>%
    str_trim()
  
  prijs = html_nodes(out, xpath = '//div[@class="occ_price2b"]') %>%
    html_text() %>%
    str_replace_all("\n","") %>%
    str_replace_all("\r","") %>%
    str_trim()
  
  auto = html_nodes(out, xpath = '//div[@class="occ_cartitle2b"]') %>%
    html_text() %>%
    str_replace_all("\n","") %>%
    str_replace_all("\r","") %>%
    str_trim()
  
  info = html_nodes(out, xpath = '//div[@class="occ_extrainfo2b"]') %>%
    html_text() %>%
    str_replace_all("\n","") %>%
    str_replace_all("\r","") %>%
    str_trim()
  
  place = html_nodes(out, xpath = '//div[@class="occ_place"]') %>%
    html_text() %>%
    str_replace_all("\n","") %>%
    str_replace_all("\r","") %>%
    str_trim()
  
  tmp = data.frame(auto, place, bjkm, prijs, info)
  
  ResultsGP = bind_rows(ResultsGP, tmp)
  
  if(i%%20 == 0)
  {
    saveRDS(ResultsGP, "ResultsGP.Rds")
    saveRDS(i,"i.Rds")
  }
  print(dim(ResultsGP))
}
