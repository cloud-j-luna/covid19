#
# These are helper functions for the Covid-19 Shiny App.
#
# However, these can be used autonomously for other purposes.
#

library(dplyr)
library(stringr)
library(ggplot2)
library(hrbrthemes)
library(scales)
library(viridis)
library(reshape2)
library(maps)

library(directlabels)

# Get World Shape Data for ggplot
world.map <- map_data("world")

covid_data <- function() {
  
  data <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
  
  #convert column to date format
  data[[1]] <- as.Date(data[[1]], "%d/%m/%Y")
  
  #drop columns 
  data <- data %>% select(-geoId, -continentExp, -countryterritoryCode)
  
  #reorder columns
  data <- data %>% select(countriesAndTerritories, everything())
  
  #rename columns
  data <- data %>% rename(country = countriesAndTerritories, population = popData2018)
  
  #drop underscores from country names
  data <- data %>% mutate(country = country %>% str_replace_all('_',' '))
  
  data <- data %>% mutate(country = recode(country,
                                           'Antigua and Barbuda' = 'Barbuda'  ,
                                           'Bonaire, Saint Eustatius and Saba' = 'Bonaire'  ,
                                           'British Virgin Islands' = 'Virgin Islands'  ,
                                           'Brunei Darussalam' = 'Brunei'  ,
                                           'Congo' = 'Republic of Congo'  ,
                                           'Cote dIvoire' = 'Ivory Coast'  ,
                                           'CuraÃ§ao' = 'Curacao'  ,
                                           'Czechia' = 'Czech Republic'  ,
                                           'Falkland Islands (Malvinas)' = 'Maldives'  ,
                                           'Guinea Bissau' = 'Guinea-Bissau'  ,
                                           'North Macedonia' = 'Macedonia'  ,
                                           'Saint Kitts and Nevis' = 'Nevis'  ,
                                           'Saint Vincent and the Grenadines' = 'Grenadines'  ,
                                           'Timor Leste' = 'Timor-Leste'  ,
                                           'Trinidad and Tobago' = 'Trinidad'  ,
                                           'Turks and Caicos islands' = 'Turks and Caicos Islands' , 
                                           'United Kingdom' = 'UK'  ,
                                           'United Republic of Tanzania' = 'Tanzania'  ,
                                           'United States of America' = 'USA'  ,
                                           'United States Virgin Islands' = 'Virgin Islands'   ))
  
} 

covid_totals <- function(data) {
  data %>% 
    group_by(country) %>% 
    summarise(cases = sum(cases), 
              deaths = sum(deaths), 
              population = max(population))
}

covid_filter <- function(data, country_list) {
  filter(data, country %in% country_list)
}

plot_covid_top10 <- function(covid_totals, order_by = c("cases", "deaths")) {
  order_by <- match.arg(order_by)
  
  data <- select(covid_totals, -population)
  
  if( order_by == "cases")
    data <- arrange(data, desc(cases))
  else
    data <- arrange(data, desc(deaths))
  
  data <- data[1:10,]
  
  country.ordered <- data$country
  
  data <- melt(data, id = "country")
  
  #reverse because of coord_flip :|
  data$country <- factor(data$country, levels = rev(country.ordered))
  
  ggplot(data, aes(fill=variable, y=value, x=country)) + 
    geom_bar(position="stack", stat="identity") +
    scale_fill_viridis(discrete = T, name = "Occurence") +
    theme_ipsum() +
    scale_y_continuous(labels = label_comma()) +
    xlab("") +
    ylab("Count") + 
    coord_flip() 
}

plot_covid_timeline_comparison <- function(data, country_list, type = c("Per day", "Cummulative")) {
  type <- match.arg(type)
  
  data <- covid_filter(data, country_list)
  
  lab_y <- "Cases per day"
  if( type == "Cummulative") {
    data <- data %>% group_by(country) %>% 
      arrange(dateRep) %>% 
      mutate(cases = cumsum(cases), deaths = cumsum(deaths)) %>%
      ungroup()
    lab_y <- "Cummulative cases"
  }
  
  p <- ggplot(data, aes(x=dateRep, y=cases, group=country, color=country)) +
    geom_line() + 
    scale_x_date(date_breaks = "1 week", date_labels =  "%b %d") +
    scale_y_continuous(labels = label_number()) + 
    scale_color_viridis(name="Country", discrete=T) +
    xlab("") +
    ylab(lab_y) + 
    theme_ipsum() +
    theme(
      plot.margin = unit(c(1,1,1,1), "pt") ,
      axis.text.x=element_text(angle=60, hjust=1)
    )
  
  p <- direct.label(p,list("last.qp", cex = 0.7))
  p
}

# This is a single bar plot, not used
# covid_plot_top10 <- function(totals) {
# 
#   data <- select(totals, -population)
# 
#   data <- arrange(data, desc(cases))
# 
#   data <- data[1:10, ]
# 
#   ggplot(data, aes(x=reorder(country, cases), y=cases)) +
#     geom_bar(stat="identity") +
#     scale_y_continuous(labels = label_number()) + # ou label_comma
#     xlab("") +
#     coord_flip() +
#     theme_ipsum() #quem deu erro, salta este comando
# 
# }

plot_covid_totals <- function(covid_totals, display = c("cases", "deaths")) {
  display <- match.arg(display)
  
  # #log breaks
  breaks <- c(0, 10,100,1000,10000,100000,1000000, 10000000)
  covid_totals$cases_group <- cut(covid_totals$cases, breaks = breaks, include.lowest = T, dig.lab = 8)
  
  breaks_deaths <- c(0, 10,100,1000,10000,100000, 1000000)
  covid_totals$deaths_group <- cut(covid_totals$deaths, breaks = breaks_deaths, include.lowest = T, dig.lab = 8)
  
  data.map <- left_join( world.map, covid_totals, by = c('region' = 'country')) 
  
  p <- ggplot(data.map, aes(x = long, y = lat, group = group)) +
    theme_ipsum() +
    theme(
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      plot.margin = unit(c(1,1,1,1), "pt")
    ) +
    xlab("") + 
    ylab("") 
  
  if(display=="cases")
    p + 
    geom_polygon(aes(fill=cases_group), colour = "black", size = 0.1) +
    #scale_fill_viridis(name = "Total cases", discrete = T) 
    scale_fill_brewer()
  else
    p + 
    geom_polygon(aes(fill=deaths_group), colour = "black", size = 0.1) +
    #scale_fill_viridis(name = "Total deaths", discrete = T) 
    scale_fill_brewer(palette = "OrRd")
}

covid_commulated <- function(data) {
  data %>% group_by(country) %>% 
    arrange(dateRep) %>% 
    mutate(cases = cumsum(cases), deaths = cumsum(deaths)) %>%
    ungroup()
}

plot_covid_totals_cummulative <- function(covid_data, date) {
  covid_commulated(covid_data)
  
  breaks <- c(0, 10,100,1000,10000,100000,1000000, 10000000)
  covid_totals$cases_group <- cut(covid_totals$cases, breaks = breaks, include.lowest = T, dig.lab = 8)
  
  data.map <- left_join( world.map, covid_commulated(covid_data()), by = c('region' = 'country')) 
  
  p <- ggplot(data.map, aes(x = long, y = lat, group = group)) +
    theme_ipsum() +
    theme(
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      plot.margin = unit(c(1,1,1,1), "pt")
    ) +
    xlab("") + 
    ylab("") 
  
  p + 
  geom_polygon(aes(fill=cases_group), colour = "black", size = 0.1) +
  scale_fill_viridis(name = "Total cases", discrete = T) 

  
}

plot_country_centroids <- function(covid_data, date) {
  
  totals <- covid_commulated(covid_data)
  
  country_centroids <- world.map %>% group_by(region) %>% summarize(long_center = mean(long), lat_center = mean(lat))
  
  country_centroids <- left_join( country_centroids, totals, by = c('region' = 'country')) %>% filter(dateRep == date)
  
  breaks <- c(10, 50, 100, 1000, 2500, 5000, 10000, 20000, 50000, 100000)
  ggplot() +
    geom_polygon(data = world.map, aes(x = long, y = lat, group = group), fill="grey", colour = "black", size = 0.1, alpha=0.3) + 
    geom_point( data = country_centroids, aes(x=long_center, y=lat_center, size=cases), color="red", alpha=0.3) +
    scale_size_binned(breaks = breaks)
  
}

plot_lethality_totals <- function(covid_totals, display = c("lethality", "mortality")) {
  display <- match.arg(display)
  
  # #log breaks
  breaks <- c(0, 5, 10, 20, 40, 60, 100)
  covid_totals$lethality_group <- cut(covid_totals$lethality, breaks = breaks, include.lowest = T, dig.lab = 8)
  
  breaks_mortality <- c(0, 2, 5, 10, 20, 40, 100)
  covid_totals$mortality_group <- cut(covid_totals$mortality, breaks = breaks_mortality, include.lowest = T, dig.lab = 8)
  
  
  data.map <- left_join( world.map, covid_totals, by = c('region' = 'country')) 
  
  p <- ggplot(data.map, aes(x = long, y = lat, group = group)) +
    theme_ipsum() +
    theme(
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      plot.margin = unit(c(1,1,1,1), "pt")
    ) +
    xlab("") + 
    ylab("") 
  
  if(display=="lethality")
    p + 
    geom_polygon(aes(fill=lethality_group), colour = "black", size = 0.1) +
    #scale_fill_viridis(name = "Total cases", discrete = T) 
    scale_fill_brewer()
  else
    p + 
    geom_polygon(aes(fill=mortality_group), colour = "black", size = 0.1) +
    #scale_fill_viridis(name = "Total deaths", discrete = T) 
    scale_fill_brewer(palette = "OrRd")
}
