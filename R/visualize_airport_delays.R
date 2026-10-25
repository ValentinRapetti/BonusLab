library(ggplot2)
library(nycflights13)
library(dplyr)

output <-flights |> rename(faa=dest) |>
  left_join(airports, by = "faa") |> 
  select(arr_delay, lat, lon)|> 
  filter(!is.na(lat), !is.na(lon))



dayta <- flights  %>% left_join(airports, by="faa")

formula <- arr_delay ~ lat 
r <- ridgereg(formula, output, 1)


data<-dayta[,c()]

visualize_airport_delays <- function(){
  
