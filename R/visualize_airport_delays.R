library(ggplot2)
library(nycflights13)
library(dplyr)





output <- flights |> rename(faa=dest) |>
  left_join(airports, by = "faa") |> 
  dplyr::select(arr_delay, lat, lon)|> 
  filter(!is.na(lat), !is.na(lon), !is.na(arr_delay))



#dayta <- flights  %>% left_join(airports, by="faa")

formula <- arr_delay ~ lat +lon
#r <- ridgereg(formula, output[1:40, ], 1)


#data<-dayta[,c()]

#visualize_airport_delays <- function(){
  
X<- model.matrix(formula, output)
y <- output[[all.vars(formula)[1]]]
X<- scale(X, center = TRUE, scale = TRUE)[,-1, drop = FALSE]