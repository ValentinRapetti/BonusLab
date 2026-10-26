library(ggplot2)
library(nycflights13)
library(dplyr)


visualize_airport_delays <- function(){
  data(flights)
  data(airports)

  output <- flights |> 
              dplyr::rename(faa=dest) |>
              dplyr::group_by(faa) |>
              dplyr::summarize(avg_delay = mean(arr_delay, na.rm=TRUE)) |>
              dplyr::left_join(airports, by="faa") |>
              dplyr::select(avg_delay, lat, lon) |>
              dplyr::filter_out(
                  dplyr::when_any( is.na(lat), is.na(lon), is.na(avg_delay) ))


  lambda <- 1


  # ----------------------------------------
  #   Regression and plot for the Latitude
  # ----------------------------------------
  formula <- avg_delay ~ lat
  lat_reg <- ridgereg(formula, output, lambda=lambda)

  df <- data.frame(
    latitude = output$lat,
    predictions = lat_reg$predicted
  )

  avg_df <- data.frame(
    latitude = output$lat,
    avg_delay = output$avg_delay
  )

  names(df) <- c("latitude", "predictions")
  names(avg_df) <- c("latitude", "avg_delay")

  g1 <- ggplot2::ggplot(data = avg_df, aes(x = latitude, y = avg_delay)) +
    geom_point() +
    geom_line(data = df, aes(x = latitude, y = predictions), color = "#00b9e7",linewidth = 1) +
    labs(
      title = "Avg arrival delay vs airport's latitude",
      x = "Latitude",
      y = "Delay"
    ) +
    theme_minimal()

  ggplot2::ggsave("DelayVsLat.png", plot = g1, width = 6, height = 4)


  # -----------------------------------------
  #   Regression and plot for the Longitude
  # -----------------------------------------

  formula <- avg_delay ~ lon
  lon_reg <- ridgereg(formula, output, lambda=lambda)

  df <- data.frame(
    latitude = output$lon,
    predictions = lon_reg$predicted
  )

  avg_df <- data.frame(
    latitude = output$lon,
    avg_delay = output$avg_delay
  )

  names(df) <- c("longitude", "predictions")
  names(avg_df) <- c("longitude", "avg_delay")

  g2 <- ggplot2::ggplot(data = avg_df, aes(x = longitude, y = avg_delay)) +
    geom_point() +
    geom_line(data = df, aes(x = longitude, y = predictions), color = "#00b9e7",linewidth = 1) +
    labs(
      title = "Avg arrival delay vs airport's longitude",
      x = "Longitude",
      y = "Delay"
    ) +
    theme_minimal()

  ggplot2::ggsave("DelayVsLon.png", plot = g2, width = 6, height = 4)

}