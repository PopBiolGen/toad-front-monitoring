# A script to load the various data sources
library(tidyverse)
library(readxl)
library(sf)

df.recon <- read_excel(path = "dat/invasion-front-reconnaissance-data.xlsx", sheet = "recon_data")

df.recon <- df.recon |>
  mutate(hour = hour(time), minute = minute(time)) |>
  st_as_sf(coords = c("lon", "lat")) |>
  st_set_crs(value = 4283) |> # set crs (GDA94/GRS 1980)
  st_transform(crs = 3857) # transform to web mercator

st_write(df.recon, "out/recon_sites.kml")
