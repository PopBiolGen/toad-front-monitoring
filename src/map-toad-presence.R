# After running a-load-data.R, this will map the point data and output a .shp for import into GIS.

library(dplyr)
library(sf)
library(ggplot2)
library(ozmaps)
library(ggspatial)

toad_pal    <- c("Present" = "#d73027", "Absent" = "#1a9850")
year_shapes <- c("2022" = 18, "2023" = 16, "2024" = 17, "2025" = 15, "2026" = 8)

# combine all survey datasets into a single set of point observations
obs_all <- bind_rows(
  df        |> st_transform(crs = 4326) |> mutate(dataset = "Main")   |> select(year, toad.present, dataset),
  df.clulow |> st_transform(crs = 4326) |> mutate(dataset = "Clulow") |> select(year, toad.present, dataset),
  df.hrc    |> st_transform(crs = 4326) |> mutate(dataset = "HRC")    |> select(year, toad.present, dataset)
) |>
  filter(!is.na(toad.present)) |>
  mutate(
    year     = as.integer(year),
    presence = ifelse(toad.present == 1, "Present", "Absent")
  ) |>
  select(year, presence, dataset)

# Static map -----------------------------------------------------------------
wa_border <- ozmap_states |> filter(NAME == "Western Australia")
bbox <- st_bbox(obs_all)

p_static <- ggplot() +
  geom_sf(data = wa_border, fill = "grey92", colour = "grey60", linewidth = 0.3) +
  geom_sf(data = obs_all, aes(colour = presence, shape = factor(year)), size = 2.2) +
  scale_colour_manual(values = toad_pal, name = "Toad presence") +
  scale_shape_manual(values = year_shapes, name = "Year") +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = TRUE) +
  annotation_scale(location = "bl", unit_category = "metric") +
  labs(x = "Longitude", y = "Latitude") +
  theme_bw()

print(p_static)
ggsave("out/map-toad-presence.pdf", plot = p_static, width = 180, height = 150, units = "mm")

# Export point data ------------------------------------------------------------
st_write(obs_all, "out/all-observations.shp", append = FALSE)
