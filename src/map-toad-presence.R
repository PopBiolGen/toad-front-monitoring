library(leaflet)
library(dplyr)
library(sf)
library(tidyr)

toad_pal   <- c("0" = "#4575b4", "1" = "#d73027")
year_shape <- c("2022" = "diamond", "2023" = "circle", "2024" = "triangle", "2025" = "square")

make_svg_url <- function(shape, fill) {
  svg <- switch(shape,
    circle   = sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="18" height="18"><circle cx="9" cy="9" r="7" fill="%s" stroke="#333" stroke-width="1.5"/></svg>', fill),
    triangle = sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="18" height="18"><polygon points="9,1 17,17 1,17" fill="%s" stroke="#333" stroke-width="1.5"/></svg>', fill),
    square   = sprintf('<svg xmlns="http://www.w3.org/2000/svg" width="18" height="18"><rect x="1" y="1" width="16" height="16" fill="%s" stroke="#333" stroke-width="1.5"/></svg>', fill)
  )
  paste0("data:image/svg+xml,", utils::URLencode(svg, reserved = TRUE))
}

coords <- st_coordinates(df)
df_map <- df |>
  st_drop_geometry() |>
  mutate(
    lng        = coords[, 1],
    lat        = coords[, 2],
    shape      = replace_na(year_shape[as.character(year)], "circle"),
    toad_color = toad_pal[as.character(toad.present)],
    dataset    = "Main"
  )

clulow_coords <- st_coordinates(df.clulow)
df_map_clulow <- df.clulow |>
  st_drop_geometry() |>
  mutate(
    lng        = clulow_coords[, 1],
    lat        = clulow_coords[, 2],
    shape      = replace_na(year_shape[as.character(year)], "circle"),
    toad_color = replace_na(toad_pal[as.character(toad.present)], "#888888"),
    dataset    = "Clulow"
  )

df_map_all <- bind_rows(
  mutate(df_map,        time = as.character(time)),
  mutate(df_map_clulow, time = as.character(time))
)
svg_urls <- mapply(make_svg_url, df_map_all$shape, df_map_all$toad_color)

shape_legend_html <- '
<div style="background:white;padding:8px 10px;border-radius:4px;font-size:12px;line-height:1.8;">
  <b>Year</b><br>
  <svg width="14" height="14"><polygon points="7,1 13,7 7,13 1,7" fill="#aaa" stroke="#333" stroke-width="1.5"/></svg>&nbsp;2022 (Clulow)<br>
  <svg width="14" height="14"><circle cx="7" cy="7" r="6" fill="#aaa" stroke="#333" stroke-width="1.5"/></svg>&nbsp;2023<br>
  <svg width="14" height="14"><polygon points="7,1 13,13 1,13" fill="#aaa" stroke="#333" stroke-width="1.5"/></svg>&nbsp;2024<br>
  <svg width="14" height="14"><rect x="1" y="1" width="12" height="12" fill="#aaa" stroke="#333" stroke-width="1.5"/></svg>&nbsp;2025
</div>'

library(ggplot2)
library(ozmaps)
library(ggspatial)

# Static map ---------------------------------------------------------------
wa_border <- ozmap_states |> filter(NAME == "Western Australia")

# Expand bounding box to include both datasets
bbox_df <- st_bbox(df)
bbox_cl <- st_bbox(df.clulow)
bbox <- st_bbox(
  c(xmin = min(bbox_df["xmin"], bbox_cl["xmin"]),
    ymin = min(bbox_df["ymin"], bbox_cl["ymin"]),
    xmax = max(bbox_df["xmax"], bbox_cl["xmax"]),
    ymax = max(bbox_df["ymax"], bbox_cl["ymax"])),
  crs = st_crs(df)
)

df_static <- df |>
  mutate(
    year_f   = factor(ifelse(is.na(year), "Unknown", as.character(year))),
    toad_f   = factor(ifelse(toad.present == 1, "Present", "Absent"),
                      levels = c("Present", "Absent")),
    dataset  = "Main"
  )

df.clulow_static <- df.clulow |>
  mutate(
    year_f   = factor(as.character(year)),
    toad_f   = factor(
      case_when(toad.present == 1 ~ "Present",
                toad.present == 0 ~ "Absent",
                TRUE              ~ "Unknown"),
      levels = c("Present", "Absent", "Unknown")
    ),
    dataset  = "Clulow"
  )

p_static <- ggplot() +
  geom_sf(data = wa_border, fill = "grey92", colour = "grey60", linewidth = 0.3) +
  # Clulow data — slightly smaller, drawn first so main data sits on top
  geom_sf(data = df.clulow_static, aes(shape = year_f, colour = toad_f), size = 2.0, alpha = 0.8) +
  # Main (df) data
  geom_sf(data = df_static, aes(shape = year_f, colour = toad_f), size = 2.5) +
  scale_colour_manual(
    values = c("Present" = "#d73027", "Absent" = "#4575b4", "Unknown" = "#888888"),
    name   = "Toad presence"
  ) +
  scale_shape_manual(
    values = c("2022" = 18, "2023" = 16, "2024" = 17, "2025" = 15, "Unknown" = 1),
    name   = "Year"
  ) +
  coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
           ylim = c(bbox["ymin"], bbox["ymax"]),
           expand = TRUE) +
  annotation_scale(location = "bl", unit_category = "metric") +
  labs(x = "Longitude", y = "Latitude",
       caption = "Clulow data (2022-2024) shown slightly smaller") +
  theme_bw()

print(p_static)
ggsave("out/map-toad-presence.pdf", plot = p_static, width = 180, height = 150, units = "mm")

# Interactive map ----------------------------------------------------------
print(
  leaflet(df_map_all) |>
    addProviderTiles(providers$CartoDB.Positron) |>
    addMarkers(
      lng  = ~lng, lat = ~lat,
      icon = icons(
        iconUrl     = svg_urls,
        iconWidth   = 18, iconHeight  = 18,
        iconAnchorX = 9,  iconAnchorY = 9
      ),
      popup = ~paste0(
        "<b>Dataset:</b> ", dataset, "<br>",
        "<b>Year:</b> ", year, "<br>",
        "<b>Toad present:</b> ", ifelse(toad.present == 1, "Yes",
                                        ifelse(toad.present == 0, "No", "Unknown")), "<br>",
        "<b>Date:</b> ", date
      )
    ) |>
    addLegend("bottomright",
      colors  = c("#d73027", "#4575b4", "#888888"),
      labels  = c("Present", "Absent", "Unknown"),
      title   = "Toad presence",
      opacity = 1
    ) |>
    addControl(html = shape_legend_html, position = "bottomleft")
)
