# Diagnostic figures for reviewing the site-reconstruction clustering done in
# prepare-central-import.R (review.md point 3). Sourced from prepare-central-import.R right after
# clustering finishes -- expects `df` (nocturnal records, pre-clustering), `df_mig` (same records
# with `site_id`/`lon`/`lat`/`raw_site_name` attached), `out_dir` and `PROXIMITY_THRESHOLD_M` to
# already exist in the calling environment.

library(leaflet)
library(htmlwidgets)

# ---------- pairwise distance histogram ----------
# distances between every pair of raw records (pre-clustering) -- the gap between "same site,
# repeat visit" distances (small) and "different site" distances (larger) should show up as a
# valley here, which is what PROXIMITY_THRESHOLD_M should sit in
dist_mat <- st_distance(st_geometry(df))
pairwise_m <- as.numeric(dist_mat[lower.tri(dist_mat)])

p_dist_hist <- ggplot(data.frame(distance_m = pairwise_m), aes(x = distance_m)) +
  geom_histogram(binwidth = 25, boundary = 0) +
  geom_vline(xintercept = PROXIMITY_THRESHOLD_M, colour = "red", linetype = "dashed") +
  coord_cartesian(xlim = c(0, 1000)) +
  labs(x = "Pairwise distance between records (m)", y = "Count",
       title = "Pairwise record distances",
       subtitle = paste0("red dashed = current PROXIMITY_THRESHOLD_M (", PROXIMITY_THRESHOLD_M, "m)")) +
  theme_bw()
ggsave(file.path(out_dir, "pairwise-distance-histogram.pdf"), plot = p_dist_hist,
       width = 180, height = 120, units = "mm")

# ---------- leaflet cluster-check map ----------
# one marker per raw record, coloured by reconstructed site_id, with a line joining every record
# assigned to the same site so a mis-cluster (a line spanning two obviously distinct locations) is
# visible at a glance
cluster_pal <- colorFactor(palette = "viridis", domain = df_mig$site_id)

cluster_map <- leaflet(df_mig) |>
  addProviderTiles("Esri.WorldImagery") |>
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    color = ~cluster_pal(site_id),
    radius = 5, stroke = FALSE, fillOpacity = 0.9,
    popup = ~paste0("<b>site_id:</b> ", site_id,
                     "<br><b>raw site name:</b> ", coalesce(raw_site_name, "(none)"))
  )

multi_record_sites <- df_mig |> count(site_id) |> filter(n > 1) |> pull(site_id)
for (sid in multi_record_sites) {
  cluster_pts <- df_mig |> filter(site_id == sid)
  cluster_map <- addPolylines(cluster_map, lng = cluster_pts$lon, lat = cluster_pts$lat,
                               color = cluster_pal(sid), weight = 2, opacity = 0.7)
}

saveWidget(cluster_map, file = file.path(out_dir, "site-cluster-review-map.html"), selfcontained = FALSE)

message("Wrote diagnostic figures: ", file.path(out_dir, "pairwise-distance-histogram.pdf"), ", ",
        file.path(out_dir, "site-cluster-review-map.html"))
