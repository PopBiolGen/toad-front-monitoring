source("src/a-load-data.R")
library(rjags)
library(dplyr)
library(sf)
library(ggplot2)
library(maptiles)
library(tidyterra)

# 1. Data preparation -------------------------------------------------------

in.dat <- filter(df, water_available == "available" &
                   survey_type == "nocturnal")

years   <- sort(unique(in.dat$year))
n.years <- length(years)
year.idx <- match(in.dat$year, years)

# project to Albers, scale to km, centre on grand mean
proj.coords <- in.dat |>
  st_transform(crs = 3577) |>
  st_coordinates() / 1000  # metres -> km

mean.coord <- colMeans(proj.coords)
proj.coords.centred <- sweep(proj.coords, 2, mean.coord, FUN = "-") |>
  as.data.frame() |>
  rename(X.c = X, Y.c = Y)

in.dat <- cbind(in.dat, proj.coords, proj.coords.centred)

# 1b. Clulow data preparation ------------------------------------------------

clulow.clean <- df.clulow |>
  filter(!is.na(toad.present))

# Re-project to Albers, scale to km, centre on the SAME mean.coord as in.dat
clulow.coords <- clulow.clean |>
  st_transform(crs = 3577) |>
  st_coordinates() / 1000

clulow.coords.centred <- sweep(clulow.coords, 2, mean.coord, FUN = "-") |>
  as.data.frame() |>
  rename(X.c = X, Y.c = Y)

clulow.clean <- cbind(st_drop_geometry(clulow.clean), clulow.coords, clulow.coords.centred)

# Combined year vector (adds 2022 prior to 2023–2025 from in.dat)
years.all   <- sort(union(years, unique(clulow.clean$year)))
n.years.all <- length(years.all)

# Re-index all data to the expanded year vector
year.idx       <- match(in.dat$year, years.all)

# Separate count observations from PA-only observations
clulow.count <- filter(clulow.clean, !is.na(total.count))
clulow.pa    <- filter(clulow.clean,  is.na(total.count))

year.idx.count <- match(clulow.count$year, years.all)
year.idx.pa    <- match(clulow.pa$year,    years.all)

# 2. JAGS data and inits ----------------------------------------------------

data.list <- list(
  # TTD survey data
  ttd          = in.dat$p.m.positive,
  is.detected  = in.dat$toad.present,
  tmax.i       = in.dat$person.minutes,
  year.idx     = year.idx,
  n.years      = n.years.all,
  n.obs        = nrow(in.dat),
  x            = in.dat$X.c,
  y            = in.dat$Y.c,
  # Clulow count data (Poisson likelihood)
  n.obs.count  = nrow(clulow.count),
  count.c      = clulow.count$total.count,
  tmax.c       = clulow.count$person.minutes,
  x.c          = clulow.count$X.c,
  y.c          = clulow.count$Y.c,
  year.idx.c   = year.idx.count,
  # Clulow PA-only data (Bernoulli likelihood)
  n.obs.pa     = nrow(clulow.pa),
  is.det.pa    = clulow.pa$toad.present,
  tmax.pa      = clulow.pa$person.minutes,
  x.pa         = clulow.pa$X.c,
  y.pa         = clulow.pa$Y.c,
  year.idx.pa  = year.idx.pa
)

init.list <- list(
  lambda = 1/10,
  a      = 0,
  b      = rep(min(in.dat$Y.c) - 1, n.years.all)
)

# 3. Run JAGS ---------------------------------------------------------------

ttd.mod <- jags.model(
  file    = "src/model-files/time-to-detection-multi-year_JAGS.txt",
  data    = data.list,
  inits   = init.list,
  n.chains = 3
)
update(ttd.mod, n.iter = 5000) # burn in

ttd.samp <- coda.samples(
  ttd.mod,
  variable.names = c("lambda", "a", "b", "delta"),
  n.iter = 10000,
  thin   = 5
)

gelman.diag(ttd.samp)
(mod.multi <- summary(ttd.samp))
coda::densplot(ttd.samp)

save(mod.multi, mean.coord, years.all,
     file = file.path(Sys.getenv("DATA_PATH"), "invasion-front-parameters-multi-year.Rdata"))

# 4. Generate per-year predictions ------------------------------------------

ttd.samp.mat <- as.matrix(ttd.samp)
x.seq <- seq(from = min(c(in.dat$X.c, clulow.clean$X.c)),
             to   = max(c(in.dat$X.c, clulow.clean$X.c)),
             length.out = 100)

# helper: predictions for one year (X, Y in km), returned as sf with a year label
pred_to_sf <- function(X, Y, re.centre, yr) {
  data.frame(X = X, Y = Y) |>
    sweep(2, re.centre, FUN = "+") |>
    (\(d) d * 1000)() |>        # km -> metres for Albers CRS
    st_as_sf(coords = c("X", "Y"), crs = 3577) |>
    st_transform(crs = 4326) |>
    mutate(year = yr)
}

lines.list <- vector("list", n.years.all)
bands.list <- vector("list", n.years.all)

for (tt in seq_len(n.years.all)) {
  yr <- years.all[tt]
  b.col <- paste0("b[", tt, "]")

  out.mat <- matrix(NA, ncol = length(x.seq), nrow = nrow(ttd.samp.mat))
  for (ii in seq_len(nrow(ttd.samp.mat))) {
    out.mat[ii, ] <- ttd.samp.mat[ii, "a"] * x.seq + ttd.samp.mat[ii, b.col]
  }

  preds.yr <- apply(out.mat, 2, quantile, p = c(0.025, 0.5, 0.975)) |>
    t() |>
    as.data.frame() |>
    setNames(c("y_lower", "y", "y_upper")) |>
    mutate(x = x.seq)

  # median line as LINESTRING
  line_geom <- pred_to_sf(preds.yr$x, preds.yr$y, mean.coord, yr) |>
    st_geometry() |>
    st_combine() |>
    st_cast("LINESTRING")
  lines.list[[tt]] <- st_sf(year = as.numeric(yr), geometry = line_geom)

  # CI band as polygon
  upp_coords <- pred_to_sf(preds.yr$x, preds.yr$y_upper, mean.coord, yr) |> st_coordinates()
  low_coords  <- pred_to_sf(preds.yr$x, preds.yr$y_lower, mean.coord, yr) |> st_coordinates()
  band_geom <- st_sfc(
    st_polygon(list(rbind(upp_coords, low_coords[nrow(low_coords):1, ], upp_coords[1, ]))),
    crs = 4326
  )
  bands.list[[tt]] <- st_sf(year = as.numeric(yr), geometry = band_geom)
}

front.lines <- do.call(rbind, lines.list)
front.bands <- do.call(rbind, bands.list)

# 5. Combined map (all years, no faceting) ----------------------------------

points.p <- st_transform(in.dat, crs = 4326) |>
  mutate(year = years.all[year.idx])

points.clulow <- df.clulow |>
  filter(!is.na(toad.present))

# Bounding box expanded to cover both datasets
bbox_df <- st_bbox(points.p)
bbox_cl <- st_bbox(points.clulow)
bbox <- st_bbox(
  c(xmin = min(bbox_df["xmin"], bbox_cl["xmin"]),
    ymin = min(bbox_df["ymin"], bbox_cl["ymin"]),
    xmax = max(bbox_df["xmax"], bbox_cl["xmax"]),
    ymax = max(bbox_df["ymax"], bbox_cl["ymax"])),
  crs = st_crs(points.p)
)

sat.raster <- get_tiles(
  x        = st_as_sfc(bbox),
  provider = "Esri.WorldImagery",
  zoom     = 11
)
sat.raster <- terra::aggregate(sat.raster, fact = 3)

year_colors <- c("2022" = "#D95F02", "2023" = "#7570B3", "2024" = "#1B9E77", "2025" = "#E7298A")

print(
  ggplot() +
    geom_spatraster_rgb(data = sat.raster) +
    geom_sf(data = front.bands,
            aes(fill = factor(year)), colour = NA, alpha = 0.2) +
    geom_sf(data = front.lines,
            aes(colour = factor(year)), linetype = "dashed", linewidth = 0.8) +
    geom_sf(data = points.clulow,
            aes(colour = factor(year), shape = factor(toad.present)),
            size = 1.5) +
    geom_sf(data = points.p,
            aes(colour = factor(year), shape = factor(toad.present)),
            size = 1.8) +
    scale_colour_manual(values = year_colors, name = "Year") +
    scale_fill_manual(values = year_colors, name = "Year", guide = "none") +
    scale_shape_manual(
      values = c("0" = 1, "1" = 16),
      labels = c("0" = "Absent", "1" = "Present"),
      name   = "Toad presence"
    ) +
    coord_sf(xlim = c(bbox["xmin"], bbox["xmax"]),
             ylim = c(bbox["ymin"], bbox["ymax"]),
             expand = TRUE) +
    labs(x = "Longitude", y = "Latitude") +
    theme_bw() +
    theme(panel.grid = element_blank())
)

ggsave("out/multi-year-front.pdf", width = 200, height = 150, units = "mm")

# 6. Posterior of front movement between consecutive years ------------------

delta.df <- ttd.samp.mat[, grep("^delta", colnames(ttd.samp.mat)), drop = FALSE] |>
  as.data.frame() |>
  setNames(as.character(years.all[-n.years.all])) |>  # starting year of each interval
  tidyr::pivot_longer(everything(), names_to = "starting_year", values_to = "distance_km") |>
  dplyr::mutate(starting_year = as.integer(starting_year),
                distance_km   = abs(distance_km))

print(
  ggplot(delta.df, aes(x = factor(starting_year), y = distance_km)) +
    geom_violin(fill = "steelblue", alpha = 0.6, colour = NA) +
    geom_boxplot(width = 0.08, outlier.shape = NA) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "red") +
    labs(x = "Starting year",
         y = "Perpendicular front movement (km)") +
    theme_bw()
)

ggsave("out/front-movement-posteriors.pdf", width = 140, height = 120, units = "mm")
