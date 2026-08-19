# Prepares historical survey data for import into ODK Central as `toad_monitoring_sites`
# entities + `toad_detection_survey` submissions, per the procedure in
# ../../../toad-monitoring-app/FULCRUM_MIGRATION.md.
#
# Input: `df`, built by src/a-load-data.R (Fulcrum 2025 nocturnal/interview surveys merged with
# the Ben & Tim 2023-4 reconnaissance data). This script does NOT talk to ODK Central -- it only
# writes files for a human to feed into Central's UI:
#
#   out/fulcrum-migration/sites_for_central_upload.csv     -- one row per reconstructed site, for
#                                                              Central's dataset "Upload CSV"
#   out/fulcrum-migration/site_clusters_for_review.csv     -- every source record with its assigned
#                                                              site_id, for the manual review the
#                                                              migration doc calls for before
#                                                              trusting the clustering
#   out/fulcrum-migration/spatial_assignment_review.csv    -- per-site native_title_area/property
#                                                              spatial join result, cross-checked
#                                                              against ../shared-taxonomy -- flags
#                                                              new NTAs/properties/overlap rows to add
#   out/fulcrum-migration/column_mapping.csv               -- app field -> df column documentation
#   out/fulcrum-migration/pairwise-distance-histogram.pdf  -- diagnostic for PROXIMITY_THRESHOLD_M
#   out/fulcrum-migration/site-cluster-review-map.html     -- leaflet map, lines join clustered records
#   out/fulcrum-migration/submissions_manifest.csv         -- one row per survey visit, human-readable
#   out/fulcrum-migration/submissions/<uuid>.xml            -- one OpenRosa submission per visit,
#                                                              matching buildSubmissionXml() in
#                                                              toad-monitoring-app.html
#
# `df`'s exact column set depends on whatever Fulcrum happened to export (see
# FULCRUM_MIGRATION.md's "isn't a single mechanical step" section) -- fields that a-load-data.R
# doesn't already guarantee (site name, waterpoint type, water origin, "can toads access water",
# notes) are looked up by pattern rather than hardcoded, and left blank with a warning if nothing
# matches. Re-run and re-check the warnings once real values are in `df`. `native_title_area` and
# `property` are assigned via spatial joins against `nta.boundaries`/`pastoral.boundaries` (loaded
# in a-load-data.R), not from any `df` column -- see the "spatial assignment" section below. Sites
# are reconstructed from raw visit records by GPS proximity only (see PROXIMITY_THRESHOLD_M) --
# site name is recorded as an attribute but is not used to cluster records.

source("src/a-load-data.R") # builds `df`
library(tidyverse)
library(sf)
library(uuid)

stopifnot(exists("df"), inherits(df, "sf"))

# this migration is for nocturnal search-effort records only -- interview records (no search
# actually happened) don't fit the toad_detection_survey schema and aren't migrated
n_before_nocturnal_filter <- nrow(df)
df <- filter(df, survey_type == "nocturnal")
message("kept ", nrow(df), " of ", n_before_nocturnal_filter,
        " records after excluding non-nocturnal (interview) surveys.")

out_dir <- "out/fulcrum-migration"
submissions_dir <- file.path(out_dir, "submissions")
if (!dir.exists(submissions_dir)) dir.create(submissions_dir, recursive = TRUE)
# every submission gets a fresh instanceID/filename each run, so a stale re-run's XML files would
# otherwise never get cleaned up and could end up uploaded to Central alongside the current ones
unlink(list.files(submissions_dir, pattern = "\\.xml$", full.names = TRUE))

# how close two un-named records' coordinates need to be (metres) to be treated as the same site
# -- tune to your sites' actual spacing, see FULCRUM_MIGRATION.md
PROXIMITY_THRESHOLD_M <- 150

# ---------- column lookup for fields a-load-data.R doesn't guarantee ----------

# `df`'s columns depend on Fulcrum's export and aren't standardised by a-load-data.R for these
# fields, so search by pattern instead of hardcoding a name, and say what was (not) found.
pick_col <- function(data, patterns) {
  hit <- names(data)[Reduce(`|`, lapply(patterns, \(p) grepl(p, names(data), ignore.case = TRUE)))]
  if (length(hit) == 0) {
    message("no column matching {", paste(patterns, collapse = "|"), "} found in df -- leaving blank")
    return(NA_character_)
  }
  if (length(hit) > 1) {
    message("multiple columns match {", paste(patterns, collapse = "|"), "}: ",
            paste(hit, collapse = ", "), " -- using '", hit[1], "'")
  }
  hit[1]
}
col_or_na <- function(data, col) if (is.na(col)) rep(NA_character_, nrow(data)) else as.character(data[[col]])

site_name_col     <- pick_col(df, c("^location_n$", "site.?name", "location.?name"))
wp_type_col       <- pick_col(df, c("water.?point.?type", "type.?of.?water.?point"))
wp_type_other_col <- pick_col(df, c("water.?point.*other", "other.*water.?point"))
water_origin_col  <- pick_col(df, c("water.?origin", "natural.?or.?artificial", "natural.?artificial"))
access_water_col  <- pick_col(df, c("toads.*water", "access.*water"))
notes_col         <- pick_col(df, c("^notes$", "^note$", "comment"))

# ---------- column mapping documentation (review point 4) ----------

column_mapping <- tribble(
  ~app_field,                                  ~df_column,        ~source,
  "visit_date",                                "date",            "guaranteed by a-load-data.R",
  "visit_time",                                "hour, minute",    "guaranteed by a-load-data.R",
  "temperature_c",                             "temperature",     "guaranteed by a-load-data.R",
  "people_searching",                          "how_many_p",      "guaranteed by a-load-data.R",
  "time_searched_minutes",                     "search_time_mins","guaranteed by a-load-data.R",
  "toad_found",                                "any_cane_t",      "guaranteed by a-load-data.R",
  "site_name",                                 site_name_col,     "pattern-matched",
  "waterpoint_type",                           wp_type_col,       "pattern-matched",
  "waterpoint_type_other",                     wp_type_other_col, "pattern-matched",
  "water_origin",                              water_origin_col,  "pattern-matched",
  "toads_access_water",                        access_water_col,  "pattern-matched",
  "notes",                                     notes_col,         "pattern-matched",
  "native_title_area",                         NA_character_,     "spatial join against nta.boundaries, not a df column",
  "property",                                  NA_character_,     "spatial join against pastoral.boundaries, not a df column"
)
write_csv(column_mapping, file.path(out_dir, "column_mapping.csv"), na = "")

# ---------- reconstruct sites from visit records ----------

coords <- st_coordinates(st_geometry(df))
raw_name <- col_or_na(df, site_name_col) # kept as a site attribute/label -- not used to cluster

# records are clustered purely by GPS proximity: connected components within
# PROXIMITY_THRESHOLD_M of each other, via a simple union-find over the neighbour list. Matching
# by site name was dropped -- generic/reused names (e.g. "Dam") were merging records that were tens
# of km apart, which proximity-only clustering can't do.
n <- nrow(df)
pts <- st_geometry(df)
near <- st_is_within_distance(pts, pts, dist = PROXIMITY_THRESHOLD_M)

parent <- seq_len(n)
uf_find <- function(x) { while (parent[x] != x) x <- parent[x]; x }
for (i in seq_along(near)) {
  for (j in near[[i]]) {
    if (j == i) next
    ri <- uf_find(i); rj <- uf_find(j)
    if (ri != rj) parent[max(ri, rj)] <- min(ri, rj)
  }
}
comp <- vapply(seq_len(n), uf_find, integer(1))
site_cluster <- match(comp, unique(comp))

n_sites <- length(unique(site_cluster))
message(n, " records reconstructed into ", n_sites, " sites (GPS proximity within ",
        PROXIMITY_THRESHOLD_M, "m).")
message("This clustering is fuzzy by nature -- review site_clusters_for_review.csv before ",
        "trusting it, per FULCRUM_MIGRATION.md.")

site_ids <- tibble(cluster = sort(unique(site_cluster))) |>
  mutate(site_id = uuid::UUIDgenerate(n = n()))

df_mig <- df |>
  st_drop_geometry() |>
  mutate(
    row_id           = row_number(),
    cluster          = site_cluster,
    lon              = coords[, 1],
    lat              = coords[, 2],
    raw_site_name    = raw_name,
    wp_type_raw      = col_or_na(df, wp_type_col),
    wp_type_other_raw = col_or_na(df, wp_type_other_col),
    water_origin_raw = col_or_na(df, water_origin_col),
    access_water_raw = col_or_na(df, access_water_col),
    notes_raw        = col_or_na(df, notes_col)
  ) |>
  left_join(site_ids, by = "cluster")

# diagnostic figures for reviewing the clustering above (review point 3) -- uses df, df_mig,
# out_dir, PROXIMITY_THRESHOLD_M from this environment
source("src/fulcrum-migration/migration-review-plots.R")

# ---------- value mapping (Fulcrum label -> this app's slug), per FULCRUM_MIGRATION.md ----------

wp_type_map <- c(
  "creek" = "creek",
  "dam" = "dam_turkey_nest_impoundment", "turkey nest" = "dam_turkey_nest_impoundment",
  "impoundment" = "dam_turkey_nest_impoundment",
  "roadside scrape" = "roadside_scrape",
  "tank" = "tank_and_trough", "trough" = "tank_and_trough",
  "outlet pipe" = "outlet_pipe",
  "waterhole" = "waterhole",
  "soak" = "soak_or_spring", "spring" = "soak_or_spring"
)
map_wp_type <- function(raw) {
  # space-separated slugs (select_multiple) -- match each comma/semicolon-separated label found
  if (is.na(raw) || raw == "") return(NA_character_)
  parts <- str_split(raw, "[,;/]")[[1]] |> str_squish() |> str_to_lower()
  slugs <- map_chr(parts, function(p) {
    hit <- wp_type_map[map_lgl(names(wp_type_map), \(k) str_detect(p, fixed(k)))]
    if (length(hit) > 0) hit[1] else "other" # unrecognised label -- falls back to 'other'
  })
  paste(unique(slugs), collapse = " ")
}
map_water_origin <- function(raw) {
  if (is.na(raw) || raw == "") return(NA_character_)
  if (str_detect(str_to_lower(raw), "artif")) return("artificial")
  if (str_detect(str_to_lower(raw), "natur")) return("natural")
  NA_character_ # unrecognised -- leave blank rather than guess
}
map_yn <- function(raw) {
  if (is.na(raw) || raw == "") return(NA_character_)
  if (str_detect(str_to_lower(raw), "^y")) return("yes")
  if (str_detect(str_to_lower(raw), "^n")) return("no")
  NA_character_
}
first_nonNA <- function(x) {
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) NA_character_ else x[[1]]
}

# ---------- site catalogue (for Central's dataset "Upload CSV") ----------

sites_out <- df_mig |>
  group_by(site_id) |>
  summarise(
    latitude  = round(mean(lat), 6),
    longitude = round(mean(lon), 6),
    site_name = first_nonNA(raw_site_name),
    waterpoint_type = map_wp_type(first_nonNA(wp_type_raw)),
    waterpoint_type_other = NA_character_, # not distinguishable from waterpoint_type in Fulcrum export seen so far
    water_origin = map_water_origin(first_nonNA(water_origin_raw)),
    n_records = n(),
    .groups = "drop"
  ) |>
  mutate(label = coalesce(site_name, paste0(latitude, ", ", longitude))) |>
  relocate(label, .before = 1)

# ---------- spatial assignment of native_title_area + property (review point 2) ----------

# `native_title_area` and pastoral `property` are assigned by a direct spatial join against the
# two boundary layers loaded in a-load-data.R (nta.boundaries, pastoral.boundaries) -- this is the
# source of truth, independent of what shared-taxonomy currently has entries for. shared-taxonomy
# is only consulted afterward, as a cross-check, to flag where it needs new rows.
stopifnot(exists("nta.boundaries"), exists("pastoral.boundaries"))

sites_sf <- sites_out |>
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)

# curated crosswalk from nta.boundaries$determinat free text -> this project's NTA mnemonics.
# checked in priority order so the Nyangumarta-Karajarri overlap proceeding (whose determinat text
# also contains "NYANGUMARTA" and "KARAJARRI") is matched before the standalone patterns
nta_determinat_patterns <- c(
  "NYANGUMARTA-KARAJARRI" = "NTA-NYKJ", "OVERLAP" = "NTA-NYKJ",
  "YAWURU"                = "NTA-YWR",
  "KARAJARRI"             = "NTA-KJ",
  "NYANGUMARTA"           = "NTA-NY",
  "NYAMA"                 = "NTA-NML"
)
match_nta_id <- function(determinat) {
  if (is.na(determinat)) return(NA_character_)
  det <- str_to_upper(determinat)
  for (pat in names(nta_determinat_patterns)) {
    if (str_detect(det, pat)) return(nta_determinat_patterns[[pat]])
  }
  NA_character_ # doesn't match any of this project's 5 known NTAs -- kept, not dropped, so a
                 # site falling inside it still gets flagged below rather than silently blank
}
nta.boundaries.tagged <- nta.boundaries |>
  select(determinat) |>
  mutate(nta_id = map_chr(determinat, match_nta_id),
         nta_id = coalesce(nta_id, "UNRECOGNISED"))
# individual determination polygons are kept separate (not unioned into one polygon per nta_id) --
# unioning loses which specific determination a site actually fell inside, which is exactly what
# the "UNRECOGNISED" flag below needs to report, and st_union on this layer also introduces
# s2-invalid degenerate edges
nta_polygons <- nta.boundaries.tagged

NTA_PRIORITY <- c("NTA-NYKJ", "NTA-YWR", "NTA-KJ", "NTA-NY", "NTA-NML", "UNRECOGNISED")

nta_join <- st_join(sites_sf |> select(site_id), nta_polygons, join = st_within) |>
  st_drop_geometry() |>
  as_tibble() |>
  rename(determinat_examples = determinat)

# a site whose mean-of-visits centroid falls just outside every determination polygon (e.g. right
# on a boundary) gets one more try against the nearest polygon within a small tolerance
NTA_NEAREST_TOLERANCE_M <- 200
unmatched_site_id <- nta_join$site_id[is.na(nta_join$nta_id)]
if (length(unmatched_site_id) > 0) {
  unmatched_sf <- sites_sf[match(unmatched_site_id, sites_sf$site_id), ]
  nearest_idx <- st_nearest_feature(unmatched_sf, nta_polygons)
  nearest_dist <- as.numeric(st_distance(unmatched_sf, nta_polygons[nearest_idx, ], by_element = TRUE))
  close_enough <- nearest_dist <= NTA_NEAREST_TOLERANCE_M
  nta_join$nta_id[match(unmatched_site_id[close_enough], nta_join$site_id)] <-
    nta_polygons$nta_id[nearest_idx][close_enough]
  nta_join$determinat_examples[match(unmatched_site_id[close_enough], nta_join$site_id)] <-
    nta_polygons$determinat[nearest_idx][close_enough]
}

# a site can legitimately fall inside more than one determination polygon (that's what an overlap
# proceeding is) -- prefer NTA_PRIORITY order and note if more than one *distinct* nta_id matched
nta_resolved <- nta_join |>
  mutate(priority = match(nta_id, NTA_PRIORITY)) |>
  arrange(site_id, priority) |>
  group_by(site_id) |>
  summarise(
    nta_id = first_nonNA(nta_id),
    nta_determinat_examples = first_nonNA(determinat_examples),
    nta_n_distinct_matches = n_distinct(nta_id[!is.na(nta_id)]),
    .groups = "drop"
  )

pastoral_join <- st_join(sites_sf |> select(site_id), pastoral.boundaries |> select(stn_name, pl_no),
                          join = st_within) |>
  st_drop_geometry() |>
  as_tibble() |>
  group_by(site_id) |>
  summarise(
    stn_name = first_nonNA(as.character(stn_name)),
    pl_no = first_nonNA(as.character(pl_no)),
    property_n_distinct_matches = n_distinct(stn_name[!is.na(stn_name)]),
    .groups = "drop"
  )

sites_out <- sites_out |>
  left_join(nta_resolved, by = "site_id") |>
  left_join(pastoral_join, by = "site_id") |>
  mutate(native_title_area = ifelse(nta_id == "UNRECOGNISED", NA_character_, nta_id))
  # `property` (the toad_monitoring_sites entity property, a property_id mnemonic like "PROP-YM")
  # is filled in below from spatial_review's already-computed property_id, once the
  # shared-taxonomy cross-check has run -- not assigned here, and deliberately not the raw
  # stn_name/str_to_title() text, since the app's form expects a property_id code, not a name.

# ---------- cross-check the spatial assignment against shared-taxonomy ----------

taxonomy_dir <- "../shared-taxonomy/taxonomy"
nta_taxonomy       <- read_csv(file.path(taxonomy_dir, "native_title_areas.csv"), show_col_types = FALSE)
properties_taxonomy <- read_csv(file.path(taxonomy_dir, "properties.csv"), show_col_types = FALSE)
overlap_taxonomy    <- read_csv(file.path(taxonomy_dir, "property_nta_overlap.csv"), show_col_types = FALSE)

property_name_col <- pick_col(properties_taxonomy, c("^name$", "station.?name", "property.?name"))
normalise_station <- function(x) x |> str_to_upper() |> str_remove("\\s+STATION$") |> str_squish()

properties_taxonomy <- properties_taxonomy |>
  mutate(.norm_name = normalise_station(.data[[property_name_col]]))

spatial_review <- sites_out |>
  st_drop_geometry() |>
  transmute(
    site_id, label, latitude, longitude,
    nta_id, nta_determinat_examples, nta_n_distinct_matches,
    stn_name, pl_no, property_n_distinct_matches
  ) |>
  mutate(
    property_id = properties_taxonomy$property_id[match(normalise_station(stn_name), properties_taxonomy$.norm_name)]
  )

spatial_review$flag <- pmap_chr(spatial_review, function(nta_id, property_id, nta_determinat_examples,
                                                          stn_name, pl_no, nta_n_distinct_matches,
                                                          property_n_distinct_matches, ...) {
  if (identical(nta_id, "UNRECOGNISED")) {
    return(paste0("site falls inside an NTA determination not in this script's crosswalk (",
                   nta_determinat_examples, ") - add a native_title_areas.csv row and extend ",
                   "nta_determinat_patterns if it's a real NTA"))
  }
  if (is.na(nta_id)) {
    return(paste0("site is outside every NTA determination polygon (beyond the ",
                   NTA_NEAREST_TOLERANCE_M, "m fallback tolerance) - native_title_area left blank, review manually"))
  }
  if (!is.na(nta_id) && !(nta_id %in% nta_taxonomy$nta_id)) {
    return(paste0("nta_id '", nta_id, "' matched by crosswalk but missing from native_title_areas.csv"))
  }
  if (!is.na(stn_name) && is.na(property_id)) {
    return(paste0("pastoral station '", stn_name, "' (", pl_no, ") not found in shared-taxonomy properties.csv - add it"))
  }
  if (!is.na(nta_id) && !identical(nta_id, "UNRECOGNISED") && !is.na(property_id)) {
    has_overlap_row <- any(overlap_taxonomy$property_id == property_id & overlap_taxonomy$nta_id == nta_id &
                              overlap_taxonomy$active)
    if (!has_overlap_row) {
      return(paste0("property_nta_overlap.csv has no active row for (", property_id, ", ", nta_id, ") - likely missing"))
    }
  }
  if (nta_n_distinct_matches > 1) return("site's centroid matched more than one NTA polygon - review")
  if (property_n_distinct_matches > 1) return("site's centroid matched more than one pastoral lease polygon - review")
  NA_character_
})

n_flagged <- sum(!is.na(spatial_review$flag))
message(n_flagged, " of ", nrow(spatial_review),
        " sites flagged in spatial_assignment_review.csv (new NTA/property, missing taxonomy ",
        "row, or a disagreement) -- review before treating native_title_area as final.")
write_csv(spatial_review, file.path(out_dir, "spatial_assignment_review.csv"), na = "")

# `property` is the property_id resolved above (NA if stn_name didn't match a shared-taxonomy
# row -- see the "not found in shared-taxonomy properties.csv" flag) -- left blank rather than
# falling back to the raw station name, same "leave blank rather than guess" convention as
# native_title_area's UNRECOGNISED/no-match cases.
#
# site_id/n_records stay on `sites_out` itself -- site_id is still needed below to join df_mig's
# submissions to their site's label, and n_records is handy for ad hoc debugging -- but neither is
# a real entity property, so neither belongs in what actually gets uploaded to Central (see below).
sites_out <- sites_out |>
  left_join(spatial_review |> select(site_id, property = property_id), by = "site_id") |>
  select(label, site_id, latitude, longitude, site_name, waterpoint_type, waterpoint_type_other,
         water_origin, native_title_area, property, n_records)

# Central's dataset "Upload CSV" (bulk-create entities) only accepts `label` plus one column per
# declared entity property, with headers matching -- and in the same order as --
# toad_detection_survey.xlsx's survey sheet declares them via save_to. It does not accept an ID
# column: Central assigns each entity's real UUID itself at creation (confirmed against a live
# Central 2026.x -- there's no way to supply your own via this upload path), and it doesn't know
# about n_records (this script's own bookkeeping, not a dataset property). Both get dropped here.
#
# The site_id above is only a *placeholder* used to build submissions/*.xml below, referencing an
# entity ID that doesn't exist in Central yet. After this CSV is uploaded, download the resulting
# entity list from Central's dataset page and run apply-entity-id-crosswalk.R, which matches it
# back to site_id_placeholder_map.csv (by label) and rewrites every submission to reference the
# real entity ID before you upload the submissions themselves. See FULCRUM_MIGRATION.md step 3-4.
write_csv(
  sites_out |> select(label, site_name, latitude, longitude, waterpoint_type,
                       waterpoint_type_other, water_origin, native_title_area, property),
  file.path(out_dir, "sites_for_central_upload.csv"),
  na = ""
)

# Internal-only crosswalk input for apply-entity-id-crosswalk.R -- NOT for uploading to Central.
write_csv(
  sites_out |> select(site_id, label, latitude, longitude),
  file.path(out_dir, "site_id_placeholder_map.csv"),
  na = ""
)

write_csv(
  df_mig |> select(row_id, site_id, raw_site_name, lat, lon, date, any_cane_t),
  file.path(out_dir, "site_clusters_for_review.csv"),
  na = ""
)

# ---------- submissions ----------

# Entities are pre-created in Central via the sites CSV above (FULCRUM_MIGRATION.md step 3), so
# every historical submission references an existing site rather than creating one.
submissions <- df_mig |>
  transmute(
    row_id,
    site_id,
    label = sites_out$label[match(site_id, sites_out$site_id)],
    site_name = sites_out$site_name[match(site_id, sites_out$site_id)],
    site_lat = sites_out$latitude[match(site_id, sites_out$site_id)],
    site_lon = sites_out$longitude[match(site_id, sites_out$site_id)],
    waterpoint_type = sites_out$waterpoint_type[match(site_id, sites_out$site_id)],
    waterpoint_type_other = sites_out$waterpoint_type_other[match(site_id, sites_out$site_id)],
    water_origin = sites_out$water_origin[match(site_id, sites_out$site_id)],
    native_title_area = sites_out$native_title_area[match(site_id, sites_out$site_id)],
    property = sites_out$property[match(site_id, sites_out$site_id)],
    is_new_site = "no",
    visit_date = format(date, "%Y-%m-%d"),
    visit_time = sprintf("%02d:%02d", hour, minute),
    temperature_c = temperature,
    toads_access_water = map_chr(access_water_raw, map_yn),
    people_searching = how_many_p,
    time_searched_minutes = round(search_time_mins),
    toad_found = any_cane_t,
    notes = notes_raw,
    instance_id = uuid::UUIDgenerate(n = n())
  )

missing_required <- submissions |>
  filter(is.na(visit_date) | is.na(people_searching) | is.na(time_searched_minutes) | is.na(toad_found))
if (nrow(missing_required) > 0) {
  message(nrow(missing_required), " of ", nrow(submissions),
          " records are missing a required field (visit_date/people_searching/",
          "time_searched_minutes/toad_found) -- see submissions_manifest.csv, row_id column, ",
          "before uploading.")
}

write_csv(submissions |> select(-label), file.path(out_dir, "submissions_manifest.csv"), na = "")

esc_xml <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  x |>
    str_replace_all(fixed("&"), "&amp;") |>
    str_replace_all(fixed("<"), "&lt;") |>
    str_replace_all(fixed(">"), "&gt;") |>
    str_replace_all(fixed('"'), "&quot;") |>
    str_replace_all(fixed("'"), "&apos;")
}

# Element order and the <meta><entity> shape are copied from buildSubmissionXml() in
# ../../../toad-monitoring-app/toad-monitoring-app.html -- keep the two in sync.
build_submission_xml <- function(rec) {
  glue::glue(
    '<?xml version="1.0" encoding="UTF-8"?>
<toad_detection_survey id="toad_detection_survey">
  <is_new_site>{esc_xml(rec$is_new_site)}</is_new_site>
  <site_id>{esc_xml(rec$site_id)}</site_id>
  <site_name>{esc_xml(rec$site_name)}</site_name>
  <site_lat>{ifelse(is.na(rec$site_lat), "", rec$site_lat)}</site_lat>
  <site_lon>{ifelse(is.na(rec$site_lon), "", rec$site_lon)}</site_lon>
  <waterpoint_type>{esc_xml(rec$waterpoint_type)}</waterpoint_type>
  <waterpoint_type_other>{esc_xml(rec$waterpoint_type_other)}</waterpoint_type_other>
  <water_origin>{esc_xml(rec$water_origin)}</water_origin>
  <native_title_area>{esc_xml(rec$native_title_area)}</native_title_area>
  <property>{esc_xml(rec$property)}</property>
  <visit_date>{esc_xml(rec$visit_date)}</visit_date>
  <visit_time>{esc_xml(rec$visit_time)}</visit_time>
  <temperature_c>{ifelse(is.na(rec$temperature_c), "", rec$temperature_c)}</temperature_c>
  <toads_access_water>{esc_xml(rec$toads_access_water)}</toads_access_water>
  <people_searching>{ifelse(is.na(rec$people_searching), "", rec$people_searching)}</people_searching>
  <time_searched_minutes>{ifelse(is.na(rec$time_searched_minutes), "", rec$time_searched_minutes)}</time_searched_minutes>
  <toad_found>{esc_xml(rec$toad_found)}</toad_found>
  <notes>{esc_xml(rec$notes)}</notes>
  <meta>
    <entity dataset="toad_monitoring_sites" create="false" update="false" baseVersion="" trunkVersion="" branchId="" id="{esc_xml(rec$site_id)}">
      <label>{esc_xml(rec$label)}</label>
    </entity>
    <instanceID>uuid:{rec$instance_id}</instanceID>
  </meta>
</toad_detection_survey>'
  )
}

for (i in seq_len(nrow(submissions))) {
  rec <- submissions[i, ]
  writeLines(build_submission_xml(rec), file.path(submissions_dir, paste0(rec$instance_id, ".xml")))
}

message("Wrote ", nrow(sites_out), " sites and ", nrow(submissions), " submissions to ", out_dir, "/")
message("Next: review site_clusters_for_review.csv, fill in native_title_area (and any other ",
        "blank fields flagged above) in sites_for_central_upload.csv, upload it via Central's ",
        "dataset 'Upload CSV', then upload the submissions/*.xml files once the entities exist. ",
        "See FULCRUM_MIGRATION.md for the full procedure.")
