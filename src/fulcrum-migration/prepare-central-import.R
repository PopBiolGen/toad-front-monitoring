# Prepares historical survey data for import into ODK Central as `toad_monitoring_sites`
# entities + `toad_detection_survey` submissions, per the procedure in
# ../../../toad-monitoring-app/FULCRUM_MIGRATION.md.
#
# Input: `df`, built by src/a-load-data.R (Fulcrum 2025 nocturnal/interview surveys merged with
# the Ben & Tim 2023-4 reconnaissance data). This script does NOT talk to ODK Central -- it only
# writes files for a human to feed into Central's UI:
#
#   out/fulcrum-migration/sites_for_central_upload.csv  -- one row per reconstructed site, for
#                                                           Central's dataset "Upload CSV"
#   out/fulcrum-migration/site_clusters_for_review.csv  -- every source record with its assigned
#                                                           site_id + clustering method, for the
#                                                           manual review the migration doc calls
#                                                           for before trusting the clustering
#   out/fulcrum-migration/submissions_manifest.csv      -- one row per survey visit, human-readable
#   out/fulcrum-migration/submissions/<uuid>.xml         -- one OpenRosa submission per visit,
#                                                           matching buildSubmissionXml() in
#                                                           toad-monitoring-app.html
#
# `df`'s exact column set depends on whatever Fulcrum happened to export (see
# FULCRUM_MIGRATION.md's "isn't a single mechanical step" section) -- fields that a-load-data.R
# doesn't already guarantee (site name, waterpoint type, water origin, native title area, "can
# toads access water", notes) are looked up by pattern rather than hardcoded, and left blank with
# a warning if nothing matches. Re-run and re-check the warnings once real values are in `df`.

source("src/a-load-data.R") # builds `df`
library(tidyverse)
library(sf)
library(uuid)

stopifnot(exists("df"), inherits(df, "sf"))

out_dir <- "out/fulcrum-migration"
submissions_dir <- file.path(out_dir, "submissions")
if (!dir.exists(submissions_dir)) dir.create(submissions_dir, recursive = TRUE)

# how close two un-named records' coordinates need to be (metres) to be treated as the same site
# -- tune to your sites' actual spacing, see FULCRUM_MIGRATION.md
PROXIMITY_THRESHOLD_M <- 75

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
# FULCRUM_MIGRATION.md assumed this field likely wouldn't exist in Fulcrum -- it does (as free
# text, e.g. "Yaruwu"), so it's usable for native_title_area rather than a pure back-fill job.
nta_col           <- pick_col(df, c("traditional.?owner", "native.?title"))

# ---------- reconstruct sites from visit records ----------

coords <- st_coordinates(st_geometry(df))
raw_name <- col_or_na(df, site_name_col)
norm_name <- raw_name |> str_squish() |> str_to_lower()
norm_name[norm_name %in% c("", "na", "n/a", "none")] <- NA

n <- nrow(df)
site_cluster <- rep(NA_integer_, n)
cluster_method <- rep(NA_character_, n)

# 1. records that share a (non-blank) site name are the same site, regardless of distance
named_idx <- which(!is.na(norm_name))
if (length(named_idx) > 0) {
  name_groups <- split(named_idx, norm_name[named_idx])
  for (g in seq_along(name_groups)) {
    site_cluster[name_groups[[g]]] <- g
    cluster_method[name_groups[[g]]] <- "site name match"
  }
}
next_id <- if (length(named_idx) > 0) max(site_cluster, na.rm = TRUE) + 1L else 1L

# 2. un-named records are clustered by GPS proximity (connected components within
#    PROXIMITY_THRESHOLD_M of each other, via a simple union-find over the neighbour list)
unnamed_idx <- which(is.na(norm_name))
if (length(unnamed_idx) > 0) {
  pts <- st_geometry(df)[unnamed_idx]
  near <- st_is_within_distance(pts, pts, dist = PROXIMITY_THRESHOLD_M)

  parent <- seq_along(unnamed_idx)
  uf_find <- function(x) { while (parent[x] != x) x <- parent[x]; x }
  for (i in seq_along(near)) {
    for (j in near[[i]]) {
      if (j == i) next
      ri <- uf_find(i); rj <- uf_find(j)
      if (ri != rj) parent[max(ri, rj)] <- min(ri, rj)
    }
  }
  comp <- vapply(seq_along(unnamed_idx), uf_find, integer(1))
  comp_id <- match(comp, unique(comp))
  site_cluster[unnamed_idx] <- next_id - 1L + comp_id
  cluster_method[unnamed_idx] <- "GPS proximity"
}

stopifnot(!anyNA(site_cluster))

n_sites <- length(unique(site_cluster))
message(n, " records reconstructed into ", n_sites, " sites (",
        sum(cluster_method == "site name match"), " by name match, ",
        sum(cluster_method == "GPS proximity"), " by proximity within ",
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
    cluster_method   = cluster_method,
    lon              = coords[, 1],
    lat              = coords[, 2],
    raw_site_name    = raw_name,
    wp_type_raw      = col_or_na(df, wp_type_col),
    wp_type_other_raw = col_or_na(df, wp_type_other_col),
    water_origin_raw = col_or_na(df, water_origin_col),
    access_water_raw = col_or_na(df, access_water_col),
    notes_raw        = col_or_na(df, notes_col),
    nta_raw          = col_or_na(df, nta_col)
  ) |>
  left_join(site_ids, by = "cluster")

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
# traditional_owner_country (free text) -> nta_choices mnemonic. Cross-check against
# ../../../shared-taxonomy/taxonomy/native_title_areas.csv if that list changes.
nta_map <- c(
  "yawuru" = "NTA-YWR", "yaruwu" = "NTA-YWR", # "Yaruwu" observed in this export -- likely a misspelling of Yawuru, confirm before uploading
  "karajarri" = "NTA-KJ",
  "nyangumarta" = "NTA-NY",
  "nyamal" = "NTA-NML"
)
map_nta <- function(raw) {
  if (is.na(raw) || raw == "") return(NA_character_)
  low <- str_to_lower(raw)
  if (str_detect(low, "nyangumarta") && str_detect(low, "karajarri")) return("NTA-NYKJ")
  for (pat in names(nta_map)) if (str_detect(low, pat)) return(nta_map[[pat]])
  message("unrecognised traditional_owner_country value '", raw,
          "' -- leaving native_title_area blank, check against ",
          "../../../shared-taxonomy/taxonomy/native_title_areas.csv")
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
    # mapped from traditional_owner_country where recognised; NA for sites with no such record
    # (most of them) still need back-filling from ../../../shared-taxonomy/taxonomy/
    # native_title_areas.csv / property_nta_overlap.csv per FULCRUM_MIGRATION.md
    native_title_area = map_nta(first_nonNA(nta_raw)),
    n_records = n(),
    .groups = "drop"
  ) |>
  mutate(label = coalesce(site_name, paste0(latitude, ", ", longitude))) |>
  relocate(label, .before = 1)

# na = "" so Central's CSV upload sees an empty property rather than the literal text "NA"
write_csv(sites_out, file.path(out_dir, "sites_for_central_upload.csv"), na = "")

write_csv(
  df_mig |> select(row_id, site_id, cluster_method, raw_site_name, lat, lon, date, any_cane_t),
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
  <site_name></site_name>
  <site_lat></site_lat>
  <site_lon></site_lon>
  <waterpoint_type></waterpoint_type>
  <waterpoint_type_other></waterpoint_type_other>
  <water_origin></water_origin>
  <native_title_area></native_title_area>
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
