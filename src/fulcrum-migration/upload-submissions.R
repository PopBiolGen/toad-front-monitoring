# Uploads out/fulcrum-migration/submissions/*.xml (already rewritten to reference real Central
# entity IDs by apply-entity-id-crosswalk.R -- run that first) to Central as historical
# toad_detection_survey submissions, via a web-user session against Central's REST API. See
# FULCRUM_MIGRATION.md step 4.
#
# Central's REST endpoint is keyed on the form's xmlFormId ("toad_detection_survey" -- set on
# toad_detection_survey.xlsx's `settings` sheet and mirrored as FORM_ID in
# toad-monitoring-app.html), NOT its display title ("Cane Toad Detection Survey") -- the two are
# different strings and only the xmlFormId goes in the URL.
#
# Required environment variables (set them in your shell before running -- don't hardcode
# credentials in this file, and don't commit them):
#   CENTRAL_URL         e.g. https://your-central.example.org (no trailing slash, no /v1)
#   CENTRAL_PROJECT_ID  the numeric project ID (visible in Central's URL bar when viewing the project)
#   CENTRAL_EMAIL       a web user with Data Manager (or Administrator) rights on this project
#   CENTRAL_PASSWORD    that web user's password
# Optional:
#   CENTRAL_UPLOAD_LIMIT  upload only the first N files -- try this against 1-2 records first,
#                         confirm they show up correctly against the right entity in Central's UI,
#                         before uploading the full batch.
#
# Usage:
#   CENTRAL_URL=https://central.example.org CENTRAL_PROJECT_ID=7 \
#     CENTRAL_EMAIL=you@org.org CENTRAL_PASSWORD=*** CENTRAL_UPLOAD_LIMIT=2 \
#     Rscript src/fulcrum-migration/upload-submissions.R
#
# Not verified against a live Central 2026.x (no test project was available while writing this --
# same caveat as everywhere else "not verified" appears in this migration's docs): the raw-XML
# POST /v1/projects/:id/forms/:xmlFormId/submissions shape below is Central's documented REST path
# for scripted submission creation, distinct from the OpenRosa multipart endpoint the app itself
# uses -- but test on a couple of records (CENTRAL_UPLOAD_LIMIT) before trusting it at scale.

library(tidyverse)
library(httr)

FORM_ID <- "toad_detection_survey" # xmlFormId -- see note above, not the display title
out_dir <- "out/fulcrum-migration"
submissions_dir <- file.path(out_dir, "submissions")

require_env <- function(name) {
  val <- Sys.getenv(name, unset = NA)
  if (is.na(val) || val == "") stop("Set the ", name, " environment variable before running this script.")
  val
}

central_url <- sub("/+$", "", require_env("CENTRAL_URL"))
project_id <- require_env("CENTRAL_PROJECT_ID")
email <- require_env("CENTRAL_EMAIL")
password <- require_env("CENTRAL_PASSWORD")
upload_limit <- Sys.getenv("CENTRAL_UPLOAD_LIMIT", unset = NA)

xml_files <- list.files(submissions_dir, pattern = "\\.xml$", full.names = TRUE)
if (length(xml_files) == 0) stop("No submission XML files found in ", submissions_dir)
if (!is.na(upload_limit)) {
  xml_files <- head(xml_files, as.integer(upload_limit))
  message("CENTRAL_UPLOAD_LIMIT set -- uploading only ", length(xml_files), " file(s).")
}

message("Logging in to Central as ", email, "...")
session_res <- POST(
  paste0(central_url, "/v1/sessions"),
  body = list(email = email, password = password),
  encode = "json"
)
if (status_code(session_res) != 200) {
  stop("Login failed (", status_code(session_res), "): ",
       content(session_res, "text", encoding = "UTF-8"))
}
token <- content(session_res)$token
message("Logged in.")

submit_url <- paste0(central_url, "/v1/projects/", project_id, "/forms/", FORM_ID, "/submissions")

upload_one <- function(f) {
  res <- POST(
    submit_url,
    add_headers(Authorization = paste("Bearer", token)),
    content_type("application/xml"),
    body = read_file(f)
  )
  ok <- status_code(res) %in% c(200, 201)
  tibble(
    file = basename(f),
    status = status_code(res),
    ok = ok,
    detail = if (ok) NA_character_ else str_sub(content(res, "text", encoding = "UTF-8"), 1, 200)
  )
}

message("Uploading ", length(xml_files), " submission(s) to ", submit_url, " ...")
results <- map(xml_files, upload_one) |> list_rbind()
write_csv(results, file.path(out_dir, "submission_upload_results.csv"), na = "")

n_ok <- sum(results$ok)
message(n_ok, " of ", nrow(results), " submissions uploaded successfully.")
if (n_ok < nrow(results)) {
  message(nrow(results) - n_ok, " failed -- see submission_upload_results.csv for details. ",
          "Common causes: the submission's site_id doesn't reference a real entity yet (did you ",
          "run apply-entity-id-crosswalk.R, and does site_id_crosswalk.csv cover this site?), or ",
          "a duplicate instanceID (already uploaded once -- safe to ignore on a re-run).")
}
