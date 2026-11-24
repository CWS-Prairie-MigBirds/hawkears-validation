# =============================================================================
# Build docs/data/results.json for the validation site from a WildTrax project
#
# Run locally:   Rscript R/build_results.R      (from the repo root)
# Run on GitHub: .github/workflows/update-results.yml (weekly + on demand)
#
# Needs env vars: WT_USERNAME, WT_PASSWORD (WildTrax login), PROJECT_ID
# Optional:       SITE_LABEL (text shown on the site), UNIT ("task"/"recording")
#
# Only species-level counts are written. Raw reports stay in a temp folder and
# are never committed, so no locations or recordings end up on the public site.
# Counting rules match docs/validate.js (the in-browser upload tool).
# =============================================================================

suppressPackageStartupMessages({
  library(wildrtrax)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(jsonlite)
})

PROJECT_ID <- as.integer(Sys.getenv("PROJECT_ID"))
SITE_LABEL <- Sys.getenv("SITE_LABEL", paste("WildTrax project", PROJECT_ID))
UNIT       <- if (Sys.getenv("UNIT", "task") == "recording") "recording_id" else "task_id"
THRESHOLDS <- round(seq(0.01, 0.99, by = 0.01), 2)   # round(): avoids 0.15000000000000002
OUT_FILE   <- file.path("docs", "data", "results.json")

stopifnot(!is.na(PROJECT_ID), nzchar(Sys.getenv("WT_USERNAME")))

# ---- download ---------------------------------------------------------------
wt_auth()
data <- wt_download_report(project_id = PROJECT_ID, sensor_id = "ARU",
                           reports = c("main", "ai"))

is_ai <- map_lgl(data, ~ "confidence" %in% names(.x))
stopifnot(sum(is_ai) == 1)
ai   <- data[[which(is_ai)]]
main <- data[[which(!is_ai)[1]]]

# ---- human side -------------------------------------------------------------
if (!"task_duration" %in% names(main)) main$task_duration <- NA_real_
main <- main |> mutate(unit = as.character(.data[[UNIT]]))
units <- unique(main$unit)

# recording -> unit + listened window (recording level: longest task)
rec_unit <- main |>
  group_by(recording_id, unit) |>
  summarise(window = suppressWarnings(max(as.numeric(task_duration), na.rm = TRUE)),
            .groups = "drop") |>
  mutate(window = if_else(is.finite(window), window, NA_real_))

names_lu <- bind_rows(
  distinct(main, species_code, species_common_name),
  if ("species_common_name" %in% names(ai)) distinct(ai, species_code, species_common_name)
) |> filter(!is.na(species_code)) |> distinct(species_code, .keep_all = TRUE)

# ---- classifier side --------------------------------------------------------
n_total <- nrow(ai)
if ("is_species_allowed_in_project" %in% names(ai)) {
  ai <- filter(ai, as.logical(is_species_allowed_in_project) %in% TRUE)
}
ai <- ai |>
  filter(!is.na(confidence)) |>
  mutate(classifier = as.character(version))
if (!"detection_time" %in% names(ai) && "ai_detection_time" %in% names(ai)) {
  ai$detection_time <- ai$ai_detection_time
}

if (UNIT == "task_id" && "task_id" %in% names(ai)) {
  ai_units <- ai |> mutate(unit = as.character(task_id)) |> filter(unit %in% units)
} else {
  ai_units <- ai |>
    inner_join(rec_unit, by = "recording_id", relationship = "many-to-many") |>
    filter(is.na(window) | is.na(detection_time) | detection_time < window)
}

ai_max <- ai_units |>
  group_by(classifier, unit, species_code) |>
  summarise(score = max(confidence), .groups = "drop")

# ---- counts per classifier --------------------------------------------------
build_classifier <- function(cl) {
  sc <- filter(ai_max, classifier == cl)
  cl_species <- sort(unique(sc$species_code))

  truth <- main |>
    filter(species_code %in% cl_species) |>
    distinct(unit, species_code) |>
    mutate(human = TRUE)

  paired <- full_join(truth, select(sc, -classifier), by = c("unit", "species_code")) |>
    mutate(human = replace_na(human, FALSE), score = replace_na(score, 0))

  species <- map(cl_species, function(sp) {
    d <- filter(paired, species_code == sp)
    list(code    = sp,
         name    = names_lu$species_common_name[match(sp, names_lu$species_code)],
         n_human = sum(d$human),
         tp      = map_int(THRESHOLDS, ~ sum(d$human  & d$score >= .x - 1e-9)),
         fp      = map_int(THRESHOLDS, ~ sum(!d$human & d$score >= .x - 1e-9)))
  })

  list(name     = cl,
       min_score = min(filter(ai, classifier == cl)$confidence),
       # I() keeps a one-element vector as a JSON array under auto_unbox
       excluded_human_species = I(sort(setdiff(unique(na.omit(main$species_code)), cl_species))),
       pooled   = list(n_human = sum(map_int(species, "n_human")),
                       tp = Reduce(`+`, map(species, "tp")),
                       fp = Reduce(`+`, map(species, "fp"))),
       species  = species)
}

classifiers <- map(sort(unique(ai_max$classifier)), build_classifier)

out <- list(
  meta = list(label = SITE_LABEL,
              generated = format(Sys.Date()),
              unit = if (UNIT == "recording_id") "recording" else "task",
              n_units = length(units),
              thresholds = THRESHOLDS,
              wildrtrax_version = as.character(packageVersion("wildrtrax")),
              ai_rows = list(total = n_total, used = nrow(ai_units))),
  classifiers = classifiers)

dir.create(dirname(OUT_FILE), recursive = TRUE, showWarnings = FALSE)
write_json(out, OUT_FILE, auto_unbox = TRUE, digits = NA, null = "null", na = "null")
message("Wrote ", OUT_FILE, ": ", length(classifiers), " classifier(s), ",
        length(units), " units")
