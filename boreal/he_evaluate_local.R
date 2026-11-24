# =============================================================================
# he_evaluate_local.R
#
# Evaluate HawkEars output you ran yourself (CSV or Audacity label files)
# against WildTrax human tags. Drop-in counterpart to
# wildrtrax::wt_evaluate_classifier(): same first five output columns
# (precision, recall, fscore, threshold, classifier) plus tp / fp / fn.
#
# What it does differently from wt_evaluate_classifier():
#   1. Recall counts EVERY human positive, including ones the classifier never
#      flagged at any score. (wildrtrax leaves those out, which is why its
#      recall hit 1.00 at low thresholds in the WildTrax-HawkEars run.)
#   2. Detections come from local files, so if you ran HawkEars with
#      --min_score 0 you can evaluate below WildTrax's 0.30 storage floor.
#   3. Classifier detections are clipped to the window the human listened to
#      (detection start < task_duration).
#   4. Only tasks on recordings you actually ran locally are scored, so
#      recordings you didn't process don't show up as misses.
#   5. Optional: put the WildTrax AI report (older HawkEars / BirdNET) through
#      the SAME counting rules on the SAME tasks for a head-to-head comparison.
#
# The `paired` table it builds has the same columns as the validation
# skeleton (classifier, unit, species_code, human, score), so the skeleton's
# count_at() / figures run on it unchanged.
#
# Main entry point: he_evaluate_classifier()
# =============================================================================

library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)

# ---- Reading local HawkEars output -----------------------------------------

#' Read HawkEars output files
#'
#' @param path       A directory (searched for .csv / .txt) or a vector of files.
#' @param format     "auto" (.csv -> CSV, .txt -> Audacity), "csv", or "audacity".
#' @param recursive  Search sub-directories.
#' @param pattern    Regex for files to pick up inside a directory.
#' @param suffix     Suffix HawkEars appends to Audacity label names, stripped
#'                   to recover the audio file stem.
#' @param col_map    Optional named vector to override CSV column detection,
#'                   e.g. c(file = "filename", start = "start_time",
#'                          code = "class_code", score = "score").
#' @param classifier Label for this run, e.g. "HawkEars 1.x (local)".
#' @return tibble: file, start, end, species_code, score, classifier.
#'         Empty Audacity files give one row with NA species/score so the
#'         recording still counts as processed.
he_read_local <- function(path,
                          format     = c("auto", "csv", "audacity"),
                          recursive  = TRUE,
                          pattern    = "\\.(csv|txt)$",
                          suffix     = "_HawkEars",
                          col_map    = NULL,
                          classifier = "HawkEars (local)") {
  format <- match.arg(format)

  files <- if (length(path) == 1 && dir.exists(path)) {
    list.files(path, pattern = pattern, recursive = recursive,
               full.names = TRUE, ignore.case = TRUE)
  } else {
    path
  }
  if (!length(files)) stop("No HawkEars output files found in: ", path)
  if (any(!file.exists(files))) {
    stop("Files not found: ", paste(head(files[!file.exists(files)], 5), collapse = ", "))
  }

  fmt <- if (format == "auto") {
    if_else(grepl("\\.csv$", files, ignore.case = TRUE), "csv", "audacity")
  } else {
    rep(format, length(files))
  }

  safe <- function(fun) {
    function(f, ...) {
      tryCatch(fun(f, ...),
               error = function(e) {
                 warning("Skipped ", basename(f), ": ", conditionMessage(e), call. = FALSE)
                 NULL
               })
    }
  }

  out <- bind_rows(
    map(files[fmt == "csv"], safe(.read_he_csv), col_map = col_map, suffix = suffix),
    map(files[fmt == "audacity"], safe(.read_he_audacity), suffix = suffix)
  )
  if (!nrow(out)) stop("Nothing could be read. Check `format` / `col_map`.")

  bad <- sum(!is.na(out$species_code) & is.na(out$score))
  if (bad) warning(bad, " labels had no parseable score and were dropped.", call. = FALSE)
  out <- out |>
    filter(!(!is.na(species_code) & is.na(score))) |>
    mutate(classifier = classifier)

  sc <- out$score[!is.na(out$score)]
  message(sprintf(
    "Read %d files (%d recordings): %s detections, %d species, scores %.3f-%.3f",
    length(files), n_distinct(out$file), format(length(sc), big.mark = ","),
    n_distinct(na.omit(out$species_code)), min(sc), max(sc)
  ))
  if (min(sc) > 0.05) {
    message(sprintf("Lowest stored score is %.2f: thresholds below that can't be evaluated.",
                    min(sc)))
  }
  out
}

.file_stem <- function(x, suffix = "_HawkEars") {
  x <- sub("\\.[^.]+$", "", basename(x))
  sub(paste0(suffix, "$"), "", x, ignore.case = TRUE)
}

# Audacity labels: "start<TAB>end<TAB>CODE;score", one detection per line.
# The code/score separator is matched loosely (; : , or space).
.read_he_audacity <- function(f, suffix = "_HawkEars") {
  stem <- .file_stem(f, suffix)
  x <- read_lines(f)
  x <- x[nzchar(trimws(x)) & !startsWith(trimws(x), "\\")]  # drop blank + Audacity freq lines
  if (!length(x)) {
    return(tibble(file = stem, start = NA_real_, end = NA_real_,
                  species_code = NA_character_, score = NA_real_))
  }
  parts <- str_split_fixed(x, "\t", 3)
  lab   <- str_trim(parts[, 3])
  m     <- str_match(lab, "^(.*?)[;:,\\s]\\s*([0-9]*\\.?[0-9]+)$")
  tibble(file         = stem,
         start        = suppressWarnings(as.numeric(trimws(parts[, 1]))),
         end          = suppressWarnings(as.numeric(trimws(parts[, 2]))),
         species_code = if_else(is.na(m[, 2]), lab, str_trim(m[, 2])),
         score        = as.numeric(m[, 3]))
}

.norm_names <- function(x) gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(x)))

.read_he_csv <- function(f, col_map = NULL, suffix = "_HawkEars") {
  x <- read_csv(f, col_types = cols(.default = col_character()),
                show_col_types = FALSE, progress = FALSE)
  if (!nrow(x)) return(NULL)
  names(x) <- .norm_names(names(x))

  cands <- list(
    file  = c("filename", "file_name", "file", "recording", "recording_name",
              "path", "file_path", "audio_file", "begin_file", "begin_path"),
    start = c("start_time", "start", "start_s", "start_sec", "begin_time_s",
              "begin_time", "offset"),
    end   = c("end_time", "end", "end_s", "end_sec", "end_time_s"),
    code  = c("class_code", "species_code", "code", "alpha_code", "banding_code",
              "class", "label", "class_name", "species", "common_name"),
    score = c("score", "confidence", "probability", "prob")
  )
  pick <- map_chr(names(cands), function(k) {
    if (!is.null(col_map) && k %in% names(col_map)) return(.norm_names(col_map[[k]]))
    hit <- intersect(cands[[k]], names(x))
    if (length(hit)) hit[1] else NA_character_
  }) |> set_names(names(cands))

  need <- c("start", "code", "score")
  if (any(is.na(pick[need]) | !pick[need] %in% names(x))) {
    stop("Couldn't find column(s) for ", paste(need[is.na(pick[need])], collapse = ", "),
         ". Columns are: ", paste(names(x), collapse = ", "), ". Use `col_map`.")
  }

  tibble(
    file         = if (!is.na(pick["file"])) .file_stem(x[[pick["file"]]], suffix)
                   else .file_stem(f, suffix),
    start        = as.numeric(x[[pick["start"]]]),
    end          = if (!is.na(pick["end"])) as.numeric(x[[pick["end"]]]) else NA_real_,
    species_code = str_trim(x[[pick["code"]]]),
    score        = suppressWarnings(as.numeric(x[[pick["score"]]]))
  )
}

# ---- Matching files to WildTrax recordings ---------------------------------

# Filenames like LOCATION_YYYYMMDD_HHMMSS(.wav/.flac) are matched on
# location + recording_date_time. Anything else (e.g. SM4 serial-number
# prefixes) needs `file_lookup`: a data frame with columns file, recording_id.
he_match_recordings <- function(det, main, file_lookup = NULL, suffix = "_HawkEars") {
  if ("recording_id" %in% names(det) && !anyNA(det$recording_id)) return(det)

  recs <- main |> distinct(recording_id, location, recording_date_time)

  if (!is.null(file_lookup)) {
    stopifnot(all(c("file", "recording_id") %in% names(file_lookup)))
    key <- file_lookup |>
      transmute(file = .file_stem(file, suffix),
                recording_id = if (is.numeric(recs$recording_id))
                  as.numeric(recording_id) else as.character(recording_id)) |>
      distinct()
  } else {
    m <- str_match(unique(det$file), "^(.+?)[_-]+(\\d{8})[_T-]?(\\d{6})")
    parsed <- tibble(file     = unique(det$file),
                     loc_key  = tolower(m[, 2]),
                     dt_key   = floor(as.numeric(as.POSIXct(paste0(m[, 3], m[, 4]),
                                                            format = "%Y%m%d%H%M%S",
                                                            tz = "UTC"))))
    key <- recs |>
      transmute(recording_id,
                loc_key = tolower(location),
                dt_key  = floor(as.numeric(.as_utc(recording_date_time)))) |>
      inner_join(parsed, by = c("loc_key", "dt_key"), relationship = "many-to-many") |>
      distinct(file, recording_id)
  }

  dup <- key |> count(file) |> filter(n > 1)
  if (nrow(dup)) {
    warning(nrow(dup), " files match more than one WildTrax recording (same location + ",
            "time); keeping the first. e.g. ", paste(head(dup$file, 3), collapse = ", "),
            call. = FALSE)
    key <- key |> distinct(file, .keep_all = TRUE)
  }

  files   <- unique(det$file)
  unmatch <- setdiff(files, key$file)
  message(sprintf("Matched %d of %d HawkEars files to WildTrax recordings.",
                  length(files) - length(unmatch), length(files)))
  if (length(unmatch)) {
    message("  Unmatched (first 5): ", paste(head(unmatch, 5), collapse = ", "),
            "\n  -> not in this project, or names differ: supply `file_lookup`.")
  }
  n_missing <- n_distinct(setdiff(recs$recording_id, key$recording_id))
  if (n_missing) {
    message(sprintf("  %d WildTrax recordings have no local HawkEars file; their tasks are skipped.",
                    n_missing))
  }

  det |> inner_join(key, by = "file", relationship = "many-to-one")
}

.as_utc <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(format(x, "%Y-%m-%d %H:%M:%S"), tz = "UTC"))  # keep clock time
  }
  as.POSIXct(substr(as.character(x), 1, 19), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

# If labels look like names rather than 4-letter codes (HawkEars --label names),
# map them to WildTrax codes using the main report. `species_map` recodes
# explicit mismatches, e.g. c(GRAJ = "CAJA").
.harmonise_species <- function(det, main, species_map = NULL) {
  if (!is.null(species_map)) {
    det <- det |> mutate(species_code = recode(species_code, !!!species_map))
  }
  codes <- unique(na.omit(det$species_code))
  if (length(codes) && mean(grepl("^[A-Z0-9]{4}$", codes)) < 0.5) {
    name_cols <- intersect(c("species_common_name", "species_scientific_name"), names(main))
    lut <- main |>
      select(species_code, all_of(name_cols)) |>
      pivot_longer(all_of(name_cols), values_to = "name") |>
      filter(!is.na(name)) |>
      distinct(key = tolower(name), species_code) |>
      distinct(key, .keep_all = TRUE)
    hit <- match(tolower(det$species_code), lut$key)
    message(sprintf("Labels look like species names; mapped %d of %d names to WildTrax codes.",
                    n_distinct(det$species_code[!is.na(hit)]), length(codes)))
    det$species_code <- if_else(is.na(hit), det$species_code, lut$species_code[hit])
  }
  det
}

# WildTrax AI report -> same shape as local detections
.ai_to_det <- function(ai) {
  ai |>
    transmute(recording_id,
              start        = as.numeric(detection_time),
              end          = NA_real_,
              species_code,
              score        = as.numeric(confidence),
              classifier   = as.character(version))
}

# ---- Pairing human tags with classifier detections -------------------------

#' Build the unit x species table (classifier, unit, species_code, human, score)
#'
#' score = max classifier score for that species in the unit (0 = never
#' detected at any stored score). One set of human positives per classifier,
#' restricted to recordings that classifier actually processed.
he_pair <- function(main, det,
                    resolution         = c("task", "recording", "minute"),
                    species            = NULL,
                    allowed_species    = NULL,
                    classifier_species = NULL,
                    common_recordings  = TRUE) {
  resolution <- match.arg(resolution)

  if (!"task_duration" %in% names(main)) {
    warning("No task_duration in main report: classifier detections are NOT clipped ",
            "to the listened window.", call. = FALSE)
    main$task_duration <- NA_real_
  }
  main <- main |>
    mutate(task_duration  = as.numeric(task_duration),
           detection_time = as.numeric(detection_time))

  if (resolution == "minute" && "task_method" %in% names(main) &&
      any(main$task_method != "1SPM", na.rm = TRUE)) {
    warning("Some tasks aren't 1SPM; minute-level human truth is only valid for 1SPM.",
            call. = FALSE)
  }

  tasks <- main |> distinct(task_id, recording_id, task_duration)

  # Human positives -----------------------------------------------------------
  hum <- main |> filter(!is.na(species_code))
  truth <- switch(
    resolution,
    task      = hum |> transmute(recording_id, unit = as.character(task_id), species_code),
    recording = hum |> transmute(recording_id, unit = as.character(recording_id), species_code),
    minute    = hum |> filter(!is.na(detection_time)) |>
      transmute(recording_id,
                unit = paste0(task_id, "_m", floor(detection_time / 60) + 1),
                species_code)
  ) |>
    distinct() |>
    mutate(human = TRUE)

  # Classifier detections inside the listened window --------------------------
  det_ok <- det |> filter(!is.na(score), !is.na(species_code))
  if (resolution == "recording") {
    win <- tasks |>
      group_by(recording_id) |>
      summarise(window = if (all(is.na(task_duration))) NA_real_
                         else max(task_duration, na.rm = TRUE),
                .groups = "drop")
    det_w <- det_ok |>
      inner_join(win, by = "recording_id") |>
      filter(is.na(window) | is.na(start) | start < window) |>
      mutate(unit = as.character(recording_id))
  } else {
    det_w <- det_ok |>
      inner_join(tasks, by = "recording_id", relationship = "many-to-many") |>
      filter(is.na(task_duration) | is.na(start) | start < task_duration) |>
      mutate(unit = if (resolution == "task") as.character(task_id)
                    else paste0(task_id, "_m", floor(start / 60) + 1))
  }

  # Species filters, applied to both sides ------------------------------------
  if (!is.null(classifier_species)) {
    gone <- truth |> filter(!species_code %in% classifier_species) |>
      count(species_code, sort = TRUE)
    if (nrow(gone)) {
      message(sprintf(
        "Dropped %d human-positive units for %d codes the classifier never outputs (top: %s).",
        sum(gone$n), nrow(gone), paste(head(gone$species_code, 8), collapse = ", ")))
    }
  }
  keep_spp <- function(df) {
    if (!is.null(classifier_species)) df <- filter(df, species_code %in% classifier_species)
    if (!is.null(allowed_species))    df <- filter(df, species_code %in% allowed_species)
    if (!is.null(species))            df <- filter(df, species_code %in% species)
    df
  }
  truth <- keep_spp(truth)
  det_w <- keep_spp(det_w)

  # Which recordings did each classifier process? -----------------------------
  proc <- det |>
    filter(recording_id %in% tasks$recording_id) |>
    distinct(classifier, recording_id)
  n_cls <- n_distinct(proc$classifier)
  if (common_recordings && n_cls > 1) {
    shared <- proc |> count(recording_id) |> filter(n == n_cls) |> pull(recording_id)
    message(sprintf("Comparing %d classifiers on the %d recordings all of them processed.",
                    n_cls, length(shared)))
    proc <- proc |> filter(recording_id %in% shared)
  }
  print(count(proc, classifier, name = "n_recordings"))

  truth_c <- truth |>
    inner_join(proc, by = "recording_id", relationship = "many-to-many") |>
    select(classifier, unit, species_code, human)

  ai_max <- det_w |>
    semi_join(proc, by = c("classifier", "recording_id")) |>
    group_by(classifier, unit, species_code) |>
    summarise(score = max(score), .groups = "drop")

  truth_c |>
    full_join(ai_max, by = c("classifier", "unit", "species_code")) |>
    mutate(human = replace_na(human, FALSE),
           score = replace_na(score, 0))
}

# ---- Metrics ---------------------------------------------------------------

#' TP / FP / FN, precision, recall, F at each threshold
#' @param thresholds length 2 = start/end of a 0.01 sequence (as in wildrtrax);
#'                   longer = the exact thresholds to use.
he_metrics <- function(paired, thresholds = c(0.01, 0.99), by_species = FALSE) {
  thr <- if (length(thresholds) == 2) seq(thresholds[1], thresholds[2], by = 0.01)
         else sort(thresholds)
  thr <- round(thr, 4)   # so joins with wildrtrax output on threshold work
  grp <- c("classifier", if (by_species) "species_code")

  map_dfr(thr, function(t) {
    paired |>
      group_by(across(all_of(grp))) |>
      summarise(tp = sum(human  & score >= t),
                fp = sum(!human & score >= t),
                fn = sum(human  & score <  t),
                .groups = "drop") |>
      mutate(threshold = t)
  }) |>
    mutate(precision = if_else(tp + fp > 0, tp / (tp + fp), NA_real_),
           recall    = if_else(tp + fn > 0, tp / (tp + fn), NA_real_),
           fscore    = if_else(!is.na(precision) & !is.na(recall) & precision + recall > 0,
                               2 * precision * recall / (precision + recall), NA_real_)) |>
    relocate(precision, recall, fscore, threshold, classifier)
}

#' Highest threshold that maximises F (per classifier, and per species if present).
#' Same rule as wildrtrax::wt_classifier_threshold().
he_threshold <- function(eval) {
  eval |>
    filter(!is.na(fscore)) |>
    group_by(across(any_of(c("classifier", "species_code")))) |>
    filter(fscore == max(fscore)) |>
    slice_max(threshold, n = 1, with_ties = FALSE) |>
    ungroup()
}

# ---- Wrapper ---------------------------------------------------------------

#' Evaluate locally run HawkEars against WildTrax human tags
#'
#' @param data      Output of wt_download_report(reports = c("main","ai")), or
#'                  just the main report data frame.
#' @param hawkears  Path(s) to HawkEars output (dir or files), or a tibble
#'                  already read with he_read_local().
#' @param resolution "task", "recording", or "minute" (1SPM only).
#' @param remove_species Restrict to species allowed in the WildTrax project
#'                  (taken from the AI report's is_species_allowed_in_project
#'                  plus human-tagged species), unless `allowed_species` given.
#' @param species   Optional subset of species codes.
#' @param thresholds Start/end of the 0.01 threshold sequence.
#' @param classifier Label for the local run (put the HawkEars version in it).
#' @param include_wildtrax_ai Also score the WildTrax AI report's classifiers
#'                  with the same rules on the same tasks.
#' @param wildtrax_classifiers Which AI-report `version` labels to include
#'                  (NULL = all). Default is just WildTrax's HawkEars.
#' @param common_recordings With >1 classifier, use only recordings all processed.
#' @param by_species Return per-species metrics instead of pooled.
#' @param file_lookup data frame (file, recording_id) when names can't be parsed.
#' @param species_map named vector to recode local codes, e.g. c(GRAJ = "CAJA").
#' @param classifier_species codes the classifier can output. Default: codes seen
#'                  in the local output. Human tags of other codes are dropped.
#' @param allowed_species explicit project species list (overrides remove_species).
#' @param return_paired Return list(metrics, paired) instead of just metrics.
he_evaluate_classifier <- function(data, hawkears,
                                   resolution          = c("task", "recording", "minute"),
                                   remove_species      = TRUE,
                                   species             = NULL,
                                   thresholds          = c(0.01, 0.99),
                                   classifier          = "HawkEars (local)",
                                   format              = "auto",
                                   include_wildtrax_ai = FALSE,
                                   wildtrax_classifiers = "Hawkears v1.0.8",
                                   common_recordings   = TRUE,
                                   by_species          = FALSE,
                                   file_lookup         = NULL,
                                   species_map         = NULL,
                                   classifier_species  = NULL,
                                   allowed_species     = NULL,
                                   return_paired       = FALSE) {
  resolution <- match.arg(resolution)

  # Split wt_download_report() output into main / ai
  if (is.data.frame(data)) {
    main <- data
    ai   <- NULL
  } else {
    is_main <- map_lgl(data, ~ "task_id" %in% names(.x))
    is_ai   <- map_lgl(data, ~ all(c("confidence", "version") %in% names(.x)))
    if (!any(is_main)) stop("No main report (with task_id) found in `data`.")
    main <- data[[which(is_main)[1]]]
    ai   <- if (any(is_ai)) data[[which(is_ai)[1]]] else NULL
  }

  # Local detections
  det <- if (is.data.frame(hawkears)) hawkears
         else he_read_local(hawkears, format = format, classifier = classifier)
  det <- .harmonise_species(det, main, species_map)
  det <- he_match_recordings(det, main, file_lookup)

  if (is.null(classifier_species)) classifier_species <- unique(na.omit(det$species_code))

  # Codes the local run outputs confidently that humans never tagged anywhere:
  # often real FPs, but also how naming mismatches (GRAJ vs CAJA) show up.
  unseen <- det |>
    filter(!is.na(score), score >= 0.9, !species_code %in% main$species_code) |>
    count(species_code, sort = TRUE)
  if (nrow(unseen)) {
    message("High-scoring (>= 0.9) local codes never tagged by humans (check for naming ",
            "mismatches -> species_map): ",
            paste(sprintf("%s (%d)", head(unseen$species_code, 10), head(unseen$n, 10)),
                  collapse = ", "))
  }

  if (include_wildtrax_ai) {
    if (is.null(ai)) stop("include_wildtrax_ai = TRUE but `data` has no AI report.")
    ai_det <- .ai_to_det(ai)
    if (!is.null(wildtrax_classifiers)) {
      ai_det <- filter(ai_det, classifier %in% wildtrax_classifiers)
      if (!nrow(ai_det)) stop("No AI rows for ", paste(wildtrax_classifiers, collapse = ", "),
                              ". Labels in the AI report: ",
                              paste(unique(ai$version), collapse = ", "))
    }
    det <- bind_rows(det, ai_det)
  }

  if (remove_species && is.null(allowed_species)) {
    if (!is.null(ai) && "is_species_allowed_in_project" %in% names(ai)) {
      allowed_species <- union(
        ai$species_code[ai$is_species_allowed_in_project %in% TRUE],
        unique(na.omit(main$species_code)))
    } else {
      message("remove_species = TRUE but there's no AI report to take the project species ",
              "list from; pass `allowed_species` to restrict. Using all species.")
    }
  }

  paired <- he_pair(main, det,
                    resolution         = resolution,
                    species            = species,
                    allowed_species    = allowed_species,
                    classifier_species = classifier_species,
                    common_recordings  = common_recordings)

  floors <- det |>
    filter(!is.na(score)) |>
    group_by(classifier) |>
    summarise(min_score = min(score), .groups = "drop")
  message("Lowest stored score per classifier (no information below these):")
  print(floors)

  metrics <- he_metrics(paired, thresholds = thresholds, by_species = by_species)
  if (return_paired) list(metrics = metrics, paired = paired) else metrics
}
