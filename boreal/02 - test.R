# =============================================================================
# HawkEars validation against human-tagged WildTrax recordings
# Wrangle AI classifications + skeleton of standard validation figures
#
# Workflow
#   0. Setup & authentication
#   1. Download main + ai reports from WildTrax
#   2. Wrangle & QC (which element is which, classifier versions, score floor)
#   3. Official wildrtrax metrics: wt_evaluate_classifier() + wt_classifier_threshold()
#   4. Manual TP / FP / FN at unit x species x threshold (wildrtrax returns only
#      P / R / F, not counts) + cross-check against wildrtrax pooled numbers
#   5. Species-level summaries (F-max threshold, precision-target threshold)
#   6. Figures (skeletons)
#   7. Export tables
#
# Notes
#   - Unit of evaluation = WildTrax task (1SPT: presence/absence per species per
#     task). Switch UNIT to "recording_id" for recording-level evaluation.
#     "minute" resolution only works for 1SPM tasks.
#   - A "true positive" here = classifier score >= threshold for a species in a
#     unit where a human tagged that species. Presence-level, not call-level.
#   - Human tags are restricted to species the classifier can report, so
#     unknowns / abiotic / non-HawkEars species don't inflate false negatives.
# =============================================================================

# ---- 0. Setup ---------------------------------------------------------------

library(wildrtrax)
library(tidyverse)

# Credentials: put these in ~/.Renviron (usethis::edit_r_environ()), not here
#   WT_USERNAME=you@email.ca
#   WT_PASSWORD=********
wt_auth()

PROJECT_ID   <- 0000          # <-- WildTrax project ID
UNIT         <- "task_id"     # "task_id" or "recording_id"
WT_RES       <- "task"        # matching wildrtrax resolution: "task" / "recording"
THRESHOLDS   <- seq(0.01, 0.99, by = 0.01)
MIN_HUMAN_N  <- 10            # min # human-positive units to report a species
TARGET_PREC  <- 0.90          # precision target for the alternate threshold
FOCAL_SPP    <- NULL          # e.g. c("OVEN", "SWTH", "WTSP"); NULL = all

# All outputs go here, regardless of working directory / RStudio project
OUT_DIR <- "C:/Users/megre/OneDrive/Desktop/Projects and Scripts/hawkears-validation/boreal"
out <- function(...) file.path(OUT_DIR, ...)

for (d in c("data", "figures", "tables")) {
  dir.create(out(d), recursive = TRUE, showWarnings = FALSE)
}
stopifnot(dir.exists(out("figures")))

# Palette: categorical slots 1-3 (CVD-validated all-pairs); slot 3 is low
# contrast on white, so metrics also get linetypes.
pal3 <- c("#2a78d6", "#eb6834", "#1baf7a")
seq_blue <- c("#cde2fb", "#86b6ef", "#3987e5", "#1c5cab", "#0d366b")

theme_val <- theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e1e0d9", linewidth = 0.3),
        strip.background = element_rect(fill = "white", colour = "#c3c2b7"),
        legend.position  = "top")

# ---- 1. Download ------------------------------------------------------------

raw_path <- out("data", paste0("wt_project_", PROJECT_ID, "_main_ai.rds"))

if (!file.exists(raw_path)) {
  data <- wt_download_report(project_id  = PROJECT_ID,
                             sensor_id   = "ARU",
                             reports     = c("main", "ai"),
                             weather_cols = FALSE)
  saveRDS(data, raw_path)
} else {
  data <- readRDS(raw_path)
}

# ---- 2. Wrangle & QC --------------------------------------------------------

# Don't assume list order: identify the AI element by its 'confidence' column
str(data, max.level = 1)
is_ai <- map_lgl(data, ~ "confidence" %in% names(.x))
stopifnot(sum(is_ai) == 1)
ai   <- data[[which(is_ai)]]
main <- data[[which(!is_ai)[1]]]

# Quick look
glimpse(main)
glimpse(ai)

# Classifier label. Check 'version' (and 'organization') values and edit if
# you want a cleaner label, e.g. "HawkEars v1.0.8".
ai <- ai |> mutate(classifier = as.character(version))
count(ai, classifier)

# Score floor: recall below the minimum stored score is not estimable
ai |>
  group_by(classifier) |>
  summarise(n_det = n(),
            min_score = min(confidence),
            max_score = max(confidence),
            n_spp = n_distinct(species_code))

# Species allowed in the project (mirrors remove_species = TRUE)
ai <- ai |> filter(is_species_allowed_in_project)

# Classifier-reportable species list
ai_species <- unique(ai$species_code)

# Units = everything humans processed (tasks with no tags still count)
units <- main |> distinct(.data[[UNIT]]) |> pull()

# Human truth: presence per unit x species
truth <- main |>
  filter(species_code %in% ai_species) |>
  distinct(unit = .data[[UNIT]], species_code) |>
  mutate(human = TRUE)

# Which human-tagged species can't be evaluated (not in classifier output)?
main |>
  filter(!species_code %in% ai_species) |>
  count(species_code, species_common_name, sort = TRUE) |>
  print(n = 30)

# Classifier: max score per unit x species x classifier, units humans processed
ai_max <- ai |>
  filter(.data[[UNIT]] %in% units) |>
  group_by(classifier, unit = .data[[UNIT]], species_code) |>
  summarise(score = max(confidence), .groups = "drop")

# Pair them: every human-positive (for each classifier) + every AI detection
paired <- truth |>
  crossing(classifier = unique(ai_max$classifier)) |>
  full_join(ai_max, by = c("classifier", "unit", "species_code")) |>
  mutate(human = replace_na(human, FALSE),
         score = replace_na(score, 0))

if (!is.null(FOCAL_SPP)) paired <- filter(paired, species_code %in% FOCAL_SPP)

saveRDS(paired, out("data", "paired_unit_species.rds"))

# ---- 3. Official wildrtrax metrics -----------------------------------------

eval_wt <- wt_evaluate_classifier(data,
                                  resolution     = WT_RES,
                                  remove_species = TRUE,
                                  species        = FOCAL_SPP,
                                  thresholds     = range(THRESHOLDS))
# columns: precision, recall, fscore, threshold, classifier

thr_wt <- wt_classifier_threshold(eval_wt)   # highest threshold maximizing F
thr_wt

# ---- 4. Manual TP / FP / FN -------------------------------------------------

count_at <- function(df, t) {
  df |>
    group_by(classifier, species_code) |>
    summarise(tp = sum(human  & score >= t),
              fp = sum(!human & score >= t),
              fn = sum(human  & score <  t),
              .groups = "drop") |>
    mutate(threshold = t)
}

add_metrics <- function(df) {
  df |>
    mutate(precision = if_else(tp + fp > 0, tp / (tp + fp), NA_real_),
           recall    = if_else(tp + fn > 0, tp / (tp + fn), NA_real_),
           fscore    = if_else(precision + recall > 0,
                               2 * precision * recall / (precision + recall),
                               NA_real_))
}

counts_spp <- map_dfr(THRESHOLDS, ~ count_at(paired, .x)) |> add_metrics()

counts_all <- counts_spp |>
  group_by(classifier, threshold) |>
  summarise(across(c(tp, fp, fn), sum), .groups = "drop") |>
  add_metrics()

# Cross-check: manual pooled vs wildrtrax. Small differences = >= vs > or
# rounding; large differences = different counting rules (worth knowing!).
xcheck <- counts_all |>
  inner_join(eval_wt, by = c("classifier", "threshold"),
             suffix = c("_manual", "_wt")) |>
  mutate(d_prec = precision_manual - precision_wt,
         d_rec  = recall_manual    - recall_wt)
summary(select(xcheck, d_prec, d_rec))
# If classifier labels don't match, inner_join is empty: align the labels.

# ---- 5. Species-level summaries --------------------------------------------

spp_n <- paired |>
  filter(human) |>
  count(classifier, species_code, name = "n_human")

eval_spp <- counts_spp |>
  left_join(spp_n, by = c("classifier", "species_code")) |>
  filter(n_human >= MIN_HUMAN_N)

# Threshold maximizing F (ties -> highest threshold, like wt_classifier_threshold)
thr_fmax <- eval_spp |>
  filter(!is.na(fscore)) |>
  group_by(classifier, species_code) |>
  filter(fscore == max(fscore)) |>
  slice_max(threshold, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(rule = "F-max")

# Lowest threshold reaching target precision (keeps most recall)
thr_prec <- eval_spp |>
  filter(precision >= TARGET_PREC) |>
  group_by(classifier, species_code) |>
  slice_min(threshold, n = 1, with_ties = FALSE) |>
  ungroup() |>
  mutate(rule = paste0("P >= ", TARGET_PREC))

spp_thresholds <- bind_rows(thr_fmax, thr_prec) |>
  select(classifier, species_code, rule, threshold, n_human,
         tp, fp, fn, precision, recall, fscore)

# Species that never reach the precision target -> flag for manual verification
anti_join(distinct(eval_spp, classifier, species_code), thr_prec,
          by = c("classifier", "species_code"))

# ---- 6. Figures -------------------------------------------------------------

metric_long <- function(df) {
  df |>
    pivot_longer(c(precision, recall, fscore),
                 names_to = "metric", values_to = "value") |>
    mutate(metric = factor(metric, c("precision", "recall", "fscore"),
                           c("Precision", "Recall", "F-score")))
}

# Fig 1. Precision, recall, F-score vs threshold (pooled, wildrtrax output)
f1 <- eval_wt |>
  metric_long() |>
  ggplot(aes(threshold, value, colour = metric, linetype = metric)) +
  geom_line(linewidth = 0.8) +
  geom_vline(data = thr_wt, aes(xintercept = threshold),
             colour = "grey40", linetype = "dotted") +
  facet_wrap(~ classifier) +
  scale_colour_manual(values = pal3, name = NULL) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash"), name = NULL) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Score threshold", y = "Metric value") +
  theme_val
ggsave(out("figures", "fig1_prf_vs_threshold.png"), f1,
       width = 7, height = 4.5, dpi = 300)

# Fig 2. Precision-recall curve, F-max threshold marked
f2 <- ggplot(eval_wt, aes(recall, precision, colour = classifier)) +
  geom_path(linewidth = 0.8) +
  geom_point(data = semi_join(eval_wt, thr_wt, by = c("classifier", "threshold")),
             size = 3) +
  scale_colour_manual(values = pal3, name = NULL) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(x = "Recall", y = "Precision") +
  theme_val
ggsave(out("figures", "fig2_pr_curve.png"), f2,
       width = 5, height = 5, dpi = 300)

# Fig 3. TP / FP / FN counts vs threshold (pooled, manual)
f3 <- counts_all |>
  pivot_longer(c(tp, fp, fn), names_to = "outcome", values_to = "n") |>
  mutate(outcome = factor(outcome, c("tp", "fp", "fn"),
                          c("True positive", "False positive", "False negative"))) |>
  ggplot(aes(threshold, n, colour = outcome, linetype = outcome)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ classifier, scales = "free_y") +
  scale_colour_manual(values = pal3, name = NULL) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash"), name = NULL) +
  labs(x = "Score threshold", y = "Units x species") +
  theme_val
ggsave(out("figures", "fig3_tp_fp_fn_vs_threshold.png"), f3,
       width = 7, height = 4.5, dpi = 300)

# Fig 4. Species x threshold F-score heatmap
spp_order <- thr_fmax |> arrange(fscore) |> pull(species_code) |> unique()
f4 <- eval_spp |>
  mutate(species_code = factor(species_code, spp_order)) |>
  ggplot(aes(threshold, species_code, fill = fscore)) +
  geom_tile() +
  geom_point(data = mutate(thr_fmax, species_code = factor(species_code, spp_order)),
             aes(threshold, species_code), inherit.aes = FALSE,
             shape = 21, fill = "white", size = 1.6) +
  facet_wrap(~ classifier) +
  scale_fill_gradientn(colours = seq_blue, limits = c(0, 1),
                       na.value = "grey95", name = "F-score") +
  scale_x_continuous(expand = c(0, 0)) +
  labs(x = "Score threshold", y = NULL) +
  theme_val + theme(legend.position = "right")
ggsave(out("figures", "fig4_species_fscore_heatmap.png"), f4,
       width = 7, height = max(4, 0.18 * length(spp_order) + 1.5), dpi = 300)

# Fig 5. Per-species precision & recall at the F-max threshold
f5 <- thr_fmax |>
  mutate(species_code = factor(species_code, spp_order)) |>
  pivot_longer(c(precision, recall), names_to = "metric", values_to = "value") |>
  ggplot(aes(value, species_code, colour = metric, shape = metric)) +
  geom_line(aes(group = species_code), colour = "grey75", linewidth = 0.5) +
  geom_point(size = 2.5) +
  geom_text(data = mutate(thr_fmax, species_code = factor(species_code, spp_order)),
            aes(x = 1.02, y = species_code,
                label = sprintf("t=%.2f  n=%d", threshold, n_human)),
            inherit.aes = FALSE, hjust = 0, size = 2.6, colour = "grey30") +
  facet_wrap(~ classifier) +
  scale_colour_manual(values = pal3[1:2], name = NULL,
                      labels = c("Precision", "Recall")) +
  scale_shape_manual(values = c(16, 17), name = NULL,
                     labels = c("Precision", "Recall")) +
  scale_x_continuous(limits = c(0, 1.25), breaks = seq(0, 1, 0.25)) +
  labs(x = "Value at F-max threshold", y = NULL) +
  theme_val
ggsave(out("figures", "fig5_species_pr_at_fmax.png"), f5,
       width = 7, height = max(4, 0.18 * length(spp_order) + 1.5), dpi = 300)

# Fig 6. Score distributions: human-present vs human-absent (top species)
top_spp <- spp_n |> slice_max(n_human, n = 12) |> pull(species_code) |> unique()
f6 <- paired |>
  # one classifier at a time; change the label if you ran more than one
  filter(classifier == unique(paired$classifier)[1]) |>
  filter(species_code %in% top_spp, score > 0) |>   # score 0 = not detected
  mutate(truth = if_else(human, "Human tagged (TP)", "Not tagged (FP)")) |>
  ggplot(aes(score, fill = truth)) +
  geom_histogram(binwidth = 0.05, boundary = 0, position = "identity",
                 alpha = 0.7, colour = "white", linewidth = 0.2) +
  facet_wrap(~ species_code, scales = "free_y") +
  scale_fill_manual(values = pal3[1:2], name = NULL) +
  labs(x = "Max HawkEars score in unit", y = "Units") +
  theme_val
ggsave(out("figures", "fig6_score_distributions.png"), f6,
       width = 9, height = 7, dpi = 300)

# Fig 7. Per-species P/R/F curves, small multiples (top species)
f7 <- eval_spp |>
  filter(species_code %in% top_spp) |>
  metric_long() |>
  ggplot(aes(threshold, value, colour = metric, linetype = metric)) +
  geom_line(linewidth = 0.6, na.rm = TRUE) +
  facet_wrap(~ species_code) +
  scale_colour_manual(values = pal3, name = NULL) +
  scale_linetype_manual(values = c("solid", "longdash", "dotdash"), name = NULL) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = "Score threshold", y = "Metric value") +
  theme_val
ggsave(out("figures", "fig7_species_prf_curves.png"), f7,
       width = 9, height = 7, dpi = 300)

# ---- 7. Export tables -------------------------------------------------------

write_csv(eval_wt,        out("tables", "wt_eval_pooled.csv"))
write_csv(counts_all,     out("tables", "manual_counts_pooled.csv"))
write_csv(eval_spp,       out("tables", "manual_metrics_by_species.csv"))
write_csv(spp_thresholds, out("tables", "species_thresholds.csv"))