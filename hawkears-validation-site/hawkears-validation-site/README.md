# HawkEars validation site

A GitHub Pages site that shows how well an AI classifier (HawkEars, BirdNET) matches
human listeners on WildTrax ARU recordings, and lets anyone run the same validation
on their own WildTrax exports in the browser.

- **Overall results**: pooled precision / recall / F-score against score threshold,
  with a draggable threshold, the precision–recall curve and TP / FP / FN counts.
- **By species**: F-max threshold and precision-target threshold for every species,
  sortable and searchable, with each species' curves. Copy or download the table.
- **Run on your data**: drop in a WildTrax main report + AI report CSV. Everything
  is computed in the visitor's browser; nothing is uploaded.
- **How to read this**: plain-language guide to the terms.

The project results update themselves: a weekly GitHub Action logs in to WildTrax,
downloads the project, recomputes the counts and commits `docs/data/results.json`.

## Repo layout

```
docs/index.html            the site (one page)
docs/validate.js           in-browser CSV parsing + counting (upload tool)
docs/data/results.json     project results the site shows (written by the Action)
R/build_results.R          WildTrax -> results.json (same counting rules as validate.js)
.github/workflows/update-results.yml   weekly + on-demand rebuild
```

## One-time setup

1. **Create the repo** on GitHub and push these files to `main`.
   GitHub Pages on a free account needs a **public** repo. The site and repo only
   ever contain species-level counts, never locations or recordings.
2. **Turn on Pages**: *Settings → Pages → Build and deployment → Deploy from a branch*,
   branch `main`, folder `/docs`. The site appears at
   `https://<user>.github.io/<repo>/` after a minute.
3. **Add the WildTrax login as secrets**: *Settings → Secrets and variables → Actions →
   New repository secret*:
   - `WT_USERNAME`: the WildTrax login email
   - `WT_PASSWORD`: its password

   Use an account that can read the project. `wt_auth()` needs a WildTrax email +
   password login; a Google sign-in won't work for automation.
4. **Add the project as variables** (same page, *Variables* tab):
   - `PROJECT_ID`: the WildTrax project ID (number in the project URL)
   - `SITE_LABEL`: text shown at the top, e.g. `Boreal recordings (CWS), HawkEars v1.0.8`
5. **Run it once**: *Actions → Update validation results → Run workflow*.
   After that it runs every Monday. If the results didn't change, nothing is committed.

## Running the build locally

From the repo root in R (with `WT_USERNAME` / `WT_PASSWORD` in `~/.Renviron`):

```r
Sys.setenv(PROJECT_ID = 1234, SITE_LABEL = "Boreal recordings (CWS)")
source("R/build_results.R")
```

To preview the site locally, serve the `docs/` folder (opening `index.html` directly
blocks the data fetch): `python -m http.server -d docs 8000`, then open
<http://localhost:8000>.

## How the numbers are counted

- **Unit**: presence per task × species (or recording × species).
- **Human truth**: species tagged in the unit, restricted to species the classifier
  reports, so unknowns, abiotic tags and out-of-model species aren't counted as misses.
- **Classifier score**: max confidence for the species in the unit, using only
  detections inside the window the human listened to (`detection_time < task_duration`),
  after dropping species not allowed in the project and rows with no score.
- **TP**: tagged and score ≥ t. **FP**: not tagged and score ≥ t.
  **FN**: tagged and no detection or score < t.
- **F-max**: highest threshold maximizing F. **Precision target**: lowest threshold with
  precision ≥ the target (default 0.90).
- **Score floor**: WildTrax stores no detections below a minimum score (0.30 for
  HawkEars), so thresholds at or below it can't be evaluated. The site shades them.

This recall is **not** the same as `wildrtrax::wt_evaluate_classifier()`, which leaves
out human tags the classifier never detected at any score and so starts at recall 1.0.
Precision is the same in both.
