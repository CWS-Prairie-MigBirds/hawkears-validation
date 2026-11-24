/* HawkEars / BirdNET validation against WildTrax human tags.
 * Runs entirely in the browser: reads the WildTrax main report and AI report
 * CSVs, matches classifier detections to human tags at the unit x species level,
 * and returns TP / FP counts at thresholds 0.01-0.99 in the same JSON format
 * that build_results.R writes for the project dashboard.
 *
 * Counting rules (same as build_results.R):
 *  - unit = task (or recording); presence/absence per unit x species
 *  - human truth = species tagged in the unit, restricted to species the
 *    classifier can report (so unknowns / abiotic tags are not misses)
 *  - classifier score = max confidence for that species in the unit,
 *    using only detections inside the window the human listened to
 *  - TP: human tagged & score >= t; FP: not tagged & score >= t;
 *    FN: tagged & (no detection or score < t)
 */
(function (root) {
  const THRESHOLDS = Array.from({ length: 99 }, (_, i) => Math.round((i + 1)) / 100);

  const ALIASES = {
    task_id: ["task_id"],
    recording_id: ["recording_id"],
    species_code: ["species_code"],
    species_common_name: ["species_common_name"],
    task_duration: ["task_duration"],
    confidence: ["confidence", "score"],
    version: ["version", "classifier"],
    detection_time: ["detection_time", "ai_detection_time"],
    allowed: ["is_species_allowed_in_project"],
  };

  function findCols(header, wanted) {
    const h = header.map((x) => x.trim().toLowerCase());
    const idx = {};
    for (const key of wanted) {
      const al = ALIASES[key] || [key];
      idx[key] = h.findIndex((c) => al.includes(c));
    }
    return idx;
  }

  // Streaming CSV parser (RFC 4180 quotes, CRLF/LF). Calls onRow(arrayOfFields).
  async function parseCSV(blob, onHeader, onRow, onProgress) {
    const reader = blob.stream().getReader();
    const dec = new TextDecoder("utf-8");
    let field = "", row = [], inQ = false, pendQ = false, header = null;
    let read = 0, first = true;
    const emit = () => {
      row.push(field); field = "";
      if (header === null) {
        if (row[0]) row[0] = row[0].replace(/^﻿/, "");
        header = row; onHeader(header);
      } else if (!(row.length === 1 && row[0] === "")) {
        onRow(row);
      }
      row = [];
    };
    for (;;) {
      const { done, value } = await reader.read();
      const text = done ? dec.decode() : dec.decode(value, { stream: true });
      if (value) read += value.byteLength;
      for (let i = 0; i < text.length; i++) {
        const c = text[i];
        if (pendQ) {
          pendQ = false;
          if (c === '"') { field += '"'; continue; }
          inQ = false;
        }
        if (inQ) {
          if (c === '"') pendQ = true; else field += c;
          continue;
        }
        if (c === '"') { inQ = true; continue; }
        if (c === ",") { row.push(field); field = ""; continue; }
        if (c === "\n") { emit(); continue; }
        if (c === "\r") continue;
        field += c;
      }
      if (onProgress && value) onProgress(read / (blob.size || 1));
      if (done) break;
      first = false;
    }
    if (pendQ) inQ = false;
    if (field !== "" || row.length) emit();
    return header;
  }

  const truthy = (v) => {
    if (v === undefined) return true;
    const s = String(v).trim().toLowerCase();
    return !(s === "false" || s === "f" || s === "0" || s === "no");
  };
  const num = (v) => {
    if (v === undefined || v === null) return NaN;
    const s = String(v).trim();
    if (s === "" || s.toUpperCase() === "NA") return NaN;
    return Number(s);
  };
  // bin k means score in [k/100, (k+1)/100); threshold t_k = k/100 includes bins >= k
  const binOf = (score) => Math.max(0, Math.min(100, Math.floor(score * 100 + 1e-9)));

  async function computeFromFiles(mainFile, aiFile, opts, status) {
    const unitKey = opts.unit === "recording" ? "recording_id" : "task_id";
    const say = status || (() => {});

    // ---- main report -------------------------------------------------------
    let mc;
    const units = new Set();
    const recUnits = new Map();      // recording_id -> Map(unit -> window seconds or NaN)
    const truth = new Map();         // species -> Set(unit)
    const names = new Map();
    say("Reading main report…", 0);
    await parseCSV(mainFile,
      (h) => {
        mc = findCols(h, ["task_id", "recording_id", "species_code", "species_common_name", "task_duration"]);
        const need = ["recording_id", "species_code"].concat(unitKey === "task_id" ? ["task_id"] : []);
        const miss = need.filter((k) => mc[k] < 0);
        if (miss.length) throw new Error("Main report is missing column(s): " + miss.join(", ") +
          ". Columns found: " + h.slice(0, 40).join(", "));
      },
      (r) => {
        const rec = r[mc.recording_id];
        const unit = r[mc[unitKey]];
        if (!rec || !unit) return;
        units.add(unit);
        const dur = mc.task_duration >= 0 ? num(r[mc.task_duration]) : NaN;
        let m = recUnits.get(rec);
        if (!m) { m = new Map(); recUnits.set(rec, m); }
        const prev = m.get(unit);
        // recording-level unit: window = longest task on that recording
        if (prev === undefined || (!isNaN(dur) && (isNaN(prev) || dur > prev))) m.set(unit, dur);
        const sp = r[mc.species_code];
        if (sp) {
          let s = truth.get(sp);
          if (!s) { s = new Set(); truth.set(sp, s); }
          s.add(unit);
          if (mc.species_common_name >= 0 && !names.has(sp)) names.set(sp, r[mc.species_common_name]);
        }
      },
      (f) => say("Reading main report…", f * 0.3));
    if (!units.size) throw new Error("No tasks found in the main report.");

    // ---- AI report ---------------------------------------------------------
    let ac;
    const scores = new Map();        // classifier -> Map(species -> Map(unit -> max score))
    const minScore = new Map();
    let nRows = 0, nKept = 0, nNoMatch = 0, nOutside = 0, nNA = 0, nNotAllowed = 0;
    let aiHasTask = false;
    say("Reading AI report…", 0.3);
    await parseCSV(aiFile,
      (h) => {
        ac = findCols(h, ["task_id", "recording_id", "species_code", "confidence", "version",
                          "detection_time", "allowed", "species_common_name"]);
        const miss = ["recording_id", "species_code", "confidence", "version"].filter((k) => ac[k] < 0);
        if (miss.length) throw new Error("AI report is missing column(s): " + miss.join(", ") +
          ". Columns found: " + h.slice(0, 40).join(", "));
        aiHasTask = unitKey === "task_id" && ac.task_id >= 0;
      },
      (r) => {
        nRows++;
        if (ac.allowed >= 0 && !truthy(r[ac.allowed])) { nNotAllowed++; return; }
        const conf = num(r[ac.confidence]);
        if (isNaN(conf)) { nNA++; return; }
        const cl = r[ac.version] || "classifier";
        const sp = r[ac.species_code];
        if (!sp) return;
        let targets;
        if (aiHasTask) {
          const t = r[ac.task_id];
          targets = units.has(t) ? [t] : [];
        } else {
          const m = recUnits.get(r[ac.recording_id]);
          if (!m) { nNoMatch++; return; }
          const dt = ac.detection_time >= 0 ? num(r[ac.detection_time]) : NaN;
          targets = [];
          for (const [u, win] of m) {
            if (!isNaN(win) && !isNaN(dt) && dt >= win) continue;
            targets.push(u);
          }
          if (!targets.length) { nOutside++; return; }
        }
        if (!targets.length) { nNoMatch++; return; }
        let byCl = scores.get(cl);
        if (!byCl) { byCl = new Map(); scores.set(cl, byCl); minScore.set(cl, Infinity); }
        let bySp = byCl.get(sp);
        if (!bySp) { bySp = new Map(); byCl.set(sp, bySp); }
        for (const u of targets) {
          const p = bySp.get(u);
          if (p === undefined || conf > p) bySp.set(u, conf);
        }
        if (conf < minScore.get(cl)) minScore.set(cl, conf);
        if (!names.has(sp) && ac.species_common_name >= 0) names.set(sp, r[ac.species_common_name]);
        nKept++;
      },
      (f) => say("Reading AI report…", 0.3 + f * 0.65));
    if (!scores.size) throw new Error("No usable classifier detections matched the human-processed recordings.");

    // ---- counts ------------------------------------------------------------
    say("Counting…", 0.96);
    const classifiers = [];
    for (const [cl, bySp] of [...scores].sort((a, b) => a[0].localeCompare(b[0]))) {
      const species = [];
      const ptp = new Array(99).fill(0), pfp = new Array(99).fill(0);
      let pn = 0;
      for (const sp of [...bySp.keys()].sort()) {
        const sc = bySp.get(sp);
        const pos = truth.get(sp) || new Set();
        const posB = new Array(101).fill(0), negB = new Array(101).fill(0);
        for (const [u, s] of sc) (pos.has(u) ? posB : negB)[binOf(s)]++;
        // cumulative from the top: count of scores >= k/100
        const tp = new Array(99), fp = new Array(99);
        let cp = posB[100], cn = negB[100];
        for (let k = 99; k >= 1; k--) {
          cp += posB[k]; cn += negB[k];
          tp[k - 1] = cp; fp[k - 1] = cn;
        }
        // scores below 0.01 never pass any threshold; nothing to add
        species.push({ code: sp, name: names.get(sp) || null, n_human: pos.size, tp, fp });
        pn += pos.size;
        for (let i = 0; i < 99; i++) { ptp[i] += tp[i]; pfp[i] += fp[i]; }
      }
      const excluded = [...truth.keys()].filter((s) => !bySp.has(s));
      classifiers.push({
        name: cl,
        min_score: minScore.get(cl),
        excluded_human_species: excluded.sort(),
        pooled: { n_human: pn, tp: ptp, fp: pfp },
        species,
      });
    }
    say("Done", 1);
    return {
      meta: {
        label: opts.label || "Your data",
        generated: new Date().toISOString().slice(0, 10),
        unit: opts.unit === "recording" ? "recording" : "task",
        n_units: units.size,
        thresholds: THRESHOLDS,
        ai_rows: { total: nRows, used: nKept, not_allowed: nNotAllowed, missing_score: nNA,
                   no_human_recording: nNoMatch, outside_listened_window: nOutside },
      },
      classifiers,
    };
  }

  const api = { THRESHOLDS, parseCSV, computeFromFiles, binOf };
  root.HV = api;
  if (typeof module !== "undefined" && module.exports) module.exports = api;
})(typeof self !== "undefined" ? self : globalThis);
