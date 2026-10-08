# =============================================================================
# Pupil preprocessing pipeline — updated v4
#
# Changes vs v3:
#   1. BUTTERWORTH FILTER: pupil_smooth_butterworth() applies a zero-phase
#      Butterworth low-pass filter (via signal::butter + signal::filtfilt) as
#      an alternative to the Hann window.  Select via params$smooth$type:
#        "hann"       – original Hann window (unchanged)
#        "butterworth"– new Butterworth low-pass filter
#      Key parameters for Butterworth (set in params$smooth):
#        cutoff_hz  – cutoff frequency in Hz (default 8)
#        order      – filter order (default 4; higher = steeper roll-off)
#      At 1000 Hz a 4th-order Butterworth at 8 Hz is a reasonable starting
#      point for slow cognitive pupil responses.  Try 4–8 Hz and compare.
#
#   2. SPLIT MISSING-DATA CUTOFF: pupil_missing_split() replaces the single
#      params$missing$missing_allowed threshold with two separate thresholds:
#        missing_allowed_baseline  – proportion of NAs tolerated in the
#                                    fixation_baseline window immediately
#                                    preceding target onset (see below)
#        missing_allowed_stimulus  – proportion of NAs tolerated from the
#                                    first "target" onset to the end of the
#                                    trial (e.g. 0.30)
#      A trial is discarded if *either* threshold is exceeded.
#      The function uses the "message" / Stimulus column to identify the two
#      windows (same convention already used throughout the pipeline).
#      Falls back to the flat params$missing$missing_allowed if the split
#      parameters are absent, so the change is backward-compatible.
#
#      UPDATE: the baseline window used for this rejection check is now
#      restricted to "fixation_baseline" rows falling within
#      baseline_duration ms immediately before target onset — i.e. the same
#      window used later by fill_baseline_bc() / calculate_pupil_rate() for
#      the actual baseline correction — rather than every row anywhere in
#      the trial labelled "fixation_baseline". See check_missing_split().
#
#   3. GAZE-POSITION CORRECTION (advisory – see note at bottom of file):
#      apply_gaze_correction() regresses pupil size on gaze X and Y
#      coordinates within each trial (or optionally across all trials of a
#      subject) and subtracts the fitted gaze component.  This removes the
#      linear component of the foreshortening artifact arising from off-center
#      gaze.  See the detailed note at the end of this file.
#      When apply_gaze_corr = TRUE, two additional plots are saved per trial
#      in the existing subject subdirectories of plot_dir:
#        S{subj}_T{tr}_13_gc_lrm.png         – L / R / mean gaze-corrected traces
#        S{subj}_T{tr}_14_gc_mean_overlay.png – corrected vs uncorrected overlay
#        S{subj}_T{tr}_gc_all_steps.png       – combined two-panel summary
#      (The individual PNGs are only written when save_individual_plots = TRUE;
#      the combined panel is always written.)
# =============================================================================


# -----------------------------------------------------------------------------
# 1. Plotting helpers (unchanged)
# -----------------------------------------------------------------------------

create_pupil_plot_both <- function(data, title, units = "mm") {
  if (units == "mm") {
    left_col  <- "L_Pupil_Diameter.mm"
    right_col <- "R_Pupil_Diameter.mm"
    y_label   <- "Pupil Size (mm)"
  } else {
    left_col  <- "L_Pupil_Diameter.px"
    right_col <- "R_Pupil_Diameter.px"
    y_label   <- "Pupil Size (a.u.)"
  }
  
  ggplot(data, aes_string(x = "Time", y = left_col)) +
    geom_point(colour = "darkgrey") +
    geom_line() +
    geom_point(aes_string(y = right_col), colour = "hotpink") +
    geom_line(aes_string(y = right_col)) +
    labs(title = title, x = "Time (ms)", y = y_label)
}

create_pupil_plot_both_baseline <- function(data, title, units = "mm") {
  if (units == "mm") {
    left_col  <- "L_Pupil_Diameter_bc.mm"
    right_col <- "R_Pupil_Diameter_bc.mm"
    y_label   <- "Baseline-Corrected Pupil Size (mm)"
  } else {
    left_col  <- "L_Pupil_Diameter_bc.px"
    right_col <- "R_Pupil_Diameter_bc.px"
    y_label   <- "Baseline-Corrected Pupil Size (a.u.)"
  }
  
  ggplot(data, aes_string(x = "Time", y = left_col)) +
    geom_point(colour = "darkgrey") +
    geom_line() +
    geom_point(aes_string(y = right_col), colour = "hotpink") +
    geom_line(aes_string(y = right_col)) +
    labs(title = title, x = "Time (ms)", y = y_label)
}

create_pupil_plot_lrm <- function(data, title, units = "mm") {
  if (units == "mm") {
    left_col  <- "L_Pupil_Diameter.mm"
    right_col <- "R_Pupil_Diameter.mm"
    mean_col  <- "Pupil_Diameter.mm"
    y_label   <- "Pupil Size (mm)"
  } else {
    left_col  <- "L_Pupil_Diameter.px"
    right_col <- "R_Pupil_Diameter.px"
    mean_col  <- "Pupil_Diameter.px"
    y_label   <- "Pupil Size (a.u.)"
  }
  
  ggplot(data, aes_string(x = "Time", y = left_col)) +
    geom_point(colour = "darkgrey") + geom_line() +
    geom_point(aes_string(y = right_col), colour = "hotpink") +
    geom_line(aes_string(y = right_col)) +
    geom_point(aes_string(y = mean_col), colour = "cyan") +
    geom_line(aes_string(y = mean_col)) +
    labs(title = title, x = "Time (ms)", y = y_label)
}

create_pupil_plot_m <- function(data, title, units = "mm") {
  if (units == "mm") {
    mean_col <- "Pupil_Diameter_bc.mm"
    y_label  <- "Baseline-Corrected Pupil Size (mm)"
  } else {
    mean_col <- "Pupil_Diameter_bc.px"
    y_label  <- "Baseline-Corrected Pupil Size (a.u.)"
  }
  
  ggplot(data, aes_string(x = "Time", y = mean_col)) +
    geom_point(colour = "cyan") + geom_line() +
    labs(title = title, x = "Time (ms)", y = y_label)
}


# -----------------------------------------------------------------------------
# 1b. Gaze-corrected plotting helpers (NEW)
#
# Two plots are produced per trial when apply_gaze_corr = TRUE:
#
#   create_pupil_plot_gc_lrm()  – left, right, and mean gaze-corrected pupil
#                                  traces on the same axes (mirrors lrm_plot)
#   create_pupil_plot_gc_m()    – mean gaze-corrected trace only, overlaid with
#                                  the uncorrected mean so the effect of the
#                                  correction is immediately visible
#
# Both functions expect the *_gc columns added by apply_gaze_correction().
# -----------------------------------------------------------------------------

create_pupil_plot_gc_lrm <- function(data, title, units = "mm") {
  if (units == "mm") {
    left_gc  <- "L_Pupil_Diameter_gc.mm"
    right_gc <- "R_Pupil_Diameter_gc.mm"
    mean_gc  <- "Pupil_Diameter_gc.mm"
    y_label  <- "Gaze-Corrected Pupil Size (mm)"
  } else {
    left_gc  <- "L_Pupil_Diameter_gc.px"
    right_gc <- "R_Pupil_Diameter_gc.px"
    mean_gc  <- "Pupil_Diameter_gc.px"
    y_label  <- "Gaze-Corrected Pupil Size (a.u.)"
  }
  
  ggplot(data, aes_string(x = "Time", y = left_gc)) +
    geom_point(colour = "darkgrey") +
    geom_line() +
    geom_point(aes_string(y = right_gc), colour = "hotpink") +
    geom_line(aes_string(y = right_gc)) +
    geom_point(aes_string(y = mean_gc), colour = "cyan") +
    geom_line(aes_string(y = mean_gc)) +
    labs(title = title, x = "Time (ms)", y = y_label)
}

create_pupil_plot_gc_m <- function(data, title, units = "mm") {
  # Overlays the gaze-corrected mean (cyan) on the uncorrected mean (grey)
  # so the magnitude of the correction can be assessed visually.
  if (units == "mm") {
    mean_col <- "Pupil_Diameter.mm"
    mean_gc  <- "Pupil_Diameter_gc.mm"
    y_label  <- "Pupil Size (mm)"
  } else {
    mean_col <- "Pupil_Diameter.px"
    mean_gc  <- "Pupil_Diameter_gc.px"
    y_label  <- "Pupil Size (a.u.)"
  }
  
  ggplot(data, aes_string(x = "Time", y = mean_col)) +
    geom_line(colour = "grey60", linetype = "dashed") +
    geom_point(colour = "grey60", size = 0.8) +
    geom_line(aes_string(y = mean_gc), colour = "cyan") +
    geom_point(aes_string(y = mean_gc), colour = "cyan", size = 0.8) +
    labs(title = title,
         x = "Time (ms)", y = y_label,
         caption = "Grey dashed = uncorrected mean  |  Cyan = gaze-corrected mean")
}


# save_gaze_corrected_plots()
#
# Called once in the main function after apply_gaze_correction() has added
# the *_gc columns.  Iterates over every subject × trial present in
# all_final_data and saves two PNGs per trial into the existing subject
# subdirectories under plot_dir.  Also saves a combined two-panel plot
# (lrm + overlay) as "13_gc_all_steps.png" so it sits alongside the
# existing "all_steps.png" for each trial.
#
# Parameters
#   all_final_data  – full processed data frame with *_gc columns already added
#   plot_dir        – root plot directory (same as passed to the main function)
#   units           – "mm" or "px"
#   save_individual – if FALSE only the combined panel is saved (default TRUE)

save_gaze_corrected_plots <- function(all_final_data, plot_dir,
                                      units = "mm",
                                      save_individual = TRUE) {
  
  combos <- unique(all_final_data[, c("Subject", "Trial")])
  
  for (i in seq_len(nrow(combos))) {
    subj <- combos$Subject[i]
    tr   <- combos$Trial[i]
    
    trial_data <- all_final_data %>%
      filter(Subject == subj, Trial == tr)
    
    subj_dir <- file.path(plot_dir, paste0("S", subj))
    dir.create(subj_dir, recursive = TRUE, showWarnings = FALSE)
    
    gc_lrm_plot <- create_pupil_plot_gc_lrm(
      trial_data,
      title = "Gaze-Corrected – Left, Right & Mean Pupil",
      units = units
    )
    
    gc_m_plot <- create_pupil_plot_gc_m(
      trial_data,
      title = "Gaze-Corrected vs Uncorrected Mean Pupil",
      units = units
    )
    
    if (save_individual) {
      save_plot(gc_lrm_plot, subj_dir, subj, tr, "13_gc_lrm")
      save_plot(gc_m_plot,   subj_dir, subj, tr, "14_gc_mean_overlay")
    }
    
    # Combined two-panel plot saved regardless of save_individual
    gc_combined <- wrap_plots(
      list(gc_lrm_plot, gc_m_plot),
      ncol = 2, guides = "collect", axes = "collect"
    )
    save_plot(gc_combined, subj_dir, subj, tr,
              "gc_all_steps", width = 14, height = 6)
  }
  
  message("Gaze-corrected plots saved to: ", plot_dir)
}


# -----------------------------------------------------------------------------
# 2. Save helpers (unchanged)
# -----------------------------------------------------------------------------

save_plot <- function(plot, subj_dir, subj, tr, step_name,
                      width = 10, height = 6) {
  filename <- paste0("S", subj, "_T", tr, "_", step_name, ".png")
  ggsave(file.path(subj_dir, filename), plot, width = width, height = height)
  return(invisible(plot))
}

save_step_data <- function(data, output_dir, subj, tr, step_name) {
  filename <- paste0(step_name, "_S", subj, "_T", tr, ".csv")
  write_csv(data, file.path(output_dir, "steps", filename))
  return(invisible(data))
}


# -----------------------------------------------------------------------------
# 3. NEW – Butterworth low-pass filter applied to one pupil column
#
# Uses signal::butter() and signal::filtfilt() for zero-phase filtering.
# Because filtfilt() requires a minimum signal length relative to the filter
# order, the function silently falls back to returning the raw values when
# the trial is too short (< 3 * order samples after removing NAs).
#
# Parameters
#   x          numeric vector (one pupil column for one trial, may contain NA)
#   cutoff_hz  low-pass cutoff in Hz (default 8)
#   hz         sampling rate of the (upsampled) data in Hz (default 1000)
#   order      Butterworth filter order (default 4)
# -----------------------------------------------------------------------------

butterworth_lowpass <- function(x, cutoff_hz = 8, hz = 1000, order = 4) {
  if (!requireNamespace("signal", quietly = TRUE))
    stop("Package 'signal' is required for the Butterworth filter. ",
         "Install with: install.packages('signal')")
  
  # Normalised cutoff (0–1, where 1 = Nyquist)
  W  <- cutoff_hz / (hz / 2)
  bf <- signal::butter(order, W, type = "low")
  
  valid <- !is.na(x)
  n_valid <- sum(valid)
  
  # filtfilt needs at least 3 * (max filter coef length) samples
  min_len <- 3 * max(length(bf$b), length(bf$a))
  if (n_valid < min_len) {
    warning("Trial too short for Butterworth filter (", n_valid,
            " valid samples, need >= ", min_len, "). Returning raw values.")
    return(x)
  }
  
  out <- x
  # Filter only on the valid (non-NA) segment; preserve NA positions
  out[valid] <- signal::filtfilt(bf, x[valid])
  return(out)
}


# -----------------------------------------------------------------------------
# 4. NEW – split missing-data check
#
# Applies separate missingness thresholds to the baseline period and the
# stimulus period.  Returns the data unchanged if missingness is within
# tolerance, and returns NULL (signalling trial rejection) if either window
# exceeds its threshold.
#
# The function expects the Stimulus column to carry:
#   "fixation_baseline" (or containing that string) during baseline
#   "target" / "Target"                              during stimulus
#
# UPDATED: the baseline window is now restricted to "fixation_baseline" rows
# falling within `baseline_duration` ms immediately preceding target onset —
# i.e. [target_onset - baseline_duration, target_onset) — matching the window
# used later by fill_baseline_bc() / calculate_pupil_rate() for the actual
# baseline correction. Previously this checked *every* row in the trial
# labelled "fixation_baseline", regardless of its distance from target onset.
#
# If no "target" row is found in the trial (target_onset is NA), there is no
# well-defined baseline window, so the function falls back to using every
# "fixation_baseline" row in the trial for the baseline check (same as the
# old behaviour) rather than silently passing with zero rows checked.
#
# Parameters
#   data                     trial-level data frame
#   pupil_col                name of the pupil column to check (left eye as proxy)
#   missing_allowed_baseline max proportion of NA allowed in baseline (0–1)
#   missing_allowed_stimulus max proportion of NA allowed in stimulus (0–1)
#   baseline_duration        length (ms) of the baseline window immediately
#                             preceding target onset (default 500)
# -----------------------------------------------------------------------------

check_missing_split <- function(data, pupil_col,
                                missing_allowed_baseline = 0.20,
                                missing_allowed_stimulus = 0.30,
                                baseline_duration = 500) {
  
  # --- Locate target onset (needed to window the baseline period) ---
  target_rows  <- grepl("^[Tt]arget$", data$Stimulus)
  target_onset <- if (any(target_rows)) min(data$Time[target_rows]) else NA
  
  # --- Baseline window: fixation_baseline rows within baseline_duration
  #     immediately preceding target onset ---
  if (!is.na(target_onset)) {
    bl_rows <- grepl("fixation_baseline", data$Stimulus, ignore.case = TRUE) &
      data$Time >= (target_onset - baseline_duration) &
      data$Time <  target_onset
  } else {
    # No target found in this trial — fall back to all fixation_baseline rows
    bl_rows <- grepl("fixation_baseline", data$Stimulus, ignore.case = TRUE)
  }
  
  bl_data <- data[[pupil_col]][bl_rows]
  if (length(bl_data) > 0) {
    prop_miss_bl <- mean(is.na(bl_data))
    if (prop_miss_bl > missing_allowed_baseline) {
      message("    Trial rejected: baseline missing proportion = ",
              round(prop_miss_bl, 3),
              " > threshold ", missing_allowed_baseline)
      return(NULL)
    }
  }
  
  # --- Stimulus window (from first "target" onset to end of trial) ---
  if (!is.na(target_onset)) {
    stim_data    <- data[[pupil_col]][data$Time >= target_onset]
    prop_miss_st <- mean(is.na(stim_data))
    if (prop_miss_st > missing_allowed_stimulus) {
      message("    Trial rejected: stimulus missing proportion = ",
              round(prop_miss_st, 3),
              " > threshold ", missing_allowed_stimulus)
      return(NULL)
    }
  }
  
  return(data)   # passes both checks
}


# -----------------------------------------------------------------------------
# 5. NEW – gaze-position correction
#
# Regresses pupil size on gaze X and Y coordinates and subtracts the fitted
# component, yielding a "gaze-corrected" residual trace.  Applied per trial
# (default) or per subject across all trials.
#
# Adds columns:
#   L_Pupil_Diameter_gc.mm / R_Pupil_Diameter_gc.mm  (gaze-corrected)
#   Pupil_Diameter_gc.mm                               (mean of the two)
#
# See the extended note at the bottom of this file for methodological detail
# and caveats.
#
# Parameters
#   data       full processed data frame (all subjects, all trials)
#   units      "mm" or "px"
#   per_trial  if TRUE (default) fit the regression within each trial;
#              if FALSE fit within each subject across all trials
#   x_col_L / y_col_L  gaze column names for left eye
#   x_col_R / y_col_R  gaze column names for right eye
# -----------------------------------------------------------------------------

apply_gaze_correction <- function(data,
                                  units      = "mm",
                                  per_trial  = TRUE,
                                  x_col_L    = "L_Gaze_Position.x",
                                  y_col_L    = "L_Gaze_Position.y",
                                  x_col_R    = "R_Gaze_Position.x",
                                  y_col_R    = "R_Gaze_Position.y") {
  
  # Column names
  if (units == "mm") {
    l_col <- "L_Pupil_Diameter.mm";  r_col <- "R_Pupil_Diameter.mm"
    l_gc  <- "L_Pupil_Diameter_gc.mm"; r_gc <- "R_Pupil_Diameter_gc.mm"
    m_gc  <- "Pupil_Diameter_gc.mm"
  } else {
    l_col <- "L_Pupil_Diameter.px";  r_col <- "R_Pupil_Diameter.px"
    l_gc  <- "L_Pupil_Diameter_gc.px"; r_gc <- "R_Pupil_Diameter_gc.px"
    m_gc  <- "Pupil_Diameter_gc.px"
  }
  
  # Check that gaze columns exist
  needed_gaze <- c(x_col_L, y_col_L, x_col_R, y_col_R)
  missing_gaze <- needed_gaze[!needed_gaze %in% names(data)]
  if (length(missing_gaze) > 0)
    stop("Gaze columns not found: ", paste(missing_gaze, collapse = ", "),
         "\nAdjust x_col_L/y_col_L/x_col_R/y_col_R arguments.")
  
  result <- data
  result[[l_gc]] <- NA_real_
  result[[r_gc]] <- NA_real_
  result[[m_gc]] <- NA_real_
  
  # Define grouping
  combos <- if (per_trial) {
    unique(data[, c("Subject", "Trial")])
  } else {
    data.frame(Subject = unique(data$Subject), Trial = NA_character_)
  }
  
  for (i in seq_len(nrow(combos))) {
    subj <- combos$Subject[i]
    if (per_trial) {
      tr  <- combos$Trial[i]
      idx <- which(data$Subject == subj & data$Trial == tr)
    } else {
      idx <- which(data$Subject == subj)
    }
    
    d <- data[idx, ]
    
    regress_eye <- function(pupil, x_gaze, y_gaze) {
      valid <- !is.na(pupil) & !is.na(x_gaze) & !is.na(y_gaze)
      if (sum(valid) < 5) return(rep(NA_real_, length(pupil)))
      fit <- lm(pupil[valid] ~ x_gaze[valid] + y_gaze[valid])
      # Residual = actual – fitted gaze component, re-centred to the mean
      # pupil size so the output is on the original scale.
      resid_full           <- rep(NA_real_, length(pupil))
      fitted_gaze          <- coef(fit)[1] +
        coef(fit)[2] * x_gaze +
        coef(fit)[3] * y_gaze
      resid_full           <- pupil - fitted_gaze + mean(pupil[valid])
      resid_full
    }
    
    result[[l_gc]][idx] <- regress_eye(d[[l_col]], d[[x_col_L]], d[[y_col_L]])
    result[[r_gc]][idx] <- regress_eye(d[[r_col]], d[[x_col_R]], d[[y_col_R]])
    result[[m_gc]][idx] <- rowMeans(cbind(result[[l_gc]][idx],
                                          result[[r_gc]][idx]), na.rm = TRUE)
  }
  
  return(result)
}


# -----------------------------------------------------------------------------
# 6. process_step_both – now routes to butterworth smoother if requested
# -----------------------------------------------------------------------------

process_step_both <- function(data, step_name, plot_title, process_fn, params,
                              subj, tr, subj_dir, output_dir,
                              save_intermediate_data, save_individual_plots,
                              units = "mm",
                              # extra args passed through for custom smooth
                              smooth_type = "hann",
                              butterworth_cutoff_hz = 8,
                              butterworth_order = 4,
                              data_hz = 1000) {
  
  # --- Special handling: Butterworth smoothing ---
  if (process_fn == "pupil_smooth" && smooth_type == "butterworth") {
    
    if (units == "mm") {
      l_col <- "L_Pupil_Diameter.mm"; r_col <- "R_Pupil_Diameter.mm"
    } else {
      l_col <- "L_Pupil_Diameter.px"; r_col <- "R_Pupil_Diameter.px"
    }
    
    result <- data
    for (col in c(l_col, r_col)) {
      if (col %in% names(result)) {
        result[[col]] <- butterworth_lowpass(result[[col]],
                                             cutoff_hz = butterworth_cutoff_hz,
                                             hz        = data_hz,
                                             order     = butterworth_order)
      }
    }
    
  } else {
    # --- Standard pupillometry pipeline call ---
    result <- do.call(process_fn, c(list(data), params))
  }
  
  # Plot
  if (process_fn == "pupil_baselinecorrect") {
    plot <- create_pupil_plot_both_baseline(result, plot_title, units = units)
  } else {
    plot <- create_pupil_plot_both(result, plot_title, units = units)
  }
  
  if (save_intermediate_data) save_step_data(result, output_dir, subj, tr, step_name)
  if (save_individual_plots)  save_plot(plot, subj_dir, subj, tr, step_name)
  
  list(data = result, plot = plot)
}


# -----------------------------------------------------------------------------
# 7. calculate_mean_pupil (unchanged)
# -----------------------------------------------------------------------------

calculate_mean_pupil <- function(data, units = "mm") {
  if (units == "mm") {
    left_col <- "L_Pupil_Diameter.mm";  right_col <- "R_Pupil_Diameter.mm"
    mean_col <- "Pupil_Diameter.mm"
    left_col_bc <- "L_Pupil_Diameter_bc.mm"; right_col_bc <- "R_Pupil_Diameter_bc.mm"
    mean_col_bc <- "Pupil_Diameter_bc.mm"
  } else {
    left_col <- "L_Pupil_Diameter.px";  right_col <- "R_Pupil_Diameter.px"
    mean_col <- "Pupil_Diameter.px"
    left_col_bc <- "L_Pupil_Diameter_bc.px"; right_col_bc <- "R_Pupil_Diameter_bc.px"
    mean_col_bc <- "Pupil_Diameter_bc.px"
  }
  
  left_col_z  <- "L_Pupil_Diameter_bc.z"
  right_col_z <- "R_Pupil_Diameter_bc.z"
  mean_col_z  <- "Pupil_Diameter_bc.z"
  
  result <- data %>%
    rowwise() %>%
    mutate(
      mean_pupil    = mean(c(!!sym(left_col),    !!sym(right_col)),    na.rm = TRUE),
      mean_pupil_bc = mean(c(!!sym(left_col_bc), !!sym(right_col_bc)), na.rm = TRUE),
      mean_pupil_z  = mean(c(!!sym(left_col_z),  !!sym(right_col_z)),  na.rm = TRUE)
    ) %>%
    ungroup() %>%
    rename_with(~mean_col,    "mean_pupil") %>%
    rename_with(~mean_col_bc, "mean_pupil_bc") %>%
    rename_with(~mean_col_z,  "mean_pupil_z") %>%
    relocate(all_of(mean_col),    .after = all_of(right_col)) %>%
    relocate(all_of(mean_col_bc), .after = all_of(right_col_bc)) %>%
    relocate(all_of(mean_col_z),  .after = all_of(right_col_z))
  
  return(result)
}


# -----------------------------------------------------------------------------
# 8. calculate_pupil_rate (unchanged – still uses Hann for the rate signal)
# -----------------------------------------------------------------------------

calculate_pupil_rate <- function(data, smooth_window_ms = 250,
                                 baseline_duration = 500, units = "mm") {
  
  mean_col           <- if (units == "mm") "Pupil_Diameter.mm"                    else "Pupil_Diameter.px"
  rate_col           <- if (units == "mm") "Pupil_Diameter_change.mm"             else "Pupil_Diameter_change.px"
  rate_smooth_col    <- if (units == "mm") "Pupil_Diameter_change_smoothed.mm"    else "Pupil_Diameter_change_smoothed.px"
  rate_smooth_bc_col <- if (units == "mm") "Pupil_Diameter_change_smoothed_bc.mm" else "Pupil_Diameter_change_smoothed_bc.px"
  
  hann_smooth <- function(x, n) {
    if (n <= 1) return(x)
    w    <- 0.5 * (1 - cos(2 * pi * seq(0, n - 1) / (n - 1)))
    w    <- w / sum(w)
    half <- floor(n / 2)
    result <- rep(NA_real_, length(x))
    for (i in seq_along(x)) {
      lo <- i - half; hi <- lo + n - 1
      lo_c <- max(1, lo); hi_c <- min(length(x), hi)
      w_idx <- (lo_c - lo + 1):(hi_c - lo + 1)
      vals  <- x[lo_c:hi_c]; valid <- !is.na(vals)
      if (any(valid))
        result[i] <- sum(w[w_idx][valid] * vals[valid]) / sum(w[w_idx][valid])
    }
    result
  }
  
  result <- data
  result[[rate_col]]           <- NA_real_
  result[[rate_smooth_col]]    <- NA_real_
  result[[rate_smooth_bc_col]] <- NA_real_
  
  combos <- unique(data[, c("Subject", "Trial")])
  
  for (i in seq_len(nrow(combos))) {
    subj <- combos$Subject[i]; tr <- combos$Trial[i]
    idx  <- which(data$Subject == subj & data$Trial == tr)
    trial_data <- data[idx, ]
    pupil      <- trial_data[[mean_col]]
    
    dt_ms     <- if (nrow(trial_data) > 1) median(diff(trial_data$Time), na.rm = TRUE) else 1
    n_samples <- max(3, round(smooth_window_ms / dt_ms))
    if (n_samples %% 2 == 0) n_samples <- n_samples + 1
    
    rate         <- c(NA_real_, diff(pupil))
    rate_smooth  <- hann_smooth(rate, n_samples)
    
    target_rows  <- trial_data[grepl("^[Tt]arget$", trial_data$Stimulus), ]
    target_onset <- if (nrow(target_rows) > 0) min(target_rows$Time) else NA
    
    if (!is.na(target_onset)) {
      bl_mask        <- grepl("fixation_baseline", trial_data$Stimulus) &
        trial_data$Time >= (target_onset - baseline_duration) &
        trial_data$Time <  target_onset
      bl_rate_mean   <- mean(rate_smooth[bl_mask], na.rm = TRUE)
      rate_smooth_bc <- rate_smooth - bl_rate_mean
    } else {
      rate_smooth_bc <- rep(NA_real_, length(rate_smooth))
    }
    
    result[[rate_col]][idx]           <- rate
    result[[rate_smooth_col]][idx]    <- rate_smooth
    result[[rate_smooth_bc_col]][idx] <- rate_smooth_bc
  }
  
  result %>%
    relocate(all_of(rate_col),           .after = all_of(mean_col)) %>%
    relocate(all_of(rate_smooth_col),    .after = all_of(rate_col)) %>%
    relocate(all_of(rate_smooth_bc_col), .after = all_of(rate_smooth_col))
}


# -----------------------------------------------------------------------------
# 9. fill_baseline_bc (unchanged)
# -----------------------------------------------------------------------------

fill_baseline_bc <- function(data, baseline_duration = 500, units = "mm") {
  
  if (units == "mm") {
    bc_cols  <- c("L_Pupil_Diameter_bc.mm", "R_Pupil_Diameter_bc.mm", "Pupil_Diameter_bc.mm")
    raw_cols <- c("L_Pupil_Diameter.mm",    "R_Pupil_Diameter.mm",    "Pupil_Diameter.mm")
  } else {
    bc_cols  <- c("L_Pupil_Diameter_bc.px", "R_Pupil_Diameter_bc.px", "Pupil_Diameter_bc.px")
    raw_cols <- c("L_Pupil_Diameter.px",    "R_Pupil_Diameter.px",    "Pupil_Diameter.px")
  }
  z_cols <- c("L_Pupil_Diameter_bc.z", "R_Pupil_Diameter_bc.z", "Pupil_Diameter_bc.z")
  
  result <- data
  combos <- unique(data[, c("Subject", "Trial")])
  
  for (i in seq_len(nrow(combos))) {
    subj <- combos$Subject[i]; tr <- combos$Trial[i]
    idx  <- which(data$Subject == subj & data$Trial == tr)
    trial_data   <- data[idx, ]
    
    target_rows  <- trial_data[grepl("^[Tt]arget$", trial_data$Stimulus), ]
    if (nrow(target_rows) == 0) next
    target_onset <- min(target_rows$Time)
    
    bl_window_idx <- which(
      grepl("fixation_baseline", trial_data$Stimulus) &
        trial_data$Time >= (target_onset - baseline_duration) &
        trial_data$Time <  target_onset
    )
    if (length(bl_window_idx) == 0) next
    
    for (j in seq_along(bc_cols)) {
      bc_col  <- bc_cols[j]; raw_col <- raw_cols[j]; z_col <- z_cols[j]
      if (!bc_col %in% names(data)) next
      
      bl_raw        <- trial_data[[raw_col]][bl_window_idx]
      baseline_mean <- mean(bl_raw, na.rm = TRUE)
      bc_vals       <- bl_raw - baseline_mean
      
      tgt_bc <- target_rows[[bc_col]]; tgt_z <- target_rows[[z_col]]
      valid  <- !is.na(tgt_bc) & !is.na(tgt_z)
      if (sum(valid) >= 2) {
        fit       <- lm(tgt_bc[valid] ~ tgt_z[valid])
        mu_est    <- coef(fit)[1]; sigma_est <- coef(fit)[2]
        z_vals    <- (bc_vals - mu_est) / sigma_est
      } else {
        z_vals <- rep(NA_real_, length(bc_vals))
      }
      
      global_idx                        <- idx[bl_window_idx]
      result[[bc_col]][global_idx]      <- bc_vals
      result[[z_col]][global_idx]       <- z_vals
    }
  }
  
  return(result)
}


# -----------------------------------------------------------------------------
# 10. process_trial_both – updated to support split missing check and
#     Butterworth smoothing
# -----------------------------------------------------------------------------

process_trial_both <- function(trial_data, subj, tr, subj_dir, output_dir,
                               params,
                               save_intermediate_data, save_individual_plots,
                               return_all_steps,
                               bin_length          = 20,
                               smooth_then_interp  = TRUE,
                               units               = "mm",
                               rate_smooth_window_ms = 250) {
  
  if (nrow(trial_data) < 10) {
    message("    Skipping: Not enough data points")
    return(NULL)
  }
  
  # Determine which pupil column to use for the missing-data check
  pupil_check_col <- if (units == "mm") "L_Pupil_Diameter.mm" else "L_Pupil_Diameter.px"
  
  tryCatch({
    # Raw
    raw_data <- trial_data
    raw_plot <- create_pupil_plot_both(raw_data, "Raw Pupil Data", units = units)
    if (save_intermediate_data) save_step_data(raw_data, output_dir, subj, tr, "1_raw")
    if (save_individual_plots)  save_plot(raw_plot, subj_dir, subj, tr, "1_raw")
    
    deblink <- process_step_both(raw_data, "2_deblink", "After Deblinking",
                                 "pupil_deblink", params$deblink,
                                 subj, tr, subj_dir, output_dir,
                                 save_intermediate_data, save_individual_plots,
                                 units = units)
    
    artifact1 <- process_step_both(deblink$data, "3_artifact1", "After First Artifact Removal",
                                   "pupil_artifact", params$artifact,
                                   subj, tr, subj_dir, output_dir,
                                   save_intermediate_data, save_individual_plots,
                                   units = units)
    
    artifact2 <- process_step_both(artifact1$data, "4_artifact2", "After Second Artifact Removal",
                                   "pupil_artifact", params$artifact,
                                   subj, tr, subj_dir, output_dir,
                                   save_intermediate_data, save_individual_plots,
                                   units = units)
    
    # ---- MISSING DATA CHECK (split or flat) ----
    if (!is.null(params$missing$missing_allowed_baseline) ||
        !is.null(params$missing$missing_allowed_stimulus)) {
      
      # Split thresholds
      bl_thr   <- params$missing$missing_allowed_baseline %||% 0.20
      st_thr   <- params$missing$missing_allowed_stimulus %||% 0.30
      # Baseline window length (ms) preceding target onset used for the
      # baseline portion of this check — defaults to params$baseline's
      # baseline_duration so it matches the actual baseline-correction window.
      bl_dur   <- params$missing$baseline_duration %||%
        params$baseline$baseline_duration %||% 500
      
      checked <- check_missing_split(artifact2$data,
                                     pupil_col                = pupil_check_col,
                                     missing_allowed_baseline = bl_thr,
                                     missing_allowed_stimulus = st_thr,
                                     baseline_duration        = bl_dur)
      if (is.null(checked)) return(NULL)
      missing <- list(data = checked,
                      plot = create_pupil_plot_both(checked,
                                                    "After Missing Data Check",
                                                    units = units))
      if (save_intermediate_data)
        save_step_data(missing$data, output_dir, subj, tr, "5_missing")
      if (save_individual_plots)
        save_plot(missing$plot, subj_dir, subj, tr, "5_missing")
      
    } else {
      # Original flat threshold behaviour
      missing <- process_step_both(artifact2$data, "5_missing",
                                   "After Missing Data Check",
                                   "pupil_missing", params$missing,
                                   subj, tr, subj_dir, output_dir,
                                   save_intermediate_data, save_individual_plots,
                                   units = units)
    }
    
    upsample <- process_step_both(missing$data, "6_upsample", "After Upsampling",
                                  "pupil_upsample", params$upsample,
                                  subj, tr, subj_dir, output_dir,
                                  save_intermediate_data, save_individual_plots,
                                  units = units)
    
    # ---- SMOOTHING (Hann or Butterworth) ----
    smooth_type <- params$smooth$type %||% "hann"
    bw_cutoff   <- params$smooth$cutoff_hz %||% 8
    bw_order    <- params$smooth$order     %||% 4
    data_hz     <- params$interpolate$hz   %||% 1000
    
    if (smooth_then_interp) {
      smooth <- process_step_both(upsample$data, "7_smooth", "After Smoothing",
                                  "pupil_smooth", params$smooth,
                                  subj, tr, subj_dir, output_dir,
                                  save_intermediate_data, save_individual_plots,
                                  units = units,
                                  smooth_type           = smooth_type,
                                  butterworth_cutoff_hz = bw_cutoff,
                                  butterworth_order     = bw_order,
                                  data_hz               = data_hz)
      
      interpolate <- process_step_both(smooth$data, "8_interpolate", "After Interpolation",
                                       "pupil_interpolate", params$interpolate,
                                       subj, tr, subj_dir, output_dir,
                                       save_intermediate_data, save_individual_plots,
                                       units = units)
    } else {
      interpolate <- process_step_both(upsample$data, "7_interpolate", "After Interpolation",
                                       "pupil_interpolate", params$interpolate,
                                       subj, tr, subj_dir, output_dir,
                                       save_intermediate_data, save_individual_plots,
                                       units = units)
      
      smooth <- process_step_both(interpolate$data, "8_smooth", "After Smoothing",
                                  "pupil_smooth", params$smooth,
                                  subj, tr, subj_dir, output_dir,
                                  save_intermediate_data, save_individual_plots,
                                  units = units,
                                  smooth_type           = smooth_type,
                                  butterworth_cutoff_hz = bw_cutoff,
                                  butterworth_order     = bw_order,
                                  data_hz               = data_hz)
    }
    
    final <- process_step_both(interpolate$data, "9_final", "Final Processed Data",
                               "pupil_missing", list(),
                               subj, tr, subj_dir, output_dir,
                               save_intermediate_data, save_individual_plots,
                               units = units)
    
    baseline <- process_step_both(final$data, "10_baselined", "After Baseline Correction",
                                  "pupil_baselinecorrect", params$baseline,
                                  subj, tr, subj_dir, output_dir,
                                  save_intermediate_data, save_individual_plots,
                                  units = units)
    
    mean_data <- calculate_mean_pupil(baseline$data, units = units)
    mean_data <- fill_baseline_bc(mean_data,
                                  baseline_duration = params$baseline$baseline_duration,
                                  units = units)
    mean_data <- calculate_pupil_rate(mean_data,
                                      smooth_window_ms  = rate_smooth_window_ms,
                                      baseline_duration = params$baseline$baseline_duration,
                                      units = units)
    
    lrm_plot <- create_pupil_plot_lrm(mean_data, "Left, Right and Mean Pupil",  units = units)
    m_plot   <- create_pupil_plot_m(mean_data,   "Mean After Baseline Correction", units = units)
    if (save_intermediate_data) save_step_data(mean_data, output_dir, subj, tr, "11_mean")
    if (save_individual_plots) {
      save_plot(lrm_plot, subj_dir, subj, tr, "11_lrm")
      save_plot(m_plot,   subj_dir, subj, tr, "12_mean_bc")
    }
    
    binned_data <- pupil_bin(mean_data, bin_length = bin_length)
    
    if (smooth_then_interp) {
      all_plots <- wrap_plots(
        list(raw_plot, deblink$plot, artifact1$plot, artifact2$plot,
             missing$plot, upsample$plot, smooth$plot, interpolate$plot,
             final$plot, baseline$plot, lrm_plot, m_plot),
        ncol = 4, guides = "collect", axes = "collect")
    } else {
      all_plots <- wrap_plots(
        list(raw_plot, deblink$plot, artifact1$plot, artifact2$plot,
             missing$plot, upsample$plot, interpolate$plot, smooth$plot,
             final$plot, baseline$plot, lrm_plot, m_plot),
        ncol = 4, guides = "collect", axes = "collect")
    }
    save_plot(all_plots, subj_dir, subj, tr, "all_steps", width = 18, height = 12)
    
    if (return_all_steps) {
      return(list(
        subject = subj, trial = tr,
        raw_data = raw_data, deblink_data = deblink$data,
        artifact1_data = artifact1$data, artifact2_data = artifact2$data,
        missing_data = missing$data, upsample_data = upsample$data,
        smooth_data = smooth$data, interpolate_data = interpolate$data,
        final_data = final$data, baseline_data = baseline$data,
        mean_data = mean_data, binned_data = binned_data))
    } else {
      return(list(subject = subj, trial = tr,
                  final_data = mean_data, binned_data = binned_data))
    }
    
  }, error = function(e) {
    message("    Error: Subject ", subj, " Trial ", tr, ": ", e$message)
    return(NULL)
  })
}


# -----------------------------------------------------------------------------
# 11. Main function – updated params signature
# -----------------------------------------------------------------------------

preprocess_and_visualize_both <- function(
    pupil_data,
    plot_dir   = here("plots",  "preprocessing"),
    output_dir = here("data",   "preprocessing"),
    params = list(
      deblink   = list(extend = 75),
      artifact  = list(n = 8),
      
      # ---- missing data ----
      # Option A – original flat threshold (keep if you don't need split):
      #   missing = list(missing_allowed = 0.90)
      #
      # Option B – split thresholds (NEW):
      #   baseline_duration below controls how far back from target onset the
      #   baseline window for the *rejection check* extends. If omitted, it
      #   falls back to params$baseline$baseline_duration (see process_trial_both).
      missing = list(
        missing_allowed_baseline = 0.20,   # ≤20 % missing during baseline
        missing_allowed_stimulus = 0.30,   # ≤30 % missing during stimulus
        baseline_duration        = 500     # ms preceding target onset
      ),
      
      upsample  = list(),
      
      # ---- smoothing ----
      # Option A – Hann window (original):
      #   smooth = list(type = "hann", n = 50)
      #
      # Option B – Butterworth low-pass (NEW):
      smooth = list(
        type       = "butterworth",
        cutoff_hz  = 8,    # low-pass cutoff in Hz  (try 4–8 Hz)
        order      = 4     # filter order (4 is a good default)
        # n is ignored for butterworth; leave it absent or set it anyway
        # as it will be passed to pupil_smooth() only for "hann"
      ),
      
      interpolate = list(type = "cubic-spline", maxgap = 500, hz = 1000),
      baseline    = list(bc_onset_message  = "Target",
                         baseline_duration = 500,
                         type             = "subtractive")
    ),
    save_intermediate_data = TRUE,
    save_individual_plots  = FALSE,
    return_all_steps       = TRUE,
    bin_length             = 20,
    smooth_then_interp     = TRUE,
    units                  = "mm",
    rate_smooth_window_ms  = 250,
    # Gaze correction: set to TRUE to apply after all processing
    apply_gaze_corr        = FALSE,
    gaze_per_trial         = TRUE) {
  
  dir.create(plot_dir,   recursive = TRUE, showWarnings = FALSE)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  if (save_intermediate_data)
    dir.create(file.path(output_dir, "steps"), recursive = TRUE,
               showWarnings = FALSE)
  
  if (units == "mm") {
    left_pupil_col  <- "L_Pupil_Diameter.mm"
    right_pupil_col <- "R_Pupil_Diameter.mm"
  } else {
    left_pupil_col  <- "L_Pupil_Diameter.px"
    right_pupil_col <- "R_Pupil_Diameter.px"
  }
  
  necessary_cols <- c("Subject", "Trial", "Time", left_pupil_col, right_pupil_col)
  if (!all(necessary_cols %in% colnames(pupil_data))) {
    missing_cols <- necessary_cols[!necessary_cols %in% colnames(pupil_data)]
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  pupil_data <- pupil_data %>%
    mutate(Eye_Event = "Fixation", L_Eye_Event = "Fixation", R_Eye_Event = "Fixation")
  
  results_list   <- list()
  unique_subjects <- unique(pupil_data$Subject)
  
  for (subj in unique_subjects) {
    message("Processing Subject: ", subj)
    subj_dir  <- file.path(plot_dir, paste0("S", subj))
    dir.create(subj_dir, recursive = TRUE, showWarnings = FALSE)
    
    subj_data <- pupil_data %>% filter(Subject == subj)
    
    trial_results <- map(unique(subj_data$Trial), function(tr) {
      message("  Trial: ", tr)
      trial_data <- subj_data %>% filter(Trial == tr)
      process_trial_both(trial_data, subj, tr, subj_dir, output_dir, params,
                         save_intermediate_data, save_individual_plots,
                         return_all_steps, bin_length = bin_length,
                         smooth_then_interp = smooth_then_interp,
                         units = units, rate_smooth_window_ms = rate_smooth_window_ms)
    }) %>% compact()
    
    if (length(trial_results) > 0) {
      combined_final  <- map(trial_results, ~ .x$mean_data)   %>% bind_rows()
      combined_binned <- map(trial_results, ~ .x$binned_data) %>% bind_rows()
      write_csv(combined_final,  file.path(output_dir, paste0("processed_S", subj, ".csv")))
      write_csv(combined_binned, file.path(output_dir, paste0("binned_S",    subj, ".csv")))
      results_list[[paste0("S", subj)]] <- trial_results
    }
  }
  
  if (length(results_list) == 0) {
    message("No results successfully processed.")
    return(list())
  }
  
  all_trials      <- unlist(results_list, recursive = FALSE)
  all_final_data  <- map(all_trials, ~ .x$mean_data)   %>% bind_rows()
  all_binned_data <- map(all_trials, ~ .x$binned_data) %>% bind_rows()
  
  # ---- Optional gaze correction (applied to the aggregated output) ----
  if (apply_gaze_corr) {
    message("Applying gaze-position correction ...")
    all_final_data <- apply_gaze_correction(all_final_data,
                                            units     = units,
                                            per_trial = gaze_per_trial)
    # Note: gaze correction is NOT propagated back into binned_data here.
    # Re-bin from all_final_data if you need corrected binned output.
    message("Gaze correction done. New columns added: *_gc.")
    
    # Save per-trial gaze-corrected plots into the existing subject plot dirs
    save_gaze_corrected_plots(
      all_final_data  = all_final_data,
      plot_dir        = plot_dir,
      units           = units,
      save_individual = save_individual_plots
    )
  }
  
  write_csv(all_final_data,  file.path(output_dir, "all_subjects_processed.csv"))
  write_csv(all_binned_data, file.path(output_dir, "all_subjects_binned.csv"))
  
  if (!return_all_steps) {
    return(list(processed_data = all_final_data, binned_data = all_binned_data))
  } else {
    return(all_trials)
  }
}


# =============================================================================
# NOTE ON GAZE-POSITION CORRECTION (question 3)
# =============================================================================
#
# The problem
# -----------
# When a participant looks away from screen centre, the pupil is viewed at an
# angle by the camera, making it appear elliptical and therefore *smaller* than
# it truly is (the "foreshortening" or "perspective" artifact).  In your
# recognition task, participants look freely at a face image.  If familiar
# faces attract gaze to different regions than novel faces, any systematic gaze
# difference between conditions will be confounded with the pupillary familiarity
# effect.
#
# What apply_gaze_correction() does
# -----------------------------------
# For each trial (or each subject), the function fits:
#
#   pupil(t) = β0 + β1 * x_gaze(t) + β2 * y_gaze(t) + ε(t)
#
# and subtracts the fitted gaze component (β1*x + β2*y), then adds back the
# mean pupil size so the output remains on the original scale.  The residual ε
# is the gaze-corrected pupil trace stored in *_gc columns.
#
# Caveats and recommended workflow
# ---------------------------------
# 1. WHEN to apply: gaze correction should be applied to the *processed* pupil
#    signal (after smoothing + interpolation + baseline correction), not to the
#    raw data.  This is what the pipeline does: apply_gaze_corr = TRUE runs
#    after all per-trial steps are complete.
#
# 2. LINEAR vs NON-LINEAR: the linear regression above removes only the *linear*
#    component of gaze-pupil covariation.  More elaborate models (e.g., the
#    Gagl et al. 2011 polynomial correction, or the model by Hayes & Petrov
#    2016, *Behav Res*) can capture the non-linear foreshortening across the
#    full screen.  If gaze variability in your data is large (participants
#    regularly fixate regions far from centre), consider those approaches.
#    The Gagl correction requires knowing the physical screen geometry and
#    camera distance; contact the eye-tracker manufacturer for these parameters.
#
# 3. PER-TRIAL vs PER-SUBJECT: per-trial regression (gaze_per_trial = TRUE,
#    default) removes within-trial covariation, which is usually what you want.
#    Per-subject regression (gaze_per_trial = FALSE) uses more data points and
#    is more stable but also removes genuine between-trial pupil variation that
#    happens to correlate with gaze shifts across trials.  For your design
#    (familiarity conditions × many trials) per-trial is recommended.
#
# 4. CHECK YOUR ASSUMPTIONS: after correction, plot *_gc against time and
#    verify that the correction did not distort the baseline period.  You can
#    also check whether the β1/β2 coefficients are meaningful by plotting them
#    across participants – if most are near zero, the artifact is minimal and
#    correction may not be worth the statistical cost.
#
# 5. REPORT IT: if you use gaze correction, report the regression approach,
#    whether it was per-trial or per-subject, and the mean ± SD of the
#    regression coefficients across participants.
#
# Alternative: if you have access to the SCREEN COORDINATES of the face
# regions (e.g., you logged where on screen each face was centred), you could
# instead include gaze-to-face-centre distance as a covariate in your LMM
# rather than removing it in preprocessing.  This preserves more information.
# =============================================================================