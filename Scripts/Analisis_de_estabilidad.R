# Complete R script: per-period 10th-percentile thresholding, 
# per-pixel dynamic regime classification, ----
# faceted classification maps (South America borders), agreement map saved separately,
# mosaic/100% stacked plot without legend, G-test annotation, 
# and final combined figure (classification maps + mosaic/note).
#
# Requirements: terra, dplyr, ggplot2, patchwork, fs, scales, rnaturalearth, sf, tidyr
# Optional: ggmosaic, DescTools
#
# Edit file paths and variable names if needed before running.

library(terra)
library(dplyr)
library(ggplot2)
library(patchwork)
library(fs)
library(scales)
library(rnaturalearth)
library(sf)
library(tidyr)

# Optional packages
has_ggmosaic <- requireNamespace("ggmosaic", quietly = TRUE)
has_desc <- requireNamespace("DescTools", quietly = TRUE)

# -------------------------.--
# User settings (edit if needed) ----
# -------------------------.--
derived_dir  <- file.path("Output", "cmip6_projected", "derived_rasters")
dynamics_dir <- file.path("Output", "cmip6_projected", "ensemble", "dynamics")
dir_create(dynamics_dir, recurse = TRUE)

summary_csv <- file.path(derived_dir, "delta_vel_accel_summary.csv")
if (!file_exists(summary_csv))
  stop("Summary CSV not found: ", summary_csv)

target_model <- "wc2_1_2_5m_bioc_ensamble_p50"
epsilon <- 1e-6            # user requested epsilon
canvas_w_cm <- 36
canvas_h_cm <- 14

# Mapping mid-period -> ensemble prediction file (user-specified)
ensemble_pred_map <- list(
  "2030.5" = file.path(
    "Output",
    "cmip6_projected",
    "collections_by_model",
    target_model,
    "wc2_1_2_5m_bioc_ensamblep50_pred_2021_2040.tif"
  ),
  "2050.5" = file.path(
    "Output",
    "cmip6_projected",
    "collections_by_model",
    target_model,
    "wc2_1_2_5m_bioc_ensamblep50_pred_2041_2060.tif"
  ),
  "2070.5" = file.path(
    "Output",
    "cmip6_projected",
    "collections_by_model",
    target_model,
    "wc2_1_2_5m_bioc_ensamblep50_pred_2061_2080.tif"
  )
)

# -------------------------.--
# Load occurrences (occs) for thresholding ----
# Expect an object 'occs' in the environment or try common CSVs
# occs must contain longitude and latitude columns
# -------------------------.--
occs_df <- NULL
if (exists("occs")) {
  occs_df <- get("occs")
  message("Using 'occs' object from workspace.")
} else {
  # Try common files
  candidates <- c(
    "Data/Points/points_gbif.csv",
    "Data/Points/points_PY.csv",
    "Data/Points/points_BR.csv",
    "Data/Points/2025_PowellSpagarino.csv"
  )
  for (f in candidates) {
    if (file_exists(f)) {
      tmp <- tryCatch(
        read.csv(f, stringsAsFactors = FALSE),
        error = function(e)
          NULL
      )
      if (!is.null(tmp)) {
        names_lower <- tolower(names(tmp))
        if (any(c("longitude", "lon", "long", "x") %in% names_lower) &&
            any(c("latitude", "lat", "y") %in% names_lower)) {
          occs_df <- tmp
          message("Loaded occurrences from: ", f)
          break
        }
      }
    }
  }
}
if (is.null(occs_df))
  stop(
    "Could not find occurrence points (occs). Provide 'occs' object or a CSV with longitude/latitude."
  )

# Normalize column names and extract lon/lat
names_lower <- tolower(names(occs_df))
lon_col <- names(occs_df)[which(names_lower %in% c("longitude", "lon", "long", "x"))[1]]
lat_col <- names(occs_df)[which(names_lower %in% c("latitude", "lat", "y"))[1]]
if (is.na(lon_col) ||
    is.na(lat_col))
  stop("Occurrence table does not contain recognizable longitude/latitude columns.")
occs_coords <- occs_df %>% dplyr::select(all_of(c(lon_col, lat_col))) %>% dplyr::rename(longitude = !!lon_col, latitude = !!lat_col)
occs_coords <- occs_coords %>% filter(!is.na(longitude) &
                                        !is.na(latitude))

# -------------------------.--
# Read summary CSV (deltas and accels) ----
# -------------------------.--
df <- read.csv(summary_csv, stringsAsFactors = FALSE)
df_model <- df %>% filter(model == target_model)
if (nrow(df_model) == 0)
  stop("No entries for model: ", target_model)

df_delta <- df_model %>% filter(type == "delta") %>% arrange(period)
df_accel <- df_model %>% filter(type == "accel") %>% arrange(period)
if (nrow(df_accel) == 0)
  stop("No accel rasters found for model: ", target_model)
if (nrow(df_delta) == 0)
  stop("No delta rasters found for model: ", target_model)

# -------------------------.--
# Helpers to parse period strings ----
# -------------------------.--
mid_from_accel <- function(p) {
  parts <- strsplit(p, "_")[[1]]
  if (length(parts) >= 3)
    return(parts[2])
  NA_character_
}
delta_before_from_accel <- function(p) {
  parts <- strsplit(p, "_")[[1]]
  if (length(parts) >= 3)
    return(paste0(parts[1], "_", parts[2]))
  NA_character_
}
delta_after_from_accel <- function(p) {
  parts <- strsplit(p, "_")[[1]]
  if (length(parts) >= 3)
    return(paste0(parts[2], "_", parts[3]))
  NA_character_
}

# -------------------------.--
# Classification function (Vavg and A) with mask applied ----
# -------------------------.--
classify_VA_masked <- function(V_r, A_r, mask_r, eps) {
  if (!compareGeom(mask_r, V_r, stopOnError = FALSE))
    mask_r <- resample(mask_r, V_r, method = "near")
  if (!compareGeom(A_r, V_r, stopOnError = FALSE))
    A_r <- resample(A_r, V_r, method = "bilinear")
  V_masked <- ifel(mask_r == 1, V_r, NA)
  A_masked <- ifel(mask_r == 1, A_r, NA)
  out <- rast(V_masked)
  values(out) <- NA_real_
  out <- ifel(V_masked > eps & A_masked > eps, 1, out)
  out <- ifel(V_masked > eps & A_masked < -eps, 2, out)
  out <- ifel(V_masked < -eps & A_masked > eps, 3, out)
  out <- ifel(V_masked < -eps & A_masked < -eps, 4, out)
  out <- ifel(abs(V_masked) <= eps & abs(A_masked) <= eps, 5, out)
  return(out)
}

# -------------------------.--
# Main loop: for each accel (mid) period: ----
#  - load period-specific ensemble prediction raster
#  - compute 10th percentile threshold at occs (training presences)
#  - build mask and classify
# -------------------------.--
classified_layers <- list()
mid_labels <- character(0)
thresholds_used <- list()

for (i in seq_len(nrow(df_accel))) {
  accel_period <- df_accel$period[i]
  mid_label    <- mid_from_accel(accel_period)
  if (is.na(mid_label))
    next
  
  # find ensemble pred file for this mid_label
  ens_file <- ensemble_pred_map[[mid_label]]
  if (is.null(ens_file) || !file_exists(ens_file)) {
    message("Ensemble prediction file for mid ",
            mid_label,
            " not found; skipping this instant.")
    next
  }
  
  message("Processing mid period: ", mid_label)
  # load ensemble pred for this period
  ens_r <- rast(ens_file)
  
  # prepare occurrence points as SpatVector in raster CRS
  r_crs <- tryCatch(
    crs(ens_r),
    error = function(e)
      NA
  )
  vec_occs <- vect(occs_coords,
                   geom = c("longitude", "latitude"),
                   crs = "EPSG:4326")
  if (!is.na(r_crs) && r_crs != "") {
    try({
      vec_occs <- project(vec_occs, r_crs)
    }, silent = TRUE)
  }
  
  # extract ensemble predictions at occurrence points
  vals <- terra::extract(ens_r, vec_occs, ID = FALSE)
  vals_vec <- as.numeric(vals[, 1])
  vals_vec <- vals_vec[!is.na(vals_vec)]
  if (length(vals_vec) < 5)
    warning(
      "Fewer than 5 occurrence points had non-NA ensemble predictions for mid ",
      mid_label,
      "; percentile may be unstable."
    )
  
  threshold_10th <- as.numeric(quantile(
    vals_vec,
    probs = 0.10,
    na.rm = TRUE,
    type = 7
  ))
  thresholds_used[[mid_label]] <- threshold_10th
  message("  10th percentile threshold for ",
          mid_label,
          " = ",
          signif(threshold_10th, 6))
  
  # find delta_before, delta_after and accel rasters
  delta_before_p <- delta_before_from_accel(accel_period)
  delta_after_p  <- delta_after_from_accel(accel_period)
  row_before <- df_delta %>% filter(period == delta_before_p)
  row_after  <- df_delta %>% filter(period == delta_after_p)
  if (nrow(row_before) == 0 || nrow(row_after) == 0) {
    message("  Missing delta before/after for mid ",
            mid_label,
            "; skipping.")
    next
  }
  
  r_db <- tryCatch(
    rast(row_before$file[1]),
    error = function(e) {
      message("  read error delta_before: ", e$message)
      NULL
    }
  )
  r_da <- tryCatch(
    rast(row_after$file[1]),
    error = function(e) {
      message("  read error delta_after: ", e$message)
      NULL
    }
  )
  r_a  <- tryCatch(
    rast(df_accel$file[i]),
    error = function(e) {
      message("  read error accel: ", e$message)
      NULL
    }
  )
  if (is.null(r_db) || is.null(r_da) || is.null(r_a))
    next
  
  # align geometries
  if (!compareGeom(r_db, r_da, stopOnError = FALSE))
    r_da <- resample(r_da, r_db, method = "bilinear")
  if (!compareGeom(r_db, r_a, stopOnError = FALSE))
    r_a  <- resample(r_a, r_db, method = "bilinear")
  if (!compareGeom(ens_r, r_db, stopOnError = FALSE))
    ens_r <- resample(ens_r, r_db, method = "bilinear")
  
  # build binary mask using threshold_10th
  mask_r <- ifel(ens_r >= threshold_10th, 1, 0)
  
  # compute average velocity and classify
  Vavg <- (r_db + r_da) / 2
  r_class <- classify_VA_masked(Vavg, r_a, mask_r, epsilon)
  names(r_class) <- paste0("class_", mid_label)
  classified_layers[[mid_label]] <- r_class
  mid_labels <- c(mid_labels, mid_label)
  
  # save per-mid classification raster
  out_rfile <- file.path(dynamics_dir,
                         paste0(target_model, "_class_", mid_label, ".tif"))
  writeRaster(r_class, out_rfile, overwrite = TRUE)
  message("  Saved classification: ", out_rfile)
}

if (length(classified_layers) == 0)
  stop("No classification layers produced.")
stack_cls <- rast(classified_layers)
n_instants <- nlyr(stack_cls)
message("Number of instants (classification maps): ", n_instants)

# -------------------------.--
# Compute contingency table and proportions ----
# -------------------------.--
regime_codes <- 1:5
regime_labels <- c(
  "Runaway expansion",
  "Asymptotic saturation",
  "Reactivation / damping",
  "Continuous retraction",
  "Metastable equilibrium"
)

counts_list <- lapply(seq_len(n_instants), function(li) {
  v <- values(stack_cls[[li]])
  v <- v[!is.na(v)]
  tab <- table(factor(v, levels = regime_codes))
  as.integer(tab)
})
names(counts_list) <- mid_labels
cont_mat <- do.call(cbind, counts_list)
rownames(cont_mat) <- regime_labels
colnames(cont_mat) <- mid_labels
prop_mat <- apply(cont_mat, 2, function(col)
  if (sum(col) == 0)
    rep(0, length(col))
  else
    100 * col / sum(col))

# -------------------------.--
# G-test (likelihood ratio) ----
# -------------------------.--
if (has_desc) {
  Gres <- DescTools::GTest(cont_mat)
  G_stat <- as.numeric(Gres$statistic)
  G_pval <- as.numeric(Gres$p.value)
  G_df <- as.numeric(Gres$parameter)
} else {
  obs <- cont_mat
  total <- sum(obs)
  row_sums <- rowSums(obs)
  col_sums <- colSums(obs)
  expected <- outer(row_sums, col_sums) / total
  idx <- which(obs > 0 & expected > 0, arr.ind = TRUE)
  G_stat <- 2 * sum(obs[idx] * log(obs[idx] / expected[idx]))
  G_df <- (nrow(obs) - 1) * (ncol(obs) - 1)
  G_pval <- 1 - pchisq(G_stat, df = G_df)
}

# -------------------------.--
# Prepare plotting data ----
# -------------------------.--
plot_df <- as.data.frame(prop_mat)
plot_df$Regime <- regime_labels
plot_long <- tidyr::pivot_longer(
  plot_df,
  cols = -Regime,
  names_to = "Instant",
  values_to = "Pct"
)
plot_long$Regime <- factor(plot_long$Regime, levels = regime_labels)
plot_long$Instant <- factor(plot_long$Instant, levels = mid_labels)

# Mosaic or fallback 100% stacked bar (legend removed) 
if (has_ggmosaic) {
  library(ggmosaic)
  long_counts <- as.data.frame(as.table(cont_mat))
  colnames(long_counts) <- c("Regime", "Instant", "Count")
  long_counts$Regime <- factor(long_counts$Regime, levels = regime_labels)
  long_counts$Instant <- factor(long_counts$Instant, levels = mid_labels)
  
  p_mosaic <- ggplot(data = long_counts) +
    ggmosaic::geom_mosaic(
      aes(
        weight = Count,
        x = product(Instant),
        fill = Regime
      ),
      na.rm = TRUE,
      color = "white"
    ) +
    scale_fill_manual(values = c("#d73027", "#fc8d59", "#fee090", "#91bfdb", "#4575b4")) +
    labs(
      title = "Regime distribution across instants",
      x = "Instant (mid period)",
      y = NULL,
      fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      plot.title = element_text(face = "bold")
    )
} else {
  p_mosaic <- ggplot(plot_long, aes(x = Instant, y = Pct, fill = Regime)) +
    geom_col(position = "fill",
             color = "white",
             width = 0.3) +
    scale_y_continuous(labels = percent_format(scale = 1)) +
    scale_fill_manual(values = c("#d73027", "#fc8d59", "#fee090", "#91bfdb", "#4575b4")) +
    labs(
      title = "Regime proportions (100% stacked)",
      y = "Proportion (100%)",
      x = "Instant (mid period)",
      fill = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold"))
}

# G-test note
g_note_text <- paste0(
  "G-test: G = ",
  signif(G_stat, 4),
  "  |  df = ",
  G_df,
  "  |  p = ",
  signif(G_pval, 8),
  ifelse(G_pval < 0.05, "  (significant)", "  (not significant)")
)
p_note <- ggplot() + annotate(
  "text",
  x = 0,
  y = 0.5,
  label = g_note_text,
  hjust = 0,
  size = 4
) + xlim(0, 1) + ylim(0, 1) + theme_void() + theme(plot.margin = margin(6, 6, 6, 6))

# -------------------------.--
# Prepare maps: classification faceted and agreement pct ----
# Use coord_sf() and project South America borders to raster CRS if possible
# -------------------------.--
map_dfs <- lapply(seq_len(n_instants), function(i) {
  dfm <- as.data.frame(stack_cls[[i]], xy = TRUE, na.rm = FALSE)
  colnames(dfm)[3] <- "RegimeCode"
  dfm$Instant <- mid_labels[i]
  dfm
})
map_df_all <- do.call(rbind, map_dfs)
map_df_all$RegimeLabel <- factor(
  as.character(map_df_all$RegimeCode),
  levels = as.character(regime_codes),
  labels = regime_labels
)

# Load only South America geometries and attempt to transform to raster CRS
world_all <- ne_countries(scale = "medium", returnclass = "sf")
# subset by ISO codes for South America
sa_iso2 <- c("AR",
             "BO",
             "BR",
             "CL",
             "CO",
             "EC",
             "GF",
             "GY",
             "PE",
             "PY",
             "SR",
             "UY",
             "VE")
world_sa <- world_all %>% filter(iso_a2 %in% sa_iso2)
if (nrow(world_sa) == 0) {
  warning("No South America geometries found; using full world as fallback.")
  world_sa <- world_all
}
r_crs <- tryCatch(
  crs(stack_cls[[1]]),
  error = function(e)
    NA
)
world_sa_proj <- world_sa
if (!is.na(r_crs) && r_crs != "") {
  try({
    world_sa_proj <- st_transform(world_sa, crs = r_crs)
  }, silent = TRUE)
}

# Faceted classification map with South America borders and transparent NA
p_class_map <- ggplot() +
  geom_raster(data = map_df_all,
              aes(x = x, y = y, fill = RegimeLabel),
              na.rm = FALSE) +
  scale_fill_manual(
    values = c("#d73027", "#fc8d59", "#fee090", "#91bfdb", "#4575b4"),
    na.value = "transparent",
    drop = FALSE
  ) +
  geom_sf(
    data = world_sa_proj,
    fill = NA,
    color = "grey40",
    size = 0.3,
    inherit.aes = FALSE
  ) +
  facet_wrap(~ Instant, nrow = 1) +
  labs(
    title = paste("Per-pixel classification by instant"),
    fill = NULL
  ) +
  coord_sf() +
  theme_minimal(base_size = 10) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# Agreement percentage map with South America borders and transparent NA
consensus_map <- app(
  stack_cls,
  fun = function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0)
      return(NA_real_)
    tb <- table(x)
    maxf <- max(tb)
    if (sum(tb == maxf) > 1)
      return(NA_real_)
    else
      as.numeric(names(tb)[which.max(tb)])
  }
)
agreement_count <- sum(stack_cls == consensus_map, na.rm = TRUE)
agreement_count <- ifel(is.na(consensus_map), NA, agreement_count)
agreement_pct <- (agreement_count / n_instants) * 100
df_agree <- as.data.frame(agreement_pct, xy = TRUE, na.rm = FALSE)
colnames(df_agree)[3] <- "AgreementPct"

p_agree_pct <- ggplot() +
  geom_raster(data = df_agree,
              aes(x = x, y = y, fill = AgreementPct),
              na.rm = FALSE) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    na.value = "transparent",
    labels = function(x)
      paste0(round(x), "%")
  ) +
  geom_sf(
    data = world_sa_proj,
    fill = NA,
    color = "grey40",
    size = 0.3,
    inherit.aes = FALSE
  ) +
  labs(
    title = paste0(
      "Agreement percentage across instants (",
      n_instants,
      " instants)"
    ),
    fill = "Consensus %"
  ) +
  coord_sf() +
  theme_minimal(base_size = 11) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# -------------------------.--
# Save agreement percentage map separately ----
# -------------------------.--
agree_out_png <- file.path(dynamics_dir, paste0(target_model, "_agreement_pct_map.png"))
agree_out_pdf <- file.path(dynamics_dir, paste0(target_model, "_agreement_pct_map.pdf"))
ggsave(
  filename = agree_out_png,
  plot = p_agree_pct,
  width = canvas_w_cm / 2.54,
  height = canvas_h_cm / 2.54,
  dpi = 300,
  bg = "transparent"
)
ggsave(
  filename = agree_out_pdf,
  plot = p_agree_pct,
  width = canvas_w_cm / 2.54,
  height = canvas_h_cm / 2.54,
  device = cairo_pdf
)
message("Saved agreement percentage map separately: ",
        agree_out_png,
        " and ",
        agree_out_pdf)

# -------------------------.--
# Combine only classification maps + right column (mosaic + note) ----
# -------------------------.--
# -------------------------.--
# Combine panels: maps on first row; mosaic/stack + note on second row
# Save agreement map separately and then combined figure with two rows
# -------------------------.--

# Save agreement percentage map separately (transparent background)
agree_out_png <- file.path(dynamics_dir, paste0(target_model, "_agreement_pct_map.png"))
agree_out_pdf <- file.path(dynamics_dir, paste0(target_model, "_agreement_pct_map.pdf"))
ggsave(
  filename = agree_out_png,
  plot = p_agree_pct,
  width = canvas_w_cm / 2.54,
  height = canvas_h_cm / 2.54,
  dpi = 300,
  bg = "transparent"
)
ggsave(
  filename = agree_out_pdf,
  plot = p_agree_pct,
  width = canvas_w_cm / 2.54,
  height = canvas_h_cm / 2.54,
  device = cairo_pdf
)
message("Saved agreement percentage map separately: ",
        agree_out_png,
        " and ",
        agree_out_pdf)

# Build right column (mosaic/stack + note) as a single vertical layout
right_col <- p_mosaic / p_note + plot_layout(heights = c(3, 0.4))

# Compose final layout: first row = p_class_map (maps) spanning full width left,
# second row = right_col (mosaic + note) spanning full width below maps.
# We'll arrange as a 2-row layout: row1 = maps (full width), row2 = mosaic+note (full width).
final_layout <- (p_class_map) / (right_col) + plot_layout(heights = c(2, 0.9))

# Recommended canvas size: width = 36 cm; height = 20 cm (maps above, bars below).
# If you prefer the previous height, set canvas_h_cm <- 14 (but bars may be cramped).
canvas_w_cm_final <- 36
canvas_h_cm_final <- 20

# Save final combined figure (PNG and PDF)
out_combined_png <- file.path(dynamics_dir,
                              paste0(target_model, "_classmaps_above_mosaic.png"))
out_combined_pdf <- file.path(dynamics_dir,
                              paste0(target_model, "_classmaps_above_mosaic.pdf"))

ggsave(
  filename = out_combined_png,
  plot = final_layout,
  width = canvas_w_cm_final / 2.54,
  height = canvas_h_cm_final / 2.54,
  dpi = 300,
  bg = "white"
)
ggsave(
  filename = out_combined_pdf,
  plot = final_layout,
  width = canvas_w_cm_final / 2.54,
  height = canvas_h_cm_final / 2.54,
  device = cairo_pdf
)

message(
  "Saved combined figure (maps above, mosaic below): ",
  out_combined_png,
  " and ",
  out_combined_pdf
)
message(
  "Recommended canvas: width = ",
  canvas_w_cm_final,
  " cm; height = ",
  canvas_h_cm_final,
  " cm."
)


# -------------------------.--
# Save contingency table and proportions ----
# -------------------------.--
write.csv(cont_mat, file.path(
  dynamics_dir,
  paste0(target_model, "_regime_contingency_masked.csv")
), row.names = TRUE)
write.csv(round(prop_mat, 2),
          file.path(
            dynamics_dir,
            paste0(target_model, "_regime_proportions_pct_masked.csv")
          ),
          row.names = TRUE)

# Save G-test summary and thresholds used
out_stats <- file.path(dynamics_dir,
                       paste0(target_model, "_regime_Gtest_summary_masked.txt"))
cat(
  "G-test summary\nModel:",
  target_model,
  "\nInstants:",
  paste(mid_labels, collapse = ", "),
  "\n\n",
  "G statistic:",
  signif(G_stat, 6),
  "\nDF:",
  G_df,
  "\nP-value:",
  signif(G_pval, 6),
  "\n\n",
  "Thresholds (10th percentile at training presences) per instant:\n",
  file = out_stats
)
for (m in names(thresholds_used)) {
  cat(m,
      ":",
      signif(thresholds_used[[m]], 6),
      "\n",
      file = out_stats,
      append = TRUE)
}
utils::write.table(
  cont_mat,
  file = out_stats,
  append = TRUE,
  sep = ",",
  col.names = NA
)

message("Saved outputs in: ", dynamics_dir)
message("Thresholds used (10th percentile of training presences) per instant:")
print(thresholds_used)
