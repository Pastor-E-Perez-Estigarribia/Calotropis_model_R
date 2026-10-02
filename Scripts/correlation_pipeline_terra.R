pacman::p_load(terra)
pacman::p_load(caret)
pacman::p_load(pheatmap)
pacman::p_load(RColorBrewer)

# envs_t: SpatRaster ya cargado

# 1) Eliminar capas con varianza cero (usar función var, no "var")
vars <- global(envs_t, FUN = var, na.rm = TRUE)
vars_vec <- as.numeric(vars[,1])
zero_var_idx <- which(is.na(vars_vec) | vars_vec == 0)
if (length(zero_var_idx) > 0) {
  message("Eliminando ", length(zero_var_idx), " capas con varianza cero o NA: ",
          paste(names(envs_t)[zero_var_idx], collapse = ", "))
  envs_t <- envs_t[[ -zero_var_idx ]]
} else {
  message("No se encontraron capas con varianza cero.")
}

# 2) Muestreo aleatorio seguro
set.seed(42)
samp_size <- 10000
samp <- terra::spatSample(envs_t, size = samp_size, method = "random", na.rm = TRUE, as.data.frame = TRUE)
if (nrow(samp) < 10) stop("Muestreo insuficiente. Reduce sample_size o revisa capas.")

# 3) Matriz de correlación (pearson)
samp_mat <- as.matrix(samp)
colnames(samp_mat) <- names(envs_t)
m_cor <- cor(samp_mat, use = "pairwise.complete.obs", method = "pearson")

# 4) Heatmap y guardado
pal <- colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100)
png("correlation_heatmap.png", width = 1400, height = 1200, res = 150)
pheatmap::pheatmap(m_cor, color = pal, border_color = NA,
                   clustering_method = "complete",
                   main = paste0("Matriz de correlación (n = ", nrow(samp_mat), " muestras)"),
                   fontsize = 10)
dev.off()

# 5) Eliminar capas altamente correlacionadas
high_idx <- findCorrelation(m_cor, cutoff = 0.9, names = FALSE)
if (length(high_idx) > 0) {
  removed_names <- colnames(m_cor)[high_idx]
  message("Capas eliminadas por correlación > 0.9: ", paste(removed_names, collapse = ", "))
  envs_clean <- envs_t[[ -high_idx ]]
} else {
  message("Ninguna capa supera el cutoff = 0.9")
  envs_clean <- envs_t
}
