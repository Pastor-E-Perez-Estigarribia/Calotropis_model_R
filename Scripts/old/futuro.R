
#Forecast climate data ####
# Download predicted climate data
forecast_data <- cmip6_world(model = "MPI-ESM1-2-HR",
                             ssp = "245",
                             time = "2061-2080",
                             var = "bioc",
                             res = 2.5,
                             path = "Data")

forecast_data <- cmip6_world(model = "MPI-ESM1-2-HR",
                             ssp = "245",
                             time = "2061-2080",
                             var = "bioc",
                             res = 10,
                             path = "Data")

# Use names from bioclim_data ####
names(forecast_data) <- names(bioclim_data)

# Crop forecast data to desired extent ####
forecast_data <- crop(x = forecast_data, y = sample_extent)

# Predict presence from model with forecast data ####
forecast_presence <- predict(forecast_data, glmnet_model, type = "response")

# Plot base map
plot(my_map,
     axes = TRUE,
     col = "grey95")
# Add model probabilities ####
plot(forecast_presence, add = TRUE)

#__________________________________________________________________

# Cargar las capas de idoneidad actual
idoneidad_actual <- raster("C:/Calotropis_model/Output/Calotropis.asc")

# Cargar las capas de proyección climática para 2050 y 2070
envs_2050 <- stack(list.files("C:/Calotropis_model/Data/Future_Layers/50asc10mAmerica", pattern = 'asc$', full.names = TRUE))
envs_2070 <- stack(list.files("C:/Calotropis_model/Data/Future_Layers/70asc10mAmerica", pattern = 'asc$', full.names = TRUE))

# Seleccionar el mejor modelo basado en AICc
bestmod <- which(e.mx@results$AICc == min(e.mx@results$AICc, na.rm = TRUE))
mejor_modelo <- e.mx@models[[bestmod]]

# Predecir la idoneidad para el presente
pr_actual <- predict(envs, mejor_modelo, type = 'cloglog')
writeRaster(pr_actual, "C:/Calotropis_model/Output/Calotropis_actual.asc", format="ascii", overwrite = TRUE)

# Predecir la idoneidad para 2050
pr_2050 <- predict(envs_2050, mejor_modelo, type = 'cloglog')
writeRaster(pr_2050, "C:/Calotropis_model/Output/Calotropis_2050.asc", format="ascii", overwrite = TRUE)

# Predecir la idoneidad para 2070
pr_2070 <- predict(envs_2070, mejor_modelo, type = 'cloglog')
writeRaster(pr_2070, "C:/Calotropis_model/Output/Calotropis_2070.asc", format="ascii", overwrite = TRUE)

# Reescalar la capa de idoneidad actual para que coincida con la resolución de la capa de 2050
pr_actual_resampled <- resample(pr_actual, pr_2050, method = "bilinear")

# Calcular la diferencia entre 2050 y la idoneidad actual
dif_2050 <- (pr_2050 - pr_actual_resampled)/(2000-2050)

writeRaster(dif_2050, "C:/Calotropis_model/Output/Diferencia_2050.asc", format="ascii", overwrite = TRUE)

# Reescalar la capa de idoneidad actual para que coincida con la resolución de la capa de 2070
pr_actual_resampled_2070 <- resample(pr_actual, pr_2070, method = "bilinear")

# Calcular la diferencia entre 2070 y la idoneidad actual
dif_2070 <- (pr_2070 - pr_actual_resampled_2070)/(2050-2070)

writeRaster(dif_2070, "C:/Calotropis_model/Output/Diferencia_2070.asc", format="ascii", overwrite = TRUE)

library(maptools)
countries <- maptools::wrld_simpl
countries <- spTransform(countries, CRS(projection(pr_actual)))

# Mapa enfocado para dif_2050
plot_m_2050 <- rasterVis::levelplot(dif_2050, 
                                    main = "Predicción de Idoneidad 2050 vs Actual", 
                                    margin = FALSE,
                                    col.regions = viridis::viridis(99),
                                    par.settings = rasterTheme(region = rev(brewer.pal(10, 'Spectral'))),
                                    ylim = c(-37+2, -14+1), 
                                    xlim = c(-62-5, -47)) + 
  latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'black'))

# Mostrar y guardar el gráfico
print(plot_m_2050)
png('C:/Calotropis_model/Output/Prediccion_2050_Area_Especifica.png', width=7, height=9, units = 'in', res = 300)
print(plot_m_2050)
dev.off()
library(rasterVis)
library(RColorBrewer)
library(sp)
library(maptools)

# Cargar los datos de los países y transformar la proyección
countries <- maptools::wrld_simpl
countries <- spTransform(countries, CRS(projection(pr_actual)))

# Mapa enfocado para dif_2050
plot_m_2050 <- rasterVis::levelplot(dif_2050, 
                                    main = "Predicción de Idoneidad 2050 vs Actual", 
                                    margin = FALSE,
                                    col.regions = viridis::viridis(99),
                                    par.settings = rasterTheme(region = rev(brewer.pal(11, 'RdBu'))),
                                    ylim = c(-37 + 2, -14 + 1), 
                                    xlim = c(-62 - 5, -47)) + 
  latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'black'))

# Mostrar y guardar el gráfico
print(plot_m_2050)

plot_m_2050 <- rasterVis::levelplot(dif_2050, 
                                    main = "Predicción de Idoneidad 2050 vs Actual", 
                                    margin = FALSE,
                                    col.regions = rev(brewer.pal(11, 'RdBu')),  # Aplica directamente la paleta RdBu
                                    ylim = c(-37 + 2, -14 + 1), 
                                    xlim = c(-62 - 5, -47)) + 
  latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'black'))

# Mostrar y guardar el gráfico
print(plot_m_2050)

plot_m_2050 <- rasterVis::levelplot(dif_2050, 
                                    main = "Predicción de Idoneidad 2050 vs Actual", 
                                    margin = FALSE,
                                    col.regions = viridis::turbo(99),
                                    ylim = c(-37 + 2, -14 + 1), 
                                    xlim = c(-62 - 5, -47)) + 
  latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'black'))

# Mapa enfocado para dif_2050
plot_m_2050 <- rasterVis::levelplot(dif_2050, 
                                    main = "Predicción de Idoneidad 2050 vs Actual", 
                                    margin = FALSE,
                                    col.regions = viridis::magma(99),
                                    ylim = c(-37 + 2, -14 + 1), 
                                    xlim = c(-62 - 5, -47)) + 
  latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'white'))

print(plot_m_2050)
png('C:/Calotropis_model/Outputs/Prediccion_2070_Area_magi2.png', width=7, height=9, units = 'in', res = 300)
print(plot_m_2070)

# Mapa enfocado para dif_2070
plot_m_2070 <- rasterVis::levelplot(dif_2070, 
                                    main = "Predicción de Idoneidad 2070 vs Actual", 
                                    margin = FALSE,
                                    col.regions = viridis::magma(99),  # Asegúrate de usar la misma paleta sin invertir
                                    ylim = c(-37 + 2, -14 + 1), 
                                    xlim = c(-62 - 5, -47)) + 
  latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'white'))

# Mostrar los gráficos
print(plot_m_2050)
print(plot_m_2070)
png('C:/Calotropis_model/Output/Prediccion_2070_Area_magi2.png', width=7, height=9, units = 'in', res = 300)
print(plot_m_2070)



# Mostrar y guardar el gráfico
print(plot_m_2050)

plot_m_2070 <- rasterVis::levelplot(dif_2070, 
                                    main = "Predicción de Idoneidad 2070 vs Actual", 
                                    margin = FALSE,
                                    col.regions = rev(viridis::magma(99)),
                                    ylim = c(-37 + 2, -14 + 1), 
                                    xlim = c(-62 - 5, -47)) + 
  latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'white'))

# Mostrar y guardar el gráfico
print(plot_m_2070)
png('C:/Calotropis_model/Output/Prediccion_2070_Area_mag.png', width=7, height=9, units = 'in', res = 300)
print(plot_m_2070)
dev.off()






# Mapa enfocado para dif_2070
plot_m_2070 <- rasterVis::levelplot(dif_2070, 
                                    main = "Predicción de Idoneidad 2070 vs Actual", 
                                    margin = FALSE,
                                    col.regions = viridis::magma(99),
                                    par.settings = rasterTheme(region = rev(brewer.pal(10, 'Spectral'))),
                                    ylim = c(-37+2, -14+1), 
                                    xlim = c(-62-5, -47)) + 
  latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'white'))

# Mostrar y guardar el gráfico
print(plot_m_2070)
png('C:/Calotropis_model/Output/Prediccion_2070_ame_magma.png', width=7, height=9, units = 'in', res = 300)
print(plot_m_2070)
dev.off()
# Asegúrate de que no se está agregando ninguna capa de líneas de países
plot_m_2050_full <- rasterVis::levelplot(dif_2050, 
                                         main = "Predicción de Idoneidad 2050 vs Actual", 
                                         margin = FALSE,
                                         col.regions = viridis::magma(99),
                                         ylim = c(-60, 15),  # Ajusta los límites latitudinales
                                         xlim = c(-85, -30))

# Mostrar y guardar el gráfico
print(plot_m_2050_full)


# Ajustar el mapa completo para dif_2050 con enfoque en América del Sur
plot_m_2050_full <- rasterVis::levelplot(dif_2050, 
                                         main = "Predicción de Idoneidad 2050 vs Actual", 
                                         margin = FALSE,
                                         col.regions = viridis::magma(99),
                                         par.settings = rasterTheme(region = rev(brewer.pal(10, 'Spectral'))),
                                         ylim = c(-60, 15),  # Ajusta los límites latitudinales
                                         xlim = c(-85, -30)) +  # Ajusta los límites longitudinales
  #latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'white'))

# Mostrar y guardar el gráfico
print(plot_m_2050_full)
png('C:/futuro/SDM/Resultados/Prediccion_2050_America_Sur_mag.png', width=7, height=9, units = 'in', res = 300)
print(plot_m_2050_full)
dev.off()

library(raster)
library(sp)
library(rasterVis)
library(RColorBrewer)
library(maptools)

# Cargar datos y asignar proyección
dif_2050 <- projectRaster(dif_2050, crs = CRS("+proj=laea +lat_0=-10 +lon_0=-60"))
countries <- spTransform(countries, CRS("+proj=laea +lat_0=-10 +lon_0=-60"))

# Mapa de predicción 2050 con una proyección adecuada para América del Sur
plot_m_2050_full <- rasterVis::levelplot(dif_2050, 
                                         main = "Predicción de Idoneidad 2050 vs Actual", 
                                         margin = FALSE,
                                         col.regions = rev(brewer.pal(10, 'Spectral')),
                                         ylim = c(-60, 15),  # Ajusta los límites latitudinales
                                         xlim = c(-85, -30)) +  # Ajusta los límites longitudinales
  latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'white'))

# Mostrar el gráfico
print(plot_m_2050_full)



# Ajustar el mapa completo para dif_2070 con enfoque en América del Sur
plot_m_2070_full <- rasterVis::levelplot(dif_2070, 
                                         main = "Predicción de Idoneidad 2070 vs Actual", 
                                         margin = FALSE,
                                         col.regions = viridis::viridis(99),
                                         par.settings = rasterTheme(region = rev(brewer.pal(10, 'Spectral'))),
                                         ylim = c(-60, 15),  # Ajusta los límites latitudinales
                                         xlim = c(-85, -30)) +  # Ajusta los límites longitudinales
  latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'white'))

# Mostrar y guardar el gráfico
print(plot_m_2070_full)
png('C:/futuro/SDM/Resultados/Prediccion_2070_America_Sur.png', width=7, height=9, units = 'in', res = 300)
print(plot_m_2070_full)
dev.off()




# Extraer los valores de las diferencias
valores_dif_2050 <- getValues(dif_2050)
valores_dif_2070 <- getValues(dif_2070)
# Calcular la capacidad de expansión (número de celdas con valores positivos)
expansion_2050 <- sum(valores_dif_2050 > 0, na.rm = TRUE)
expansion_2070 <- sum(valores_dif_2070 > 0, na.rm = TRUE)

# Calcular el número total de celdas
total_celdas_2050 <- length(valores_dif_2050)
total_celdas_2070 <- length(valores_dif_2070)

# Calcular el porcentaje de celdas con expansión
porcentaje_expansion_2050 <- (expansion_2050 / total_celdas_2050) * 100
porcentaje_expansion_2070 <- (expansion_2070 / total_celdas_2070) * 100

# Imprimir resultados
print(paste("Número de celdas con expansión en 2050:", expansion_2050))
print(paste("Porcentaje de celdas con expansión en 2050:", porcentaje_expansion_2050, "%"))

print(paste("Número de celdas con expansión en 2070:", expansion_2070))
print(paste("Porcentaje de celdas con expansión en 2070:", porcentaje_expansion_2070, "%"))
2

# Imprimir la tasa de cambio porcentual
print(paste("Tasa de cambio porcentual entre 2050 y 2070:", tasa_cambio, "%"))

#cambio_absoluto <- expansion_2070 - expansion_2050
#print(paste("Cambio absoluto en la expansión entre 2050 y 2070:", cambio_absoluto))

# <- sum(valores_dif_2050 > 0 & valores_dif_2070 > 0, na.rm = TRUE)
#porcentaje_retencion <- (retencion / expansion_2050) * 100
#print(paste("Porcentaje de retención de celdas idóneas de 2050 a 2070:", porcentaje_retencion, "%"))

#perdida <- sum(valores_dif_2050 > 0 & valores_dif_2070 <= 0, na.rm = TRUE)
#porcentaje_perdida <- (perdida / expansion_2050) * 100
#print(paste("Porcentaje de celdas que pierden idoneidad de 2050 a 2070:", porcentaje_perdida, "%"))

#ganancia <- sum(valores_dif_2050 <= 0 & valores_dif_2070 > 0, na.rm = TRUE)
#porcentaje_ganancia <- (ganancia / total_celdas_2070) * 100
#print(paste("Porcentaje de celdas nuevas idóneas en 2070:", porcentaje_ganancia, "%"))



# Mapa de expansión (celdas con valores positivos)
expansion_map <- calc(stack_difs, fun = function(x) {
  ifelse(x > 0, 1, NA)
})

# Mapa de contracción (celdas con valores negativos)
contraction_map <- calc(stack_difs, fun = function(x) {
  ifelse(x < 0, 1, NA)
})

# Graficar mapas
plot(expansion_map, main = "Mapa de Expansión 2070 vs 2050")
plot(contraction_map, main = "Mapa de Contracción 2070 vs 2050")
# Crear dataframe con los resultados obtenidos
df_cambio <- data.frame(
  Año = c("2050", "2070"),
  Expansión = c(porcentaje_expansion_2050, porcentaje_expansion_2070),
  Retención = c(porcentaje_retencion, NA),
  Pérdida = c(NA, porcentaje_perdida)
)

# Gráfico de barras
library(ggplot2)
ggplot(df_cambio, aes(x = Año)) +
  geom_bar(aes(y = Expansión, fill = "Expansión"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Retención, fill = "Retención"), stat = "identity", position = "dodge") +
  geom_bar(aes(y = Pérdida, fill = "Pérdida"), stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Expansión" = "blue", "Retención" = "green", "Pérdida" = "red")) +
  labs(title = "Cambios en la Idoneidad 2050 vs 2070", y = "Porcentaje", fill = "Categoría") +
  theme_minimal()

# Identificar las celdas que son idóneas en 2050 pero no en 2070 (pérdida)
vulnerabilidad_map <- calc(stack_difs, fun = function(x) {
  ifelse(x[1] > 0 & x[2] <= 0, 1, NA)
})

# Graficar mapa de vulnerabilidad
plot(vulnerabilidad_map, main = "Mapa de Vulnerabilidad 2050-2070")

# Mapa de vulnerabilidad para América del Sur
vulnerabilidad_map <- calc(stack_difs, fun = function(x) {
  ifelse(x[1] > 0 & x[2] <= 0, 1, NA)
})

# Graficar el mapa con enfoque completo en América del Sur
plot_vulnerabilidad <- rasterVis::levelplot(vulnerabilidad_map, 
                                            main = "Mapa de Vulnerabilidad 2050-2070 (América del Sur)", 
                                            margin = FALSE,
                                            col.regions = viridis::viridis(99),
                                            par.settings = rasterTheme(region = rev(brewer.pal(10, 'Spectral'))),
                                            ylim = c(-60, 15),  # Ajusta los límites latitudinales
                                            xlim = c(-85, -30)) +  # Ajusta los límites longitudinales
  latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'white'))

# Mostrar y guardar el gráfico
print(plot_vulnerabilidad)
png('C:/futuro/SDM/Resultados/Mapa_Vulnerabilidad_America_Sur.png', width=7, height=9, units = 'in', res = 300)
print(plot_vulnerabilidad)
dev.off()

# Ajustar los límites y la extensión del mapa para cubrir todo América del Sur

# Mapa de expansión geográfica para América del Sur
expansion_map <- rasterVis::levelplot(expansion_2050, 
                                      main = "Expansión Geográfica 2050 (América del Sur)", 
                                      margin = FALSE,
                                      col.regions = viridis::viridis(99),
                                      par.settings = rasterTheme(region = rev(brewer.pal(10, 'Spectral'))),
                                      ylim = c(-60, 15),  # Ajusta los límites latitudinales
                                      xlim = c(-85, -30)) +  # Ajusta los límites longitudinales
  latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'white'))

# Mostrar y guardar el gráfico de expansión
print(expansion_map)
png('C:/futuro/SDM/Resultados/Mapa_Expansion_2050_America_Sur.png', width=7, height=9, units = 'in', res = 300)
print(expansion_map)
dev.off()

# Reemplazar con las capas de diferencia
expansion_2050 <- dif_2050  # Diferencia entre la proyección de 2050 y la actual
contraccion_2070 <- dif_2070  # Diferencia entre la proyección de 2070 y la actual

# Mapa de la diferencia para América del Sur (2050)
expansion_map <- rasterVis::levelplot(expansion_2050, 
                                      main = "Expansión/Contracción Geográfica 2050 (América del Sur)", 
                                      margin = FALSE,
                                      col.regions = viridis::viridis(99),
                                      par.settings = rasterTheme(region = rev(brewer.pal(10, 'Spectral'))),
                                      ylim = c(-60, 15),  # Ajusta los límites latitudinales
                                      xlim = c(-85, -30)) +  # Ajusta los límites longitudinales
  latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'white'))

# Mostrar y guardar el gráfico
print(expansion_map)
png('C:/futuro/SDM/Resultados/Mapa_Expansion_Contraccion_2050_America_Sur.png', width=7, height=9, units = 'in', res = 300)
print(expansion_map)
dev.off()

library(raster)
library(pacman)
#Calculos
idoneidad_actual <- raster("C:/futuro/SDM/Resultados/Calotropis_actual.asc")
idoneidad_2050 <- raster("C:/futuro/SDM/Resultados/Calotropis_2050.asc") 
idoneidad_2070 <- raster("C:/futuro/SDM/Resultados/Calotropis_2070.asc") 

contraccio_2050 = ifelse(idoneidad_2070 - idoneidad_2050 < 0, -1, 0) 

?ifelse

aux_50 <- overlay(idoneidad_2050,fun = function(x) {
  ifelse(x > 0.05, 1, 0)
})

plot(aux_50)

aux_70 <- overlay(idoneidad_2070,fun = function(x) {
  ifelse(x < 0.05, -1, 0)
})

plot(aux_70)

cont = aux_50*aux_70

plot(cont)


# Suponiendo que idoneidad_2050 e idoneidad_2070 son objetos RasterLayer
contraccion_2070 <- overlay(idoneidad_2070, idoneidad_2050,aux, 
                            fun = function(x, y,z) {
  ifelse((x - y)*z <= -0.05, -1, 0)
})

plot(contraccion_2070)

plot(idoneidad_2050)

occ = 0.5

aux <- overlay(idoneidad_2050,fun = function(x, y) {
  ifelse(x > 0.05, 0, 1)
})

plot(aux)


expan_2070 <- overlay(idoneidad_2070, idoneidad_2050,aux, fun = function(x,y,z) {
  ifelse((x - y)*z >= 0.05, 1, 0)
})


plot(expan_2070)

print(expan_2070)
png('C:/futuro/SDM/Resultados/Mapa_Expansion.png', width=7, height=9, units = 'in', res = 400)
print(expan_2070)

exp_contra = cont + expan_2070

plot(exp_contra)
plot(idoneidad_2070)
