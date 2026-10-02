library(colorspace)
library(raster)
library(rasterVis)
library(maptools)
library(viridis)
library(scales)
library(raster)
library(rasterVis)
library(maptools)
library(maps)
library(grid)

#países
data(wrld_simpl)
plot(wrld_simpl)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

?ne_countries

countries <- map("world", plot=FALSE) 
countries <- map2SpatialLines(countries, proj4string = CRS("+proj=longlat"))

View(countries)

# Cargar el raster de idoneidad actual
idoneidad_actual <- raster("C:/futuro/SDM/Resultados/Calotropis_actual.asc")

# Crear la paleta turbo y reducir su saturación
colores_turbo_mate <- desaturate(viridis::turbo(99), amount = 0.1)

# Para America del Sur
plot_m_actual <- rasterVis::levelplot(idoneidad_actual, 
                                      margin = FALSE,
                                      col.regions = colores_turbo_mate, 
                                      ylim = c(-60, 15),  
                                      xlim = c(-85, -30))
print(plot_m_actual)


png("C:/futuro/SDM/Resultados/final/Idoneidad_Actualisimo.png", width = 4000, height = 3000, res = 450)
print(plot_m_actual)
dev.off()
 
gc()
 
 #para cono sur
 plot_m_actual <- rasterVis::levelplot(idoneidad_actual, 
                                       main = "Cono Sur",
                                       margin = FALSE,
                                       col.regions = colores_turbo_mate, 
                                       ylim = c(-37+2, -14+1),  # Ajusta los límites latitudinales para el Cono Sur
                                       xlim = c(-62-5, -47)) +  # Ajusta los límites longitudinales para el Cono Sur
   latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'black'))
 
 print(plot_m_actual)
 png("C:/futuro/SDM/Resultados/final/Idoneidad_Actualisimo_cono.png", width = 2000, height = 1500, res = 450)
 print(plot_m_actual)
 dev.off()

 # Mapa de idoneidad 2050
 idoneidad_2050 <- raster("C:/futuro/SDM/Resultados/Calotropis_2050.asc")
 plot_m_2050 <- rasterVis::levelplot(idoneidad_2050, 
                                     #main = "Idoneidad 2050 (América del Sur)", 
                                     margin = FALSE, 
                                     col.regions = colores_turbo_mate, 
                                     ylim = c(-60, 13),  
                                     xlim = c(-85, -30)) +
 latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'white'))
 print(plot_m_2050)
 png("C:/futuro/SDM/Resultados/final/Idoneidad_20501.png", width = 4000, height = 3000, res = 450)
 print(plot_m_2050)
 dev.off()
 
 plot_m_2050 <- rasterVis::levelplot(idoneidad_2050, 
                                     #main = "Idoneidad 2050 (América del Sur)", 
                                     margin = FALSE, 
                                     col.regions = colores_turbo_mate, 
                                     ylim = c(-60, 13),  
                                     xlim = c(-85, -30))
   latticeExtra::layer(sp.lines(countries, lwd = 0.8, col = 'white'))
 
 print(plot_m_2050)
 
 # Mapa de idoneidad 2070
 idoneidad_2070 <- raster("C:/futuro/SDM/Resultados/Calotropis_2070.asc")
 
 plot_m_2070 <- rasterVis::levelplot(idoneidad_2070, 
                                     margin = FALSE, 
                                     col.regions = colores_viridis, 
                                     ylim = c(-60, 15),  
                                     xlim = c(-85, -30))
 
 print(plot_m_2070)
   
 print(plot_m_2070)
 png("C:/futuro/SDM/Resultados/final/Idoneidad_2070.png", width = 4000, height = 3000, res = 450)
 print(plot_m_2070)
 dev.off()
 
 
 



