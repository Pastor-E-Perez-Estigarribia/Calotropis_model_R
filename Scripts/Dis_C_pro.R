

# Flujo de trabajo ####

# Ver lecturas de ejemplos 


browseURL('https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html')
browseURL('http://cran.nexr.com/web/packages/ENMeval/vignettes/ENMeval-vignette.html')
browseURL('https://support.bccvl.org.au/support/solutions/articles/6000083216-maxent')


# Instalar y cargar paquetes ####

if (!require('pacman'))
  install.packages('pacman',
  repos = "http://cran.us.r-project.org"
  )

if (!require('geodata'))
remotes::install_github("rspatial/geodata"
  )

#if (!require('rgeos'))
#remotes::install_version("rgeos", version = "0.6-4"
#  )


pacman::p_load(spocc,
               ENMeval,
               tidyverse,
               rio,
               dismo,
               raster,
               #maptools,
               rgeos,
               dplyr,
               spThin,
               rasterVis,
               lattice,
               geodata,
               sf,
               rnaturalearth,
               rnaturalearthdata,
               terra,
               gstat,
               ncf,
               hexbin,
               MASS)




memory.size()      # Memoria en uso
memory.limit()     # Límite actual

# Instalar maptools desde el archivo del CRAN Archive

#install.packages("https://cran.r-project.org/src/contrib/Archive/maptools/maptools_1.1-8.tar.gz", repos = NULL, type = "source")


# Data Acquisition & Pre-processing ##

sp = 'Calotropis procera'

spK = "C_pro"

# Search GBIF for occurrence data.
#spp <- occ(sp, 'gbif', limit=20000, has_coords=TRUE) 

# Get the latitude/coordinates for each locality. Also convert the tibble that occ() outputs
# to a data frame for compatibility with ENMeval functions.

#occs0 <- as.data.frame(spp$gbif$data$Calotropis_procera[,2:3]) %>%
#  dplyr::filter(
#    latitude >= -56 & latitude <= 13,
#    longitude >= -82 & longitude <= -34
#  )

#dim(occs0)

#rio::export(occs0, 'Data/Points/points_gbif.csv') 

occs1 = rio::import('Data/Points/points_gbif.csv') |> unique() # concatener df y eliminar 
# duplicados exactos en todas las columnas (mantiene la primera ocurrencia)

occs2 = rio::import("Data/Points/points_PY.csv") %>% 
  subset(sp == spK) %>% 
  dplyr::rename(longitude = LONG, latitude = LAT) %>% 
  dplyr::select(-sp) 

occs3 = rio::import("Data/Points/points_BR.csv") %>% 
  subset(sp == spK) %>% 
  dplyr::rename(longitude = LONG, latitude = LAT) %>% 
  dplyr::select(-sp) 

occs4 = rio::import("Data/Points/2025_PowellSpagarino.csv") %>% 
  dplyr::rename(longitude = Longitud, latitude = Latitud)  %>% 
  dplyr::select(longitude,latitude)



occs = rbind(occs1,occs2,occs3,occs4) |> unique() # concatener df y eliminar 
# duplicados exactos en todas las columnas (mantiene la primera ocurrencia)

dim(occs)


# climate model data ####

# https://rdrr.io/github/rspatial/geodata/man/cmip6.html

# https://forum.posit.co/t/downloading-worldclim-data-using-the-geodata-package-in-r/166363

# https://rdrr.io/cran/raster/man/getData.html

#  getData('worldclim', var='tmin', res=0.5, lon=5, lat=45)

# NOTE: this will download, decompress, and make a raster stack of 2.5' resolution climate data 
# for specific arguments see ?getData
# recommended: create a specific directory where data will be downloaded & add it into path
#bioclim.data <- getData(name = "worldclim", var = "bio", res = 2.5, path = "Data/")

#?getData

# examine the downloaded raster files
#plot(bioclim.data$bio1)

#  2.5′ (≈5 km) suele ser el compromiso estándar para modelos de distribución 
# de especies y análisis regionales porque mantiene heterogeneidad relevante sin costos prohibitivos
# Buen balance detalle/eficiencia; ampliamente usado en SDM regionales.
# Recomendado para Sudamérica a escala regional/continental.

#Add Bioclim data #### descarga del proyecto actual 
#bioclim_data <- worldclim_global(var = "bio", res = 2.5, path = "Data/")

#bioclim_data <- worldclim_global(var = "bio",
#                                 res = 10,
#                                 path = "Data/")


# First, load some predictor rasters from the dismo folder:
#files <- list.files(path=paste(system.file(package='dismo'), '/ex', sep=''),
#                    pattern='grd', full.names=TRUE)

#rm(bioclim_data)

# First, load some predictor rasters

path <- "Data/climate/wc2.1_2.5m"

files <- list.files(path, pattern='tif$', full.names=TRUE)

files

envs <- stack(files)

class(envs)


# Put the rasters into a RasterStack:
envs <- stack(files)

library(raster)

# tu stack ya creado
# envs <- stack(files)

# definir extensión: xmin, xmax, ymin, ymax
ext_sa <- extent(-82, -34, -56, 13)

# recortar (devuelve un RasterStack en memoria)
envs_crop <- crop(envs, ext_sa)

# guardar en disco (opcional, evita mantener todo en RAM)
writeRaster(envs_crop, filename = "Data/worldclim_bio_2.5_sudamerica.tif",
            format = "GTiff", overwrite = TRUE)

envs = envs_crop

rm(envs_crop)

summary(envs)


res(envs[[16]])

# Plot first raster in the stack, bio1
plot(envs[[16]], main=names(envs)[16])

# eliminar capas con varianza cer0
# Colinealidad 

envs_t <- envs %>%  terra::rast(envs)

source("Scripts/correlation_pipeline_terra.R")

# Ver la lista de capas eliminadas y comprobar que existen en el original

removed_names <- c("wc2.1_2.5m_bio_6","wc2.1_2.5m_bio_11","wc2.1_2.5m_bio_12",
                   "wc2.1_2.5m_bio_9","wc2.1_2.5m_bio_16","wc2.1_2.5m_bio_1",
                   "wc2.1_2.5m_bio_4","wc2.1_2.5m_bio_10","wc2.1_2.5m_bio_17")

# nombres en el SpatRaster original
names(envs_t)

# comprobar cuáles de removed_names están realmente en envs_t
present <- removed_names %in% names(envs_t)
data.frame(name = removed_names, present = present)

# Extraer las capas eliminadas (crear SpatRaster con solo esas capas)

# Extraer por nombre (si están presentes)
envs_removed <- envs_t[[ removed_names[ removed_names %in% names(envs_t) ] ]]

# Ver resumen
plot(envs_removed)

# Visualizar las capas eliminadas (mosaico / panel)

# Mostrar la primera capa
# Abrir explícitamente el dispositivo gráfico de RStudio (Windows)
if (.Platform$OS.type == "windows") windows()

# En macOS:
# quartz()

# En Linux:
# X11()

# Luego volver a plotear
terra::plot(envs_removed, nc = 3, nr = 3)


# quitar por nombre
to_remove <- c("wc2.1_2.5m_bio_6","wc2.1_2.5m_bio_11","wc2.1_2.5m_bio_12",
               "wc2.1_2.5m_bio_9","wc2.1_2.5m_bio_16","wc2.1_2.5m_bio_1",
               "wc2.1_2.5m_bio_4","wc2.1_2.5m_bio_10","wc2.1_2.5m_bio_17")
keep <- setdiff(names(envs), to_remove)
envs <- subset(envs, keep)


# Luego volver a plotear
if (.Platform$OS.type == "windows") windows()

terra::plot(envs, nc = 3, nr = 3)
# Add points for all the occurrence points onto the raster
#points(occs)

# There are some points all the way to the south-east, far from all others. Let's say we know that this represents a subpopulation that we don't want to include, and want to remove these points from the analysis. We can find them by first sorting the occs table by latitude.
head(occs[order(occs$latitude),])

## 

occs$longitude = as.numeric(occs$longitude)

occs$latitude = as.numeric(occs$latitude)

# Make a SpatialPoints object
occs.sp <- occs %>% dplyr::select(longitude, latitude) %>% SpatialPoints()

# Get the bounding box of the points
bb <- bbox(occs.sp)

# Add 5 degrees to each bound by stretching each bound by 10, as the resolution is 0.5 degree.
bb.buf <- extent(bb[1]-10, bb[3]+10, bb[2]-10, bb[4]+10)

# Crop environmental layers to match the study extent
envs.backg <- crop(envs, bb.buf)

## 
# Get a simple world countries polygon
library(rnaturalearth)
library(sf)

# Cargar países del mundo como objeto sf
world <- ne_countries(scale = "medium", returnclass = "sf")


ca.sa <- world[world$subregion %in% c("South America", "Central America"), ]


# Both spatial objects have the same geographic coordinate system with slightly different specifications, so just name the coordinate reference system (crs) for ca.sa with that of
# envs.backg to ensure smooth geoprocessing.
crs(envs.backg) <- crs(ca.sa)

# Mask envs by this polygon after buffering a bit to make sure not to lose coastline.
library(terra)

ca.sa.v <- vect(ca.sa)          # convertir sp → SpatVector
ca.sa.buf <- buffer(ca.sa.v, width = 1)

envs.backg <- mask(envs.backg, ca.sa)

# Let's check our work. We should see Central and South America without the Carribbean.
if (.Platform$OS.type == "windows") windows()
plot(envs.backg[[1]], main=names(envs.backg)[1])
points(occs)




##
# Reduce spatial autocorrelation due to putative biased species sampling----
# observation records are often spatially autocorrelated due to uneven or biased species sampling
# this will further reduce your dataset

library(spThin)

# verify the data
head(occs)

names(occs)

occs = occs %>% 
  mutate(species = "C_pro") %>% 
  dplyr::select(species, longitude, latitude)


#-
# 1. Cargar raster de referencia para eliminar duplicaciones en grillas
#-
r <- rast(envs[[1]])  # tu raster ambiental (terra SpatRaster)

library(terra)

# 1. Asegurar coordenadas numéricas
occs$longitude <- as.numeric(occs$longitude)
occs$latitude  <- as.numeric(occs$latitude)

# 2. Asegurar CRS del raster
if (is.na(crs(r)) | crs(r) == "") {
  crs(r) <- "EPSG:4326"
}

# 3. Crear SpatVector
pts <- vect(occs, geom = c("longitude", "latitude"), crs = crs(r))

# 4. Extraer matriz de coordenadas
xy <- crds(pts)

# 5. Extraer ID de celda
occs$cell_id <- cellFromXY(r, xy)

# 6. Eliminar duplicados por celda
occs_unique <- occs[!duplicated(occs$cell_id), ]
occs_unique$cell_id <- NULL


occs = occs_unique

dim(occs)

# Let's check our work. We should see Central and South America without the Carribbean.
if (.Platform$OS.type == "windows") windows()
plot(envs.backg[[1]], main=names(envs.backg)[1])
points(occs |> dplyr::select(-species))

# Cierra todos los dispositivos gráficos abiertos (equivalente a cerrar la ventana de plots)
graphics.off()



# I. - Barrido de distancias con spThin (ejecutar thin para varios thin.par y guardar resultados):

library(spThin)
distancias <- seq(from = 0, to = 20, by = 0.25) # km; ajusta según resolución y biología
reps <- 100
results <- data.frame(thin.par=distancias, max_retained=NA, mean_retained=NA, sd_retained=NA)

for(i in seq_along(distancias)){
  th <- thin(loc.data = occs,
             lat.col = "latitude",
             long.col = "longitude",
             spec.col = "species",
             thin.par = distancias[i],
             reps = reps,
             locs.thinned.list.return = TRUE,
             write.files = FALSE,
             verbose = FALSE)
  retained <- sapply(th, nrow)
  results$max_retained[i]  <- max(retained)
  results$min_retained[i]  <- min(retained)
  results$mean_retained[i] <- mean(retained)
  results$sd_retained[i]   <- sd(retained)
}
plot(results$thin.par, results$max_retained, type="b", xlab="thin.par (km)", ylab="Máx registros retenidos")

library(ggplot2)

p_records_retainedVs_thinkm = ggplot(results, 
                                     aes(x = thin.par, 
                                         y = (100-100*(1028-mean_retained)/1028))) +
  geom_line() +
  geom_point(alpha=0.5) +
  theme_bw() +
  labs(
    x = "thin.par (km)",
    y = "% of records retained"
  )


ggsave("Output/figure/p_records_retainedVs_thinkm.png", plot = p_records_retainedVs_thinkm,
       width = 17.4, height = 9, units = "cm",
       dpi = 300, type = "cairo")

ggsave("Output/figure/p_records_retainedVs_thinkm.pdf", plot = p_records_retainedVs_thinkm,
       width = 17.4, height = 9, units = "cm",
       device = cairo_pdf)

ggsave("Output/figure/p_records_retainedVs_thinkm.svg", plot = p_records_retainedVs_thinkm,
       width = 17.4, height = 9, units = "cm",
       device = cairo_pdf)



# Autocorrelación espacial ----

# Nearest Neighbor Analysis (NNA) to quantify spatial autocorrelation and aggregation in ecological data

#p_NNA_aggregation_p50

pacman::p_load(spatstat.geom)

# 1. Coordenadas únicas
occs_unique <- unique(occs[, c("longitude", "latitude")])

# 2. Crear ventana
win <- owin(
  xrange = range(occs_unique$longitude),
  yrange = range(occs_unique$latitude)
)

# 3. Crear patrón de puntos
pp <- ppp(
  x = occs_unique$longitude,
  y = occs_unique$latitude,
  window = win
)

?nndist
# 4. Distancias al vecino más cercano
nn <- nndist(pp)

# 5. Estadísticas descriptivas
summary(nn)

hist(nn, breaks = 30, main = "Distancias al vecino más cercano",
     xlab = "Distancia (grados)")
lines(density(nn), col = "red", lwd = 2)

nn_km <- nn * 111.32
summary(nn_km)

summary(nn_km)
max(nn_km)
quantile(nn_km, 0.95)


#-
# 1. Filtrar outliers con método IQR
#-
x <- nn_km

Q1  <- quantile(x, 0.25, na.rm = TRUE)
Q3  <- quantile(x, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lim_inf <- Q1 - 1.5 * IQR
lim_sup <- Q3 + 1.5 * IQR

nn_km_no_out <- x[x >= lim_inf & x <= lim_sup]

#-
# 2. Calcular medianas
#-
mediana_con_out  <- median(x, na.rm = TRUE)
mediana_sin_out  <- median(nn_km_no_out, na.rm = TRUE)

#-
# 3. Crear dataframe filtrado
#-
df <- data.frame(nn_km = nn_km_no_out)

#-
# 4. Histograma con ambas medianas anotadas
#-
p_NNA_aggregation_p50 = ggplot(df, aes(x = nn_km)) +
  geom_histogram(
    bins = 30,
    fill = "grey80",
    color = "black"
  ) +
  # Línea de mediana sin outliers
  geom_vline(
    xintercept = mediana_sin_out,
    color = "red",
    linetype = "dashed",
    size = 1
  ) +
  # Línea de mediana con outliers
  geom_vline(
    xintercept = mediana_con_out,
    color = "blue",
    linetype = "dotted",
    size = 1
  ) +
  # Anotación mediana sin outliers
  annotate(
    "text",
    x = mediana_sin_out+7.5,
    y = 50,
    label = paste0("Median(NNA) = ", round(mediana_sin_out, 2), " km"),
    vjust = -0.5,
    color = "red",
    size = 4
  ) +
  # Anotación mediana con outliers
  annotate(
    "text",
    x = mediana_con_out+10.5,
    y = 100,
    label = paste0("Median(NNA_No_Outliers) = ", round(mediana_con_out, 2), " km"),
    vjust = -1.5,
    color = "blue",
    size = 4
  ) + 
  theme_bw() +
  labs(
    x = "Distance to nearest neighbor (km)",
    y = "Frequency"#,
    #title = "Histograma de distancias al vecino más cercano\n(con y sin outliers)"
  )


ggsave("Output/figure/p_NNA_aggregation_p50.png", plot = p_NNA_aggregation_p50,
       width = 17.4, height = 9, units = "cm",
       dpi = 300, type = "cairo")

ggsave("Output/figure/p_NNA_aggregation_p50.pdf", plot = p_NNA_aggregation_p50,
       width = 17.4, height = 9, units = "cm",
       device = cairo_pdf)

ggsave("Output/figure/p_NNA_aggregation_p50.svg", plot = p_NNA_aggregation_p50,
       width = 17.4, height = 9, units = "cm",
       device = cairo_pdf)


# Capacidad de dispersión: 

# https://consensus.app/search/natural-dispersal-capacity-of-calotropis-procera/Hjao8Y2sQcW_-lcb-f5t3Q/?utm_source=share&utm_medium=clipboard

# - Main vector	Wind (anemochory) via silky pappus	
# - Common dispersal distances	0–55 m (short-distance peak)	
# - Maximum recorded natural flight	Up to ~1.8 km in open plains	

# 1. Convertir tus coordenadas únicas a sf

?thin

# create a thinned dataset #### (0,1,2,3,5,10)
thin_data <- thin(loc.data = occs, lat.col = "latitude", long.col = "longitude", spec.col = "species", 
                  thin.par = 20, reps = 50, locs.thinned.list.return = T, write.files = T, 
                  max.files = 3, out.dir = "Data/Points/20km", out.base = "sp", 
                  write.log.file = TRUE, log.file = "Data/Points/thin_sp1_log20km.txt")

#?thin
# NOTES: the thinning parameter (thin.par) controls the spatial distance in km between species observations
# use knowledge of your species and the spacial resolution of covariates to set the thinning parameter 
# reps indicates the number of iterations assigned. must run enough reps to ensure convergence
# Plot 1: Gives you the number of records retained per iteration
# Plot 2: Is the same as Plot 1 but with a log scale, so that it is easier to see if too many points or too many iterations are used
# Plot 3: Gives you the frequency of the maximum records retained



# import the thinned dataset to proceed (assuming files are in home directory)
# "*thin1.csv" shown as example. choose appropriate directory and thinned dataset to import
occs_0km <- import("Data/Points/0km/sp_thin1.csv")

dim(occs_0km)

occs_1km <- import("Data/Points/1km/sp_thin1.csv")

dim(occs_1km)

occs_2km <- import("Data/Points/2km/sp_thin2.csv")

dim(occs_2km)

occs_3km <- import("Data/Points/3km/sp_thin1.csv")

dim(occs_3km)

occs_4km <- import("Data/Points/4km/sp_thin1.csv")

dim(occs_4km)

occs_5km <- import("Data/Points/5km/sp_thin1.csv")

dim(occs_5km)

occs_6km <- import("Data/Points/6km/sp_thin1.csv")

dim(occs_6km)

occs_7km <- import("Data/Points/7km/sp_thin1.csv")

dim(occs_7km)

occs_8km <- import("Data/Points/8km/sp_thin1.csv")

dim(occs_8km)

occs_9km <- import("Data/Points/9km/sp_thin1.csv")

dim(occs_9km)


occs_10km <- import("Data/Points/10km/sp_thin1.csv")

dim(occs_10km)

occs_20km <- import("Data/Points/20km/sp_thin1.csv")

dim(occs_20km)

occs_20km = occs_20km %>% dplyr::select(longitude,latitude)


occs_10km = occs_10km %>% dplyr::select(longitude,latitude)

dim(occs_10km)
dim(occs)

# Paquetes necesarios
library(ggplot2)
library(sf)
library(rnaturalearth)
library(viridis)
library(hexbin)
library(MASS)

# tu data: occs = occs_10km %>% select(longitude, latitude)
# asegurarse de que no haya NA
occs <- na.omit(occs)

# convertir a sf (WGS84)
occs_sf <- st_as_sf(occs, coords = c("longitude", "latitude"), crs = 4326)

# bbox de Sudamérica
xmin <- -82; xmax <- -34; ymin <- -56; ymax <- 13

# límite de países (mapa base)
sa <- ne_countries(scale = "medium", returnclass = "sf")
sa_crop <- st_crop(sa, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)

# Opción A — Kernel density con stat_density_2d (relleno + contornos)

p_kde <- ggplot() +
  geom_sf(data = sa_crop, fill = "grey95", color = "grey60", size = 0.3) +
  stat_density_2d(data = occs, 
                  aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                  geom = "polygon", contour = TRUE, n = 200, bins = 12) +
  geom_point(data = occs, aes(x = longitude, y = latitude),
             color = "black", size = 0.3, alpha = 0.25) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Densidad (kernel)") +
  scale_alpha(range = c(0.15, 0.6), guide = "none") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  labs(#title = "Densidad kernel de ocurrencias",
       #subtitle = "Puntos y niveles de densidad — Sudamérica",
       x = "Longitud", y = "Latitud") +
  theme_minimal(base_size = 12)

print(p_kde)

# Opción B — Binned density con hexágonos (geom_hex)

p_hex <- ggplot() +
  geom_sf(data = sa_crop, fill = "grey98", color = "grey70", size = 0.25) +
  geom_hex(data = occs, aes(x = longitude, y = latitude, fill = ..count..),
           bins = 30, color = NA) +
  geom_point(data = occs, aes(x = longitude, y = latitude),
             color = "black", size = 0.3, alpha = 0.25) +
  scale_fill_viridis_c(trans = "sqrt", name = "N by Hex") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  labs(#title = "Densidad binned (hex) de ocurrencias",
       #subtitle = "Hex bins muestran sesgos de muestreo",
       x = "Longitud", y = "Latitud") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p_hex)


# Opción A — Kernel density con stat_density_2d (relleno + contornos)

p_kde10km <- ggplot() +
  geom_sf(data = sa_crop, fill = "grey95", color = "grey60", size = 0.3) +
  stat_density_2d(data = occs_10km, 
                  aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                  geom = "polygon", contour = TRUE, n = 200, bins = 12) +
  geom_point(data = occs, aes(x = longitude, y = latitude),
             color = "black", size = 0.3, alpha = 0.25) +
  scale_fill_viridis_c(option = "magma", direction = -1, name = "Densidad (kernel)") +
  scale_alpha(range = c(0.15, 0.6), guide = "none") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  labs(#title = "Densidad kernel de ocurrencias",
    #subtitle = "Puntos y niveles de densidad — Sudamérica",
    x = "Longitud", y = "Latitud") +
  theme_minimal(base_size = 12)

print(p_kde10km)

# Opción B — Binned density con hexágonos (geom_hex)

p_hex10km <- ggplot() +
  geom_sf(data = sa_crop, fill = "grey98", color = "grey70", size = 0.25) +
  geom_hex(data = occs, aes(x = longitude, y = latitude, fill = ..count..),
           bins = 30, color = NA) +
  geom_point(data = occs_10km, aes(x = longitude, y = latitude),
             color = "black", size = 0.3, alpha = 0.25) +
  scale_fill_viridis_c(trans = "sqrt", name = "N by Hex") +
  coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
  labs(#title = "Densidad binned (hex) de ocurrencias",
    #subtitle = "Hex bins muestran sesgos de muestreo",
    x = "Longitud", y = "Latitud") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p_hex10km)


# Recomendación de theme para consistencia
common_theme <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.7, "lines"),
    plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")  # top, right, bottom, left
  )

# Aplicar al plot antes de guardar
p_kde_final <- p_kde + common_theme
p_hex_final <- p_hex + common_theme
p_kde10km_final <- p_kde10km + common_theme
p_hex10km_final <- p_hex10km + common_theme

outdir <- "Output/figure"
#dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Función auxiliar para guardar en los 3 formatos
save_map_variants <- function(plot_obj, name, width_cm = 19.4, height_cm = 11) {
  # PDF (vector)
  ggsave(filename = file.path(outdir, paste0(name, ".pdf")),
         plot = plot_obj,
         width = width_cm, height = height_cm, units = "cm", useDingbats = FALSE)
  # SVG (vector)
  ggsave(filename = file.path(outdir, paste0(name, ".svg")),
         plot = plot_obj,
         width = width_cm, height = height_cm, units = "cm",
         device = "svg")
  # PNG (raster alta calidad)
  ggsave(filename = file.path(outdir, paste0(name, ".png")),
         plot = plot_obj,
         width = width_cm, height = height_cm, units = "cm",
         dpi = 300, type = "cairo", bg = "white")
}

# Guardar tus mapas
save_map_variants(p_kde_final, "p_kde")
save_map_variants(p_hex_final, "p_hex")
save_map_variants(p_kde10km_final, "p_kde10km")
save_map_variants(p_hex10km_final, "p_hex10km")


# Cierra todos los dispositivos gráficos abiertos (equivalente a cerrar la ventana de plots)
#graphics.off()


# define the background #### 

occs = occs_10km #|> dplyr::select(-species)

# To help ensure we do not include areas that are suitable for our species 
# but are unoccupied due to limitations like dispersal constraints, we will
# conservatively define the background extent as an area surrounding our 
# occurrence localities (VanDerWal et al. 2009, Merow et al. 2013). 

# We'll now experiment with a different spatial R package called sf (simple features).
# Let's make our occs into a sf object -- as the coordinate reference system (crs) for these 
# points is WGS84, a geographic crs (lat/lon) and the same as our envs rasters, we specify it 
# as the RasterStack's crs.
occs.sf <- sf::st_as_sf(occs, coords = c("longitude","latitude"), crs = raster::crs(envs))

# Now, we project our point data to an equal-area projection, which converts our 
# degrees to meters, which is ideal for buffering (the next step). 
# We use the typical Eckert IV projection.
eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
occs.sf <- sf::st_transform(occs.sf, crs = eckertIV)

# Buffer all occurrences by 900 km, union the polygons together 
# (for visualization), and convert back to a form that the raster package 
# can use. Finally, we reproject the buffers back to WGS84 (lat/lon).
# We choose 900 km here to avoid sampling the Caribbean islands.
occs.buf <- sf::st_buffer(occs.sf, dist = 900000) %>% 
  sf::st_union() %>% 
  sf::st_sf() %>%
  sf::st_transform(crs = raster::crs(envs))
plot(envs[[1]], main = names(envs)[1])
points(occs)
# To add sf objects to a plot, use add = TRUE
plot(occs.buf, border = "blue", lwd = 3, add = TRUE)


# Plot 


# Crop environmental rasters to match the study extent
envs.bg <- raster::crop(envs, occs.buf)
# Next, mask the rasters to the shape of the buffers
envs.bg <- raster::mask(envs.bg, occs.buf)
plot(envs.bg[[1]], main = names(envs)[1])
points(occs)
plot(occs.buf, border = "blue", lwd = 3, add = TRUE)

# In the next step, we’ll sample 10,000 random points from the background 
# (note that the number of background points is also a consideration you 
# should make with respect to your own study).

envs.bg[[1]]

# dimensions : 1083, 1152, 1247616  (nrow, ncol, ncell)

# Randomly sample 10,000 background points from one background extent raster 
# (only one per cell without replacement). Note: Since the raster has <10,000 pixels, 
# you'll get a warning and all pixels will be used for background. We will be sampling 
# from the biome variable because it is missing some grid cells, and we are trying to 
# avoid getting background points with NA. If one raster in the stack has NAs where the
# other rasters have data, ENMeval internally converts these cells to NA.
bg <- dismo::randomPoints(raster::raster(envs.bg[[1]]), n = 10000) %>% as.data.frame()
colnames(bg) <- colnames(occs)

# Notice how we have pretty good coverage (every cell).
plot(envs.bg[[1]])
points(bg, pch = 20, cex = 0.2)


# Partitioning Occurrences for Evaluation ####

# In this section, we explain and illustrate these different functions. 
# We also demonstrate how to make informative plots of partitions and the 
# environmental similarity of partitions to the background or study extent.

# Running ENMeval #### 

# Initial considerations ####

# Although any algorithm can potentially be specified in ENMeval 2.0 
# (see below), and ENMeval 2.0 has built-in implementations for Maxent 
# and BIOCLIM, we will explain here the tuning procedure for Maxent models
# (either maxent.jar or maxnet).

# ENMevaluate() builds a separate model for each unique combination 
# of RM values and feature class combinations. For example, the 
# following call will build and evaluate 2 models. One with RM=1 and 
# another with RM=2, both allowing only linear features.
# We may, however, want to compare a wider range of models that can use 
# a wider variety of feature classes and regularization multipliers:
#install.packages("ecospat")
#library(ecospat)

rm_vals <- seq(0.5, 8, by = 0.5)

envs <- terra::rast(envs.bg)

e.mx <- ENMevaluate(occs = occs, envs = envs, bg = bg, 
                    algorithm = 'maxnet', partitions = 'block', 
                    tune.args = list(fc = c("L","LQ","LQH"), rm = rm_vals))

#e.mx

# Visualizing tuning results #### 

# Here, we will plot average validation AUC and omission rates for the models 
# we tuned. The x-axis is the regularization multiplier, and the color 
# of the points and lines represents the feature class.

# https://en.wikipedia.org/wiki/Receiver_operating_characteristic

# https://nsojournals.onlinelibrary.wiley.com/doi/full/10.1111/ecog.02881

# https://www.researchgate.net/publication/264322936_Modeling_the_Spatial_Impact_of_Climate_Change_on_Grevy%27s_Zebra_Equus_grevyi_niche_in_Kenya/figures?lo=1

evalplot.stats(e = e.mx, stats = "or.mtp", color = "fc", x.var = "rm")

# We can plot more than one statistic at once with ggplot facetting.
evalplot.stats(e = e.mx, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm")

# Sometimes the error bars make it hard to visualize the plot, so we can try turning them off.
evalplot.stats(e = e.mx, stats = c("auc.val"), color = "fc", x.var = "rm", 
               error.bars = T)

#?evalplot.stats


# We can also fiddle with the dodge argument to jitter the positions of overlapping points.
evalplot.stats(e = e.mx, stats = c("AICc"), color = "fc", x.var = "rm", 
               dodge = 0.5)


# Finally, we can switch which variables are on the x-axis and which symbolized by color.
# ENMeval currently only accepts two variables for plotting at a time.
evalplot.stats(e = e.mx, stats = c("or.mtp", "auc.val"), color ="fc" , x.var = "rm" , 
               error.bars = F)

# Model selection ####

# Once we have our results, we will want to select one or more models 
# that we think are optimal across all the models we ran.

# Overall results
res2 <- eval.results(e.mx)

rio::export(res2,"Output/table/model_selectio.csv")

# Select the model with delta AICc equal to 0, or the one with the lowest AICc score.
# In practice, models with delta AICc scores less than 2 are usually considered 
# statistically equivalent.
opt.aicc <- res2 %>% filter(delta.AICc == 0)
opt.aicc
# This dplyr operation executes the sequential criteria explained above.
opt.seq <- res2 %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))
opt.seq


# Let’s now choose the optimal model settings based on the sequential 
# criteria and examine it.


# Composite score + heatmaps + save (PDF, SVG, PNG) using ggsave
# Requirements: tidyverse, viridis, ggrepel, patchwork, (optional) ragg, svglite
library(tidyverse)
library(viridis)
library(ggrepel)
library(patchwork)

# Optional backends
has_ragg <- requireNamespace("ragg", quietly = TRUE)
has_svglite <- requireNamespace("svglite", quietly = TRUE)

# ---------- User settings ----------
csv_file <- "Output/table/model_selectio.csv"   # adjust if needed
outdir <- "Output/figure"
#dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Composite weights (sum must be 1)
w_auc  <- 0.40   # weight for AUC validation (higher = prioritize discrimination)
w_or   <- 0.20   # weight for OR.10p.avg (lower is better)
w_diff <- 0.20   # weight for AUC diff (lower is better)
w_aicc <- 0.20   # weight for AICc (lower is better)

# ---------- Read and prepare data ----------
df <- read.csv(csv_file, stringsAsFactors = FALSE) %>%
  mutate(
    fc = factor(fc, levels = unique(fc)),
    rm = as.numeric(rm),
    rm_f = factor(rm, levels = sort(unique(rm)))
  )

# ---------- Normalization helpers ----------
minmax <- function(x) {
  rng <- range(x, na.rm = TRUE)
  if (rng[1] == rng[2])
    return(rep(0.5, length(x)))
  (x - rng[1]) / (rng[2] - rng[1])
}

# ---------- Compute normalized scores and composite ----------
df2 <- df %>%
  mutate(
    auc_val_s  = minmax(auc.val.avg),
    # larger better
    or10p_s     = 1 - minmax(or.10p.avg),
    # smaller better -> invert
    aucdiff_s   = 1 - minmax(auc.diff.avg),
    # smaller better -> invert
    aicc_s      = 1 - minmax(AICc),
    # smaller better -> invert
    composite   = w_auc * auc_val_s + w_or * or10p_s + w_diff * aucdiff_s + w_aicc * aicc_s
  ) %>%
  arrange(desc(composite))

# Print top candidates
top10 <- df2 %>% dplyr::select(fc, rm, composite, auc.val.avg, or.10p.avg, auc.diff.avg, AICc) %>% slice(1:10)
print(top10)

# Print where LQH_rm=1 if present
print(df2 %>% filter(fc == "LQH", rm == 1))

# ---------- Common theme (English labels) ----------
common_theme <- theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold")
  )

# ---------- Individual heatmaps ----------
p_aicc <- ggplot(df2, aes(x = rm_f, y = fc, fill = AICc)) +
  geom_tile(color = "grey80") +
  scale_fill_viridis(option = "magma",
                     direction = -1,
                     name = "AICc") +
  labs(title = "AICc by fc × rm", x = "Regularization multiplier (rm)", y = "Feature class (fc)") +
  common_theme +
  geom_point(
    data = df2 %>% filter(AICc == min(AICc, na.rm = TRUE)),
    aes(x = factor(rm), y = fc),
    color = "cyan",
    size = 4,
    shape = 21,
    stroke = 1.2
  ) +
  geom_label_repel(
    data = df2 %>% filter(AICc == min(AICc, na.rm = TRUE)),
    aes(
      x = factor(rm),
      y = fc,
      label = paste0("min AICc\nAICc=", round(AICc, 1))
    ),
    nudge_y = 0.3,
    size = 3,
    fill = "white"
  )

p_auc <- ggplot(df2, aes(x = rm_f, y = fc, fill = auc.val.avg)) +
  geom_tile(color = "grey80") +
  scale_fill_viridis(option = "magma",
                     direction = 1,
                     name = "AUC (validation)") +
  labs(title = "AUC (validation) by fc × rm", x = "Regularization multiplier (rm)", y = "Feature class (fc)") +
  common_theme +
  {
    # mark max AUC and min OR10p (if same row both will be highlighted)
    row_max_auc <- df2 %>% filter(auc.val.avg == max(auc.val.avg, na.rm = TRUE)) %>% slice(1)
    row_min_or  <- df2 %>% filter(or.10p.avg == min(or.10p.avg, na.rm = TRUE)) %>% slice(1)
    list(
      geom_point(
        data = row_max_auc,
        aes(x = factor(rm), y = fc),
        color = "red",
        size = 4,
        shape = 21,
        stroke = 1.2
      ),
      geom_label_repel(
        data = row_max_auc,
        aes(
          x = factor(rm),
          y = fc,
          label = paste0("max AUC\nauc=", round(auc.val.avg, 3))
        ),
        nudge_y = 0.3,
        size = 3,
        fill = "white"
      ),
      geom_point(
        data = row_min_or,
        aes(x = factor(rm), y = fc),
        color = "blue",
        size = 4,
        shape = 21,
        stroke = 1.2
      ),
      geom_label_repel(
        data = row_min_or,
        aes(
          x = factor(rm),
          y = fc,
          label = paste0("min OR10p\nor10p=", round(or.10p.avg, 3))
        ),
        nudge_y = -0.3,
        size = 3,
        fill = "white"
      )
    )
  }

p_or10p <- ggplot(df2, aes(x = rm_f, y = fc, fill = or.10p.avg)) +
  geom_tile(color = "grey80") +
  scale_fill_viridis(option = "magma",
                     direction = 1,
                     name = "OR 10% (avg)") +
  labs(title = "OR 10% (avg) by fc × rm", x = "Regularization multiplier (rm)", y = "Feature class (fc)") +
  common_theme

# AUC diff heatmap (risk of overfitting)
th_risk <- quantile(df2$auc.diff.avg, probs = 0.9, na.rm = TRUE)
p_diff <- ggplot(df2, aes(x = rm_f, y = fc, fill = auc.diff.avg)) +
  geom_tile(color = "grey80") +
  scale_fill_viridis(option = "magma",
                     direction = 1,
                     name = "AUC diff\n(train - val)") +
  labs(title = "AUC diff (train - validation) by fc × rm", x = "Regularization multiplier (rm)", y = "Feature class (fc)") +
  common_theme +
  geom_point(
    data = df2 %>% filter(auc.diff.avg >= th_risk),
    aes(x = rm_f, y = fc),
    shape = 4,
    color = "black",
    size = 2
  )

# Composite heatmap
p_comp <- ggplot(df2, aes(x = rm_f, y = fc, fill = composite)) +
  geom_tile(color = "grey80") +
  scale_fill_viridis(option = "magma",
                     direction = 1,
                     name = "Composite score") +
  labs(title = "Composite score (AUC val, OR10p, AUC diff, AICc)", x = "Regularization multiplier (rm)", y = "Feature class (fc)") +
  common_theme +
  geom_point(
    data = df2 %>% slice(1:3),
    aes(x = factor(rm), y = fc),
    color = "red",
    size = 4,
    shape = 21,
    stroke = 1.2
  ) +
  geom_label_repel(
    data = df2 %>% slice(1:3),
    aes(
      x = factor(rm),
      y = fc,
      label = paste0(fc, "_rm=", rm, "\nscore=", round(composite, 3))
    ),
    nudge_y = 0.3,
    size = 3,
    fill = "white"
  )

# ---------- Combine plots (vertical stack) ----------
combined <- p_aicc / p_auc / p_or10p / p_diff / p_comp + plot_layout(heights = c(1, 1, 1, 1, 1))

# ---------- Save each figure individually and combined (PDF, SVG, PNG) ----------
save_plot_variants <- function(plot_obj,
                               name,
                               width_cm = 17.4,
                               height_cm = 6) {
  pdf_file <- file.path(outdir, paste0(name, ".pdf"))
  svg_file <- file.path(outdir, paste0(name, ".svg"))
  png_file <- file.path(outdir, paste0(name, ".png"))
  
  # PDF (cairo)
  ggsave(
    filename = pdf_file,
    plot = plot_obj,
    width = width_cm,
    height = height_cm,
    units = "cm",
    device = cairo_pdf
  )
  
  # SVG (svglite preferred)
  if (has_svglite) {
    svglite::svglite(svg_file, width = width_cm / 2.54, height = height_cm /
                       2.54)
    print(plot_obj)
    dev.off()
  } else {
    ggsave(
      filename = svg_file,
      plot = plot_obj,
      width = width_cm,
      height = height_cm,
      units = "cm",
      device = "svg"
    )
  }
  
  # PNG (ragg preferred)
  if (has_ragg) {
    ragg::agg_png(
      png_file,
      width = width_cm,
      height = height_cm,
      units = "cm",
      res = 300
    )
    print(plot_obj)
    dev.off()
  } else {
    ggsave(
      filename = png_file,
      plot = plot_obj,
      width = width_cm,
      height = height_cm,
      units = "cm",
      dpi = 300,
      type = "cairo",
      bg = "white"
    )
  }
}

# Save individual heatmaps
save_plot_variants(p_aicc,
                   "heatmap_AICc",
                   width_cm = 17.4,
                   height_cm = 6)
save_plot_variants(p_auc,
                   "heatmap_AUCval",
                   width_cm = 17.4,
                   height_cm = 6)
save_plot_variants(p_or10p,
                   "heatmap_OR10p",
                   width_cm = 17.4,
                   height_cm = 6)
save_plot_variants(p_diff,
                   "heatmap_AUCdiff",
                   width_cm = 17.4,
                   height_cm = 6)
save_plot_variants(p_comp,
                   "heatmap_composite",
                   width_cm = 17.4,
                   height_cm = 6)

# Save combined stacked figure (taller)
ggsave(
  filename = file.path(outdir, "heatmaps_all_combined.pdf"),
  plot = combined,
  width = 17.4,
  height = 30,
  units = "cm",
  device = cairo_pdf
)
ggsave(
  filename = file.path(outdir, "heatmaps_all_combined.svg"),
  plot = combined,
  width = 17.4,
  height = 30,
  units = "cm",
  device = "svg"
)
if (has_ragg) {
  ragg::agg_png(
    file.path(outdir, "heatmaps_all_combined.png"),
    width = 17.4,
    height = 30,
    units = "cm",
    res = 300
  )
  print(combined)
  dev.off()
} else {
  ggsave(
    filename = file.path(outdir, "heatmaps_all_combined.png"),
    plot = combined,
    width = 17.4,
    height = 30,
    units = "cm",
    dpi = 300,
    type = "cairo",
    bg = "white"
  )
}

# ---------- Save table of scores ----------
rio::export(df2,"Output/table/model_scores_composite.csv")


# We can select a single model from the ENMevaluation object using the tune.args of our
# optimal model.
mod.seq <- eval.models(e.mx)[[opt.seq$tune.args]]
# Here are the non-zero coefficients in our model.
mod.seq$betas
# And these are the marginal response curves for the predictor variables wit non-zero 
# coefficients in our model. We define the y-axis to be the cloglog transformation, which
# is an approximation of occurrence probability (with assumptions) bounded by 0 and 1
# (Phillips et al. 2017).
par(mar = c(3, 3, 3, 3) + 0.1)
plot(mod.seq, type = "cloglog")

# The above function plots with graphical customizations to include multiple plots on 
# the same page. 
# Clear the graphics device to avoid plotting sequential plots with these settings.
#dev.off()

# Now we plot and inspect the prediction raster for our optimal model. 

# We can select the model predictions for our optimal model the same way we did for the 
# model object above.
#pred.seq <- eval.predictions(e.mx)[[opt.seq$tune.args]]
#plot(pred.seq)

# We can also plot the binned background points with the occurrence points on top to 
# visualize where the training data is located.
#points(eval.bg(e.mx), pch = 3, col = eval.bg.grp(e.mx), cex = 0.5)
#points(eval.occs(e.mx), pch = 21, bg = eval.occs.grp(e.mx))

# Let us now explore how model complexity changes the predictions 
# in our example. We will compare the simple model built with only linear 
# feature classes and the highest regularization multiplier value we used 
# (fc=‘L’, rm=5) with the complex model built with linear, quadratic, 
# and hinge feature classes and the lowest regularization multiplier 
# value we used (i.e., fc=‘LQH’, rm=1).

# We will first examine the marginal response curves, and then the 
# mapped model model predictions. 

# # First, let's examine the non-zero model coefficients in the betas slot.
# # The simpler model has fewer model coefficients.
# mod.simple <- eval.models(e.mx)[['fc.L_rm.5']]
# mod.complex <- eval.models(e.mx)[['fc.LQH_rm.1']]
# mod.simple$betas
# length(mod.simple$betas)
# mod.complex$betas
# length(mod.complex$betas)
# # Next, let's take a look at the marginal response curves.
# # The complex model has marginal responses with more curves (from quadratic terms) and 
# # spikes (from hinge terms).
# plot(mod.simple, type = "cloglog")
# 
# plot(mod.complex, type = "cloglog")
# png(filename = "mod_seq_cloglog.png", width = 800, height = 600)
# plot(mod.seq, type = "cloglog")
# dev.off()

# Análisis “ready data” para escenarios futuros ----

# 1. Cargar y revisar los rasters CMIP6

# verificar que los rasters climáticos futuros estén perfectamente alineados 
# con el modelo: misma extensión, resolución, CRS y —sobre todo— nombres de 
# variables idénticos a los usados en el entrenamiento (wc2.1_2.5m_bio_13, etc.).

# 1. Cargar y revisar los rasters CMIP6

# plotting area into two rows to visualize the predictions   ####

names(envs)



library(terra)
library(ggplot2)
library(viridis)

# Carpeta donde están los CMIP6
cmip_dir <- "Data/cmip6_projected/climate/wc2.1_2.5m"
cmip_files <- list.files(cmip_dir, pattern = "\\.tif$", full.names = TRUE)
print(length(cmip_files))
print(basename(cmip_files)[1:5])  # muestra los primeros nombres

# 2. Verificar extensión, resolución y CRS

# Cargar baseline para comparar
baseline <- rast("Data/climate/wc2.1_2.5m/wc2.1_2.5m_bio_13.tif")

# Tomar un CMIP de ejemplo
cmip_example <- rast(cmip_files[1])

# Comparar geometría
compareGeom(baseline, cmip_example, stopOnError = FALSE)

# Mostrar resumen de resolución y CRS
cat("Baseline res:", res(baseline), "\n")
cat("CMIP res:", res(cmip_example), "\n")
cat("Baseline CRS:", crs(baseline), "\n")
cat("CMIP CRS:", crs(cmip_example), "\n")

# 3. Revisar nombres de capas y coincidencia con el modelo


# Cargar stack completo de baseline y CMIP
baseline_stack <- rast(
  list.files(
    "C:/R/Calotropis_model/Data/climate/wc2.1_2.5m",
    pattern = "\\.tif$",
    full.names = TRUE
  )
)
cmip_stack <- rast(cmip_files[1])  # ejemplo: primer CMIP

# Nombres esperados según el modelo
expected_names <- c(
  "wc2.1_2.5m_bio_13",
  "wc2.1_2.5m_bio_14",
  "wc2.1_2.5m_bio_15",
  "wc2.1_2.5m_bio_18",
  "wc2.1_2.5m_bio_19",
  "wc2.1_2.5m_bio_2",
  "wc2.1_2.5m_bio_3",
  "wc2.1_2.5m_bio_5",
  "wc2.1_2.5m_bio_7",
  "wc2.1_2.5m_bio_8"
)

# Comparar nombres
cat("Coinciden todos los nombres?: ",
    all(expected_names %in% names(cmip_stack)),
    "\n")
setdiff(expected_names, names(cmip_stack))  # muestra los que faltan

expected_names

names(cmip_stack)

# 4. Visualizar cada variable para inspección rápida
# 
# 
# # Función para graficar una capa con ggplot
# plot_raster <- function(r, title) {
#   df <- as.data.frame(r, xy = TRUE)
#   colnames(df)[3] <- "value"
#   ggplot(df, aes(x = x, y = y, fill = value)) +
#     geom_raster() +
#     scale_fill_viridis(option = "C", na.value = "transparent") +
#     coord_quickmap() +
#     labs(title = title, x = "Longitude", y = "Latitude") +
#     theme_minimal(base_size = 10)
# }
# 
# # Ejemplo: visualizar las primeras tres variables del CMIP
# for (i in 1:min(3, nlyr(cmip_stack))) {
#   print(plot_raster(cmip_stack[[i]], names(cmip_stack)[i]))
# }

# 5. Resumen estadístico por variable

stats_cmip <- data.frame(
  variable = names(cmip_stack),
  min = sapply(1:nlyr(cmip_stack), function(i)
    global(cmip_stack[[i]], fun = "min", na.rm = TRUE)[1, 1]),
  max = sapply(1:nlyr(cmip_stack), function(i)
    global(cmip_stack[[i]], fun = "max", na.rm = TRUE)[1, 1]),
  mean = sapply(1:nlyr(cmip_stack), function(i)
    global(cmip_stack[[i]], fun = "mean", na.rm = TRUE)[1, 1])
)
print(stats_cmip)

# 6. Qué verificar visualmente

# Que las extensiones y resoluciones sean idénticas al baseline.

# Que los nombres de las capas coincidan exactamente con los usados en el modelo (wc2.1_2.5m_bio_X).

# Que los valores estén en rangos similares (no escalas distintas o desplazadas).

# Que no haya grandes áreas con NA o valores extremos.

# Ready‑Data QC para todos los CMIP6 ----

library(terra)
library(dplyr)
library(ggplot2)
library(viridis)

# #-
# CONFIGURACIÓN
# #-
baseline_dir <- "Data/climate/wc2.1_2.5m"
cmip_dir <- "Data/cmip6_projected/climate/wc2.1_2.5m"

expected_names <- c(
  "wc2.1_2.5m_bio_13",
  "wc2.1_2.5m_bio_14",
  "wc2.1_2.5m_bio_15",
  "wc2.1_2.5m_bio_18",
  "wc2.1_2.5m_bio_19",
  "wc2.1_2.5m_bio_2",
  "wc2.1_2.5m_bio_3",
  "wc2.1_2.5m_bio_5",
  "wc2.1_2.5m_bio_7",
  "wc2.1_2.5m_bio_8"
)

# #-
# 1. Cargar baseline
# #-
baseline_files <- list.files(baseline_dir, pattern = "\\.tif$", full.names =
                               TRUE)
baseline_stack <- rast(baseline_files) %>% subset(expected_names)

names(baseline_stack) <- expected_names

cat("Baseline cargado con", nlyr(baseline_stack), "capas.\n")
print(names(baseline_stack))

# #-
# 2. Cargar todos los CMIP6
# #-
cmip_files <- list.files(cmip_dir, pattern = "\\.tif$", full.names = TRUE)
cat("CMIP6 encontrados:", length(cmip_files), "\n")

# #-
# 3. Función QC para cada archivo
# #-
qc_cmip <- function(file, baseline_stack, expected_names) {
  r <- rast(file)
  
  # Comparaciones
  same_crs  <- crs(r) == crs(baseline_stack)
  same_res  <- all(res(r) == res(baseline_stack))
  same_ext  <- all(ext(r) == ext(baseline_stack))
  same_nlyr <- nlyr(r) == length(expected_names)
  same_names <- all(names(r) == expected_names)
  
  # Estadísticos
  stats <- sapply(1:nlyr(r), function(i) {
    v <- global(r[[i]], fun = "mean", na.rm = TRUE)[1, 1]
    return(v)
  })
  
  data.frame(
    file = basename(file),
    CRS = same_crs,
    Resolution = same_res,
    Extent = same_ext,
    Layers = same_nlyr,
    Names = same_names,
    Mean_min = min(stats, na.rm = TRUE),
    Mean_max = max(stats, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

# #-
# 4. Ejecutar QC para todos los CMIP
# #-
#qc_results <- bind_rows(lapply(cmip_files, qc_cmip, baseline_stack, expected_names))
#print(qc_results)


# Script de corrección automática: CMIP6 “ready‑to‑model” ----

# Script corregido: procesa CMIP multi-banda -> subdirs por modelo_periodo,
# extrae variables de interés, renombra, recorta a Sudamérica, resamplea,
# guarda stack (GeoTIFF) y capas individuales (TIF + ASC), y genera logs.
library(terra)
library(dplyr)
library(stringr)

# ---------------- USER SETTINGS ----------------
baseline_dir <- "C:/R/Calotropis_model/Data/climate/wc2.1_2.5m"
cmip_dir     <- "Data/cmip6_projected/climate/wc2.1_2.5m"
out_base     <- "Data/cmip6_projected/cmip6_ready"
dir.create(out_base, recursive = TRUE, showWarnings = FALSE)

sa_ext <- ext(-82, -34, -56, 13)

expected_names <- c(
  "wc2.1_2.5m_bio_13",
  "wc2.1_2.5m_bio_14",
  "wc2.1_2.5m_bio_15",
  "wc2.1_2.5m_bio_18",
  "wc2.1_2.5m_bio_19",
  "wc2.1_2.5m_bio_2",
  "wc2.1_2.5m_bio_3",
  "wc2.1_2.5m_bio_5",
  "wc2.1_2.5m_bio_7",
  "wc2.1_2.5m_bio_8"
)

# ---------------- Baseline reference ----------------
baseline_files <- list.files(baseline_dir, pattern = "\\.tif$", full.names =
                               TRUE)
if (length(baseline_files) == 0)
  stop("No baseline files found in baseline_dir")
baseline_stack <- rast(baseline_files)
if (nlyr(baseline_stack) == length(expected_names))
  names(baseline_stack) <- expected_names
baseline_stack <- if (!grepl("4326|longlat|lonlat", crs(baseline_stack), ignore.case =
                             TRUE))
  project(baseline_stack, "EPSG:4326") else
  baseline_stack
baseline_stack <- crop(baseline_stack, sa_ext)

# ---------------- Helpers ----------------
extract_model_period <- function(bn) {
  m <- str_match(bn,
                 "wc2\\.1_2\\.5m_bioc_([A-Za-z0-9\\-]+)_ssp\\d{3}_([0-9]{4}-[0-9]{4})")
  if (!is.na(m[1, 1]))
    return(list(model = m[1, 2], period = m[1, 3]))
  m2 <- str_match(bn, "([A-Za-z0-9\\-]+)_([0-9]{4}-[0-9]{4})")
  if (!is.na(m2[1, 1]))
    return(list(model = m2[1, 2], period = m2[1, 3]))
  return(list(
    model = tools::file_path_sans_ext(bn),
    period = "unknown"
  ))
}

select_bands_by_names_or_index <- function(r, expected_names) {
  rn <- names(r)
  if (!is.null(rn) &&
      any(grepl("bio|bio_", rn, ignore.case = TRUE))) {
    clean <- function(x)
      tolower(gsub("[^a-z0-9_\\-]", "", x))
    rn_clean <- sapply(rn, clean)
    expected_clean <- sapply(expected_names, clean)
    idx <- integer(0)
    for (en in expected_clean) {
      i <- which(grepl(en, rn_clean, fixed = TRUE))
      if (length(i) == 0) {
        num <- str_extract(en, "[0-9]+$")
        if (!is.na(num))
          i <- which(grepl(paste0("_", num, "$"), rn_clean))
      }
      idx <- c(idx, if (length(i) >= 1)
        i[1]
        else
          NA_integer_)
    }
    if (sum(!is.na(idx)) >= length(expected_names))
      return(r[[idx]])
  }
  if (nlyr(r) >= length(expected_names))
    return(r[[1:length(expected_names)]])
  return(NULL)
}

# ---------------- Process function (with robust write) ----------------
process_cmip_file <- function(infile,
                              baseline_stack,
                              expected_names,
                              out_base,
                              sa_ext) {
  bn <- basename(infile)
  info <- extract_model_period(bn)
  model_name <- info$model
  period <- info$period
  model_dir <- file.path(out_base, paste0(model_name, "_", period))
  dir.create(model_dir, recursive = TRUE, showWarnings = FALSE)
  log_msgs <- character(0)
  message("Procesando: ", bn)
  r <- tryCatch(
    rast(infile),
    error = function(e) {
      log_msgs <<- c(log_msgs, paste("read_error:", e$message))
      return(NULL)
    }
  )
  if (is.null(r))
    return(list(infile = infile, ok = FALSE, log = log_msgs))
  if (is.na(crs(r)) ||
      !grepl("4326|longlat|lonlat", crs(r), ignore.case = TRUE)) {
    r <- project(r, "EPSG:4326")
    log_msgs <- c(log_msgs, "reprojected_to_4326")
  }
  r <- crop(r, sa_ext)
  log_msgs <- c(log_msgs, "cropped_SA")
  if (!compareGeom(r, baseline_stack, stopOnError = FALSE)) {
    r <- resample(r, baseline_stack, method = "bilinear")
    log_msgs <- c(log_msgs, "resampled_to_baseline")
  }
  r_sel <- select_bands_by_names_or_index(r, expected_names)
  if (is.null(r_sel)) {
    log_msgs <- c(log_msgs, "select_bands_failed")
    writeLines(log_msgs, con = file.path(
      model_dir,
      paste0(tools::file_path_sans_ext(bn), "_log.txt")
    ))
    return(list(
      infile = infile,
      ok = FALSE,
      log = log_msgs
    ))
  }
  if (nlyr(r_sel) != length(expected_names)) {
    log_msgs <- c(log_msgs, paste0("n_layers_mismatch:", nlyr(r_sel)))
    writeLines(log_msgs, con = file.path(
      model_dir,
      paste0(tools::file_path_sans_ext(bn), "_log.txt")
    ))
    return(list(
      infile = infile,
      ok = FALSE,
      log = log_msgs
    ))
  }
  names(r_sel) <- expected_names
  stats <- data.frame(
    variable = names(r_sel),
    min = NA_real_,
    max = NA_real_,
    mean = NA_real_,
    stringsAsFactors = FALSE
  )
  for (i in seq_len(nlyr(r_sel))) {
    stats[i, "min"]  <- global(r_sel[[i]], fun = "min", na.rm = TRUE)[1, 1]
    stats[i, "max"]  <- global(r_sel[[i]], fun = "max", na.rm = TRUE)[1, 1]
    stats[i, "mean"] <- global(r_sel[[i]], fun = "mean", na.rm = TRUE)[1, 1]
  }
  warnings <- character(0)
  if (any(stats$max > 10000, na.rm = TRUE))
    warnings <- c(warnings, "max > 10000")
  if (any(stats$mean > 1000, na.rm = TRUE))
    warnings <- c(warnings, "mean > 1000")
  if (any(stats$min < -100, na.rm = TRUE))
    warnings <- c(warnings, "min < -100")
  out_stack_tif <- file.path(model_dir,
                             paste0(tools::file_path_sans_ext(bn), "_ready.tif"))
  tryCatch({
    terra::writeRaster(r_sel,
                       out_stack_tif,
                       overwrite = TRUE,
                       filetype = "GTiff")
    log_msgs <- c(log_msgs, paste("wrote_stack:", out_stack_tif))
  }, error = function(e) {
    log_msgs <<- c(log_msgs, paste("write_stack_error:", e$message))
    writeLines(log_msgs, con = file.path(
      model_dir,
      paste0(tools::file_path_sans_ext(bn), "_log.txt")
    ))
    return(list(
      infile = infile,
      ok = FALSE,
      log = log_msgs
    ))
  })
  for (i in seq_len(nlyr(r_sel))) {
    varname <- names(r_sel)[i]
    out_tif <- file.path(model_dir, paste0(varname, ".tif"))
    out_asc <- file.path(model_dir, paste0(varname, ".asc"))
    tryCatch({
      terra::writeRaster(r_sel[[i]],
                         out_tif,
                         overwrite = TRUE,
                         filetype = "GTiff")
      log_msgs <- c(log_msgs, paste("wrote_tif:", out_tif))
    }, error = function(e) {
      log_msgs <<- c(log_msgs, paste("write_tif_error:", varname, e$message))
    })
    tryCatch({
      terra::writeRaster(r_sel[[i]],
                         out_asc,
                         overwrite = TRUE,
                         filetype = "AAIGrid")
      log_msgs <- c(log_msgs, paste("wrote_asc:", out_asc))
    }, error = function(e) {
      log_msgs <<- c(log_msgs, paste("write_asc_error:", varname, e$message))
    })
  }
  stats_file <- file.path(model_dir,
                          paste0(tools::file_path_sans_ext(bn), "_stats.csv"))
  write.csv(stats, stats_file, row.names = FALSE)
  if (length(warnings) > 0)
    writeLines(warnings, con = file.path(
      model_dir,
      paste0(tools::file_path_sans_ext(bn), "_warnings.txt")
    ))
  writeLines(log_msgs, con = file.path(model_dir, paste0(
    tools::file_path_sans_ext(bn), "_log.txt"
  )))
  return(
    list(
      infile = infile,
      model_dir = model_dir,
      stats = stats,
      warnings = warnings,
      ok = TRUE,
      log = log_msgs
    )
  )
}

# ---------------- Ejecutar ----------------
cmip_files <- list.files(cmip_dir, pattern = "\\.tif$", full.names = TRUE)
if (length(cmip_files) == 0)
  stop("No CMIP files found in cmip_dir")
results <- list()
for (f in cmip_files) {
  res <- tryCatch(
    process_cmip_file(f, baseline_stack, expected_names, out_base, sa_ext),
    error = function(e) {
      message("ERROR procesando ", f, ": ", e$message)
      return(list(
        infile = f,
        ok = FALSE,
        log = paste("fatal:", e$message)
      ))
    }
  )
  results[[basename(f)]] <- res
}

# Consolidar estadísticas y logs
ok_files <- sapply(results, function(x)
  ! is.null(x) && isTRUE(x$ok))
summary_list <- lapply(results[ok_files], function(x)
  x$stats %>% mutate(source = basename(x$infile)))
if (length(summary_list) > 0) {
  all_stats <- bind_rows(summary_list, .id = "source_file")
  write.csv(all_stats,
            file.path(out_base, "cmip_ready_stats_summary.csv"),
            row.names = FALSE)
} else {
  write.csv(data.frame(),
            file.path(out_base, "cmip_ready_stats_summary.csv"),
            row.names = FALSE)
}
# guardar lista de fallos
failed <- names(results)[!ok_files]
writeLines(failed, con = file.path(out_base, "cmip_ready_failed_files.txt"))
message("Proceso finalizado. OK:",
        sum(ok_files),
        " Failed:",
        length(failed))


#
# ============================
# Species distribution projection, velocity and acceleration pipeline
# ============================
# Full pipeline (complete) — uses raster::predict for maxnet predictions
# - Select best model from 'res' (composite score)
# - Crop baseline and CMIP stacks to South America
# - Predict baseline and future (if CMIP files exist)
# - Compute threshold (10th percentile), binary maps, area, centroids, velocities, accelerations
# - Save rasters and figures (PDF, SVG, PNG)
# # -



# --Seleccionar modelo LQH RM=1.5 desde ENMevaluation ----
# Asume que 'e.mx' es tu objeto ENMevaluation cargado en memoria

# 1) inspeccionar resultados para encontrar filas con FC = "LQH" y RM = 1.5
res_df <- as.data.frame(e.mx@results)
# Normalizar nombres de columnas (puede variar según versión)
print(colnames(res_df))


class(e.mx@models[["fc.LQH_rm.1.5"]])

# Definición del modelo seleccionado ----

model_maxnet = e.mx@models[["fc.LQH_rm.1.5"]]


# Script completo integrado: selección, recorte, predicción (raster::predict), deltas, aceleración,
# guardado de figuras (PDF/SVG/PNG) y registro CSV de archivos generados.
#

# ---------- Preparación y comprobaciones ----------
library(terra)
library(raster)
library(dplyr)

# Asume: e.mx ya en memoria y model_maxnet asignado como:
# model_maxnet <- e.mx@models[["fc.LQH_rm.1.5"]]

# 1) Clase y varnames
cat("Clase del modelo seleccionado:", class(model_maxnet), "\n")
if (!is.null(model_maxnet$varnames)) {
  model_varnames <- model_maxnet$varnames
} else if (!is.null(model_maxnet$coefficients)) {
  model_varnames <- names(model_maxnet$coefficients)
} else {
  model_varnames <- c(
    "wc2.1_2.5m_bio_13",
    "wc2.1_2.5m_bio_14",
    "wc2.1_2.5m_bio_15",
    "wc2.1_2.5m_bio_18",
    "wc2.1_2.5m_bio_19",
    "wc2.1_2.5m_bio_2",
    "wc2.1_2.5m_bio_3",
    "wc2.1_2.5m_bio_5",
    "wc2.1_2.5m_bio_7",
    "wc2.1_2.5m_bio_8"
  )
}
cat("Variables del modelo (primeras):",
    paste(head(model_varnames, 10), collapse = ", "),
    "\n")

# ---------- Funciones auxiliares ----------
inv_cloglog <- function(eta)
  1 - exp(-exp(eta))

predict_sample_types <- function(r_stack,
                                 model_obj,
                                 model_varnames,
                                 sample_n = 500) {
  # alinear nombres
  nvars <- length(model_varnames)
  if (nlayers(r_stack) < nvars)
    stop("Stack tiene menos capas que variables del modelo.")
  names(r_stack)[1:nvars] <- model_varnames
  # muestreo de celdas no-NA
  vals_df <- raster::sampleRandom(r_stack,
                                  size = sample_n,
                                  na.rm = TRUE,
                                  asRaster = FALSE)
  samp_df <- as.data.frame(vals_df)
  # probar tipos
  out <- list()
  for (t in c("cloglog", "logistic", "link")) {
    res <- tryCatch({
      predict(model_obj, newdata = samp_df, type = t)
    }, error = function(e)
      e)
    out[[t]] <- res
  }
  return(out)
}

# ---------- Prueba en un stack ready (sanity check) ----------
ready_example <- list.files(
  "Data/cmip6_projected/cmip6_ready",
  pattern = "_ready\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)[1]
r_stack_example <- raster::stack(ready_example)
cat("Usando ejemplo:", basename(ready_example), "\n")

sample_res <- predict_sample_types(r_stack_example, model_maxnet, model_varnames, sample_n = 500)

# Interpretación automática
choose_type <- function(sample_res) {
  # si logistic existe y sus valores están en [0,1] con medias razonables -> elegir logistic
  if (!inherits(sample_res$logistic, "error")) {
    v <- sample_res$logistic
    if (all(v >= -1e-6 &
            v <= 1 + 1e-6, na.rm = TRUE) &&
        mean(v, na.rm = TRUE) > 0.001)
      return("logistic")
  }
  # si cloglog parece devolver valores en (0,1) -> elegir cloglog
  if (!inherits(sample_res$cloglog, "error")) {
    v <- sample_res$cloglog
    if (all(v >= -1e-6 &
            v <= 1 + 1e-6, na.rm = TRUE) &&
        mean(v, na.rm = TRUE) > 0.001)
      return("cloglog")
  }
  # si link existe y al aplicar inv_cloglog da probabilidades razonables -> usar link + inv
  if (!inherits(sample_res$link, "error")) {
    vlink <- sample_res$link
    p <- inv_cloglog(vlink)
    if (all(p >= -1e-6 &
            p <= 1 + 1e-6, na.rm = TRUE) &&
        mean(p, na.rm = TRUE) > 0.001)
      return("link_inv_cloglog")
  }
  # fallback
  return("logistic")
}

chosen <- choose_type(sample_res)
cat("Tipo elegido para predicción masiva:", chosen, "\n")

# ---------- Predicción masiva (todos los ready stacks) ----------
ready_files <- list.files(
  "Data/cmip6_projected/cmip6_ready",
  pattern = "_ready\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)
out_base <- "C:/R/Calotropis_model/Output/cmip6_projected"
dir.create(out_base, recursive = TRUE, showWarnings = FALSE)

for (f in ready_files) {
  bn <- basename(f)
  model_period <- sub("_ready\\.tif$", "", bn)
  out_dir <- file.path(out_base, model_period)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_file <- file.path(out_dir, paste0("pred_", model_period, "_SA.tif"))
  message("Predict -> ", model_period)
  r_stack <- raster::stack(f)
  names(r_stack)[1:length(model_varnames)] <- model_varnames
  
  if (chosen == "link_inv_cloglog") {
    # predecir en escala enlace y transformar
    tmp_link <- tempfile(fileext = ".tif")
    r_link <- raster::predict(
      r_stack,
      model = model_maxnet,
      type = "link",
      filename = tmp_link,
      overwrite = TRUE,
      progress = "text"
    )
    # aplicar inversa del cloglog con terra para eficiencia
    r_link_spat <- rast(tmp_link)
    r_prob <- app(
      r_link_spat,
      fun = function(x)
        inv_cloglog(x)
    )
    writeRaster(r_prob,
                out_file,
                overwrite = TRUE,
                filetype = "GTiff")
  } else {
    # usar directamente cloglog o logistic
    r_pred <- tryCatch({
      raster::predict(
        r_stack,
        model = model_maxnet,
        type = chosen,
        filename = out_file,
        overwrite = TRUE,
        progress = "text"
      )
    }, error = function(e) {
      message("  predict error (",
              chosen,
              "): ",
              e$message,
              " -> intentando logistic")
      tryCatch(
        raster::predict(
          r_stack,
          model = model_maxnet,
          type = "logistic",
          filename = out_file,
          overwrite = TRUE,
          progress = "text"
        ),
        error = function(e2) {
          message("  predict logistic error: ", e2$message)
          NULL
        }
      )
    })
  }
  if (file.exists(out_file))
    message("  Guardada: ", out_file)
  else
    message("  Falló predicción para: ", model_period)
}

# A. Generar pred_baseline_SA.tif (recomendado) ----


library(raster)
library(terra)

# rutas
baseline_dir <- "C:/R/Calotropis_model/Data/climate/wc2.1_2.5m"
out_proj_dir <- "C:/R/Calotropis_model/Output/cmip6_projected/wc2.1_2.5m_bioc_baseline_1970_2000"
pred_baseline_file <- file.path(out_proj_dir, "pred_wc2.1_2.5m_bioc_pred_baseline_SA_1970_2000.tif")

# cargar baseline stack (RasterStack)
baseline_files <- list.files(baseline_dir, pattern = "\\.tif$", full.names =
                               TRUE)
r_baseline_stack <- raster::stack(baseline_files)

# Alinear nombres con el modelo
if (!is.null(model_maxnet$varnames)) {
  nvars <- length(model_maxnet$varnames)
  if (nlayers(r_baseline_stack) >= nvars) {
    names(r_baseline_stack)[1:nvars] <- model_maxnet$varnames
  } else
    stop("El baseline tiene menos capas que las variables del modelo.")
} else {
  warning("model_maxnet$varnames no disponible; usando nombres existentes del baseline.")
}

# recortar a Sudamérica (si quieres usar terra/extent)
sa_ext <- terra::ext(-82, -34, -56, 13)
# convertir a SpatRaster para recorte con terra y volver a raster si prefieres
r_baseline_spat <- terra::rast(r_baseline_stack)
r_baseline_spat <- terra::crop(r_baseline_spat, sa_ext)
r_baseline_stack2 <- raster::stack(r_baseline_spat)

# predecir y guardar (intentar cloglog, fallback logistic)
try({
  raster::predict(
    r_baseline_stack2,
    model = model_maxnet,
    filename = pred_baseline_file,
    type = "cloglog",
    overwrite = TRUE,
    progress = "text"
  )
}, silent = TRUE)

if (!file.exists(pred_baseline_file)) {
  # fallback a logistic
  raster::predict(
    r_baseline_stack2,
    model = model_maxnet,
    filename = pred_baseline_file,
    type = "logistic",
    overwrite = TRUE,
    progress = "text"
  )
}

# comprobar
if (!file.exists(pred_baseline_file))
  stop("No se pudo generar pred_baseline_SA.tif. Revisa errores anteriores.")
message("Predicción baseline guardada en: ", pred_baseline_file)

# # Paso 1: exportar mapas de predicción (PDF, SVG, PNG) ----
library(terra)
library(raster)
library(ggplot2)
library(viridis)
library(dplyr)
has_ragg <- requireNamespace("ragg", quietly = TRUE)
has_svglite <- requireNamespace("svglite", quietly = TRUE)

out_proj_dir <- "C:/R/Calotropis_model/Output/cmip6_projected"
fig_dir <- file.path(out_proj_dir, "figures", "predictions")
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# helper: reducir raster para plotting si es muy grande
reduce_for_plot <- function(r, maxdim = 1200, fact = 4) {
  if (!inherits(r, "SpatRaster"))
    r <- rast(r)
  dims <- dim(r)[1:2]
  if (max(dims) > maxdim)
    return(aggregate(
      r,
      fact = ceiling(max(dims) / maxdim),
      fun = mean,
      na.rm = TRUE
    ))
  return(r)
}


plot_and_save <- function(r,
                          title,
                          basename_out,
                          width_cm = 17,
                          height_cm = 10) {
  # reducir para plotting si es muy grande (usa tu función reduce_for_plot)
  r_small <- reduce_for_plot(r)
  df <- as.data.frame(r_small, xy = TRUE)
  colnames(df)[3] <- "value"
  
  p <- ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_viridis(option = "C",
                       na.value = "transparent",
                       name = "Suitability") +
    coord_quickmap() +
    labs(
      title = title,
      subtitle = NULL,
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(
        size = 16/3,
        face = "bold",
        hjust = 0.5,
        margin = margin(b = 6)
      ),
      plot.subtitle = element_text(
        size = 11,
        hjust = 0.5,
        margin = margin(b = 6)
      ),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.6, "lines"),
      legend.key.width = unit(0.6, "lines"),
      plot.margin = margin(
        t = 6,
        r = 6,
        b = 6,
        l = 6
      )
    )
  
  pdf_file <- file.path(fig_dir, paste0(basename_out, ".pdf"))
  svg_file <- file.path(fig_dir, paste0(basename_out, ".svg"))
  png_file <- file.path(fig_dir, paste0(basename_out, ".png"))
  
  # Guardado: PDF, SVG, PNG (alta resolución)
  tryCatch({
    ggsave(
      pdf_file,
      p,
      device = cairo_pdf,
      width = width_cm,
      height = height_cm,
      units = "cm"
    )
  }, error = function(e)
    NULL)
  if (requireNamespace("svglite", quietly = TRUE)) {
    tryCatch({
      svglite::svglite(svg_file,
                       width = width_cm / 2.54,
                       height = height_cm / 2.54)
      print(p)
      dev.off()
    }, error = function(e)
      NULL)
  } else {
    tryCatch({
      ggsave(
        svg_file,
        p,
        device = "svg",
        width = width_cm,
        height = height_cm,
        units = "cm"
      )
    }, error = function(e)
      NULL)
  }
  if (requireNamespace("ragg", quietly = TRUE)) {
    tryCatch({
      ragg::agg_png(
        png_file,
        width = width_cm,
        height = height_cm,
        units = "cm",
        res = 300
      )
      print(p)
      dev.off()
    }, error = function(e)
      NULL)
  } else {
    tryCatch({
      ggsave(
        png_file,
        p,
        width = width_cm,
        height = height_cm,
        units = "cm",
        dpi = 300
      )
    }, error = function(e)
      NULL)
  }
  
  return(c(pdf_file, svg_file, png_file))
}


# recorrer predicciones
pred_files <- list.files(
  out_proj_dir,
  pattern = "^pred_.*_SA\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)
log_rows <- list()
for (pf in pred_files) {
  message("Mapear: ", pf)
  r <- tryCatch(
    rast(pf),
    error = function(e) {
      message("  read error: ", e$message)
      NULL
    }
  )
  if (is.null(r))
    next
  stats <- global(r, fun = c("min", "mean", "max"), na.rm = TRUE)
  title <- paste0("Scenery: ", basename(dirname(pf)))
  baseout <- paste0("pred_map_", gsub("[^A-Za-z0-9_\\-]", "_", basename(dirname(pf))))
  files_saved <- plot_and_save(r, title, baseout)
  log_rows[[length(log_rows) + 1]] <- data.frame(
    source = pf,
    pdf = files_saved[1],
    svg = files_saved[2],
    png = files_saved[3],
    min = stats[1, 1],
    mean = stats[2, 1],
    max = stats[3, 1],
    stringsAsFactors = FALSE
  )
}
if (length(log_rows) > 0) {
  log_df <- bind_rows(log_rows)
  write.csv(log_df,
            file.path(fig_dir, "predictions_maps_log.csv"),
            row.names = FALSE)
  message("Mapas guardados en: ", fig_dir)
} else
  message("No se generaron mapas (no se encontraron pred_*.tif).")


# Paso 2 — Generar solo capas raster internas (delta, velocidad, aceleración) ----



# Código: crear colecciones por modelo y (opcional) copiar archivos

# 0. Preparación
library(terra)
library(dplyr)
library(fs)

base_dir <- "Output/cmip6_projected"          # donde están las carpetas por modelo_periodo
collections_dir <- file.path(base_dir, "collections_by_model")  # donde opcionalmente copiar
dir_create(collections_dir)

# 1. localizar todos los pred_*.tif (flexible)
pred_files <- list.files(
  base_dir,
  pattern = "^pred_.*\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)
if (length(pred_files) == 0)
  stop("No se encontraron archivos pred_*.tif en: ", base_dir)

# 2. funciones para extraer modelo y periodo desde el nombre de archivo
#    Ajusta las regex si tus nombres difieren.
extract_model_prefix <- function(path) {
  bn <- basename(path)
  # quitar prefijo "pred_" y sufijo "_SA" si existe
  bn2 <- sub("^pred_", "", bn)
  bn2 <- sub("_SA", "", bn2)
  # buscar patrón de periodo YYYY_YYYY o YYYY-YYYY
  m <- regexpr("_[0-9]{4}_[0-9]{4}|_[0-9]{4}-[0-9]{4}", bn2)
  if (m[1] > 0) {
    prefix <- substring(bn2, 1, m[1] - 1)
  } else {
    # si no hay periodo, tomar todo hasta la extensión
    prefix <- tools::file_path_sans_ext(bn2)
  }
  # limpiar caracteres problemáticos
  gsub("[^A-Za-z0-9_\\-]", "_", prefix)
}
extract_period <- function(path) {
  bn <- basename(path)
  m <- regmatches(bn, regexpr("[0-9]{4}_[0-9]{4}", bn))
  if (length(m) == 0 ||
      m == "")
    m <- regmatches(bn, regexpr("[0-9]{4}-[0-9]{4}", bn))
  if (length(m) == 0 || m == "") {
    # fallback: extraer primer año encontrado
    yrs <- regmatches(bn, gregexpr("[0-9]{4}", bn))[[1]]
    if (length(yrs) >= 2)
      return(paste0(yrs[1], "_", yrs[2]))
    if (length(yrs) == 1)
      return(paste0(yrs[1], "_", yrs[1]))
    return(NA_character_)
  }
  gsub("-", "_", m)
}
get_midyear <- function(period_str) {
  if (is.na(period_str))
    return(NA_real_)
  yrs <- as.numeric(strsplit(period_str, "_")[[1]])
  mean(yrs, na.rm = TRUE)
}

# 3. construir tabla con modelo, periodo, año medio, ruta
tbl <- tibble(path = pred_files) %>%
  mutate(
    model = sapply(path, extract_model_prefix),
    period = sapply(path, extract_period),
    midyear = sapply(period, get_midyear)
  )

# 4. agrupar por modelo
grouped <- split(tbl, tbl$model)

# 5. crear colecciones (opcional: copiar archivos a collections_dir/<modelo>/)
copy_collections <- TRUE   # cambia a FALSE si no quieres duplicar archivos
for (m in names(grouped)) {
  df <- grouped[[m]] %>% arrange(midyear)
  # crear carpeta de colección
  col_dir <- file.path(collections_dir, m)
  dir_create(col_dir)
  # copiar archivos (si se desea) con nombres seguros
  for (i in seq_len(nrow(df))) {
    src <- df$path[i]
    # nombre destino: <modelo>_pred_<period>.tif
    dest_name <- paste0(m, "_pred_", df$period[i], ".tif")
    dest <- file.path(col_dir, dest_name)
    if (copy_collections) {
      if (!file_exists(dest))
        file_copy(src, dest, overwrite = TRUE)
    }
  }
}

# 6. resumen: cuántos periodos por modelo y si incluye baseline (1970_2000)
summary_tbl <- lapply(names(grouped), function(m) {
  df <- grouped[[m]]
  n_periods <- nrow(df)
  has_baseline <- any(grepl("1970_2000|1970-2000|baseline", tolower(df$path)))
  data.frame(
    model = m,
    n_periods = n_periods,
    has_baseline = has_baseline,
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

print(summary_tbl)
# guardar resumen
write.csv(summary_tbl,
          file.path(collections_dir, "collections_summary.csv"),
          row.names = FALSE)
message("Colecciones creadas en: ", collections_dir)



# Bloque para procesar colecciones ya creadas ----

# Procesar colecciones físicas en collections_dir y generar derived rasters
library(terra)
library(dplyr)
library(fs)

# --- Rutas (ajusta si hace falta) ---
collections_dir <- file.path("Output", "cmip6_projected", "collections_by_model")  # colecciones por modelo
derived_dir     <- file.path("Output", "cmip6_projected", "derived_rasters")       # salida unificada
dir_create(derived_dir, recurse = TRUE)

# --- Buscar modelos (carpetas) ---
models <- list.dirs(collections_dir, full.names = FALSE, recursive = FALSE)
if (length(models) == 0)
  stop("No se encontraron colecciones en: ", collections_dir)

# --- Utilidades para extraer periodo y año medio ---
extract_period <- function(fname) {
  bn <- basename(fname)
  p <- regmatches(bn, regexpr("[0-9]{4}_[0-9]{4}", bn))
  if (length(p) == 0 ||
      p == "")
    p <- regmatches(bn, regexpr("[0-9]{4}-[0-9]{4}", bn))
  if (length(p) == 0 || p == "") {
    yrs <- regmatches(bn, gregexpr("[0-9]{4}", bn))[[1]]
    if (length(yrs) >= 2)
      return(paste0(yrs[1], "_", yrs[2]))
    if (length(yrs) == 1)
      return(paste0(yrs[1], "_", yrs[1]))
    return(NA_character_)
  }
  gsub("-", "_", p)
}
midyear_from_period <- function(period_str) {
  if (is.na(period_str))
    return(NA_real_)
  yrs <- as.numeric(strsplit(period_str, "_")[[1]])
  mean(yrs, na.rm = TRUE)
}
safe_name <- function(x)
  gsub("[^A-Za-z0-9_\\-]", "_", x)

# --- Opciones de escritura GeoTIFF ---
gdal_opts <- c("COMPRESS=LZW", "TILED=YES")
write_args <- list(
  filetype = "GTiff",
  datatype = "FLT4S",
  overwrite = TRUE,
  gdal = gdal_opts
)

# --- Resumen global ---
summary_rows <- list()

# --- Loop por cada colección (modelo) ---
for (m in models) {
  message("Colección: ", m)
  col_dir <- file.path(collections_dir, m)
  files <- list.files(col_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(files) == 0) {
    message("  (vacío) ", col_dir)
    next
  }
  
  # extraer periodos y años medios
  periods <- sapply(files, extract_period)
  midyears <- sapply(periods, midyear_from_period)
  ord <- order(midyears, na.last = TRUE)
  files <- files[ord]
  midyears <- midyears[ord]
  periods <- periods[ord]
  
  # cargar rasters y comprobar geometría; usar la primera como referencia
  ras_list <- lapply(files, rast)
  ref <- ras_list[[1]]
  # si algún raster tiene CRS vacío, asignar EPSG:4326 por defecto (solo si ref tiene CRS)
  if (is.na(crs(ref)) || crs(ref) == "")
    crs(ref) <- "EPSG:4326"
  for (j in seq_along(ras_list)) {
    if (is.na(crs(ras_list[[j]])) ||
        crs(ras_list[[j]]) == "")
      crs(ras_list[[j]]) <- crs(ref)
    if (!compareGeom(ref, ras_list[[j]], stopOnError = FALSE)) {
      message("  Re-muestreando ",
              basename(files[j]),
              " a la grilla de referencia")
      ras_list[[j]] <- resample(ras_list[[j]], ref, method = "bilinear")
    }
  }
  
  # nombre seguro para archivos de salida
  safe_m <- safe_name(m)
  
  # --- DELTA y VELOCIDAD por pares consecutivos ---
  if (length(ras_list) >= 2) {
    for (i in seq_len(length(ras_list) - 1)) {
      y1 <- midyears[i]
      y2 <- midyears[i + 1]
      if (is.na(y1) || is.na(y2)) {
        message("  Saltando par sin año detectado: ",
                basename(files[i]),
                " / ",
                basename(files[i + 1]))
        next
      }
      dt <- y2 - y1
      if (dt == 0) {
        message("  Saltando par con dt=0: ", y1, "->", y2)
        next
      }
      r1 <- ras_list[[i]]
      r2 <- ras_list[[i + 1]]
      
      delta <- (r2 - r1) / dt
      vel   <- abs(r2 - r1) / dt
      
      out_delta <- file.path(derived_dir,
                             paste0(safe_m, "_delta_", y1, "_to_", y2, ".tif"))
      out_vel   <- file.path(derived_dir,
                             paste0(safe_m, "_vel_", y1, "_to_", y2, ".tif"))
      
      do.call(terra::writeRaster, c(list(delta, filename = out_delta), write_args))
      do.call(terra::writeRaster, c(list(vel, filename = out_vel), write_args))
      
      s_delta <- global(delta,
                        fun = c("min", "mean", "max"),
                        na.rm = TRUE)
      s_vel   <- global(vel,
                        fun = c("min", "mean", "max"),
                        na.rm = TRUE)
      
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        model = m,
        type = "delta",
        period = paste0(y1, "_", y2),
        file = out_delta,
        min = s_delta[1, 1],
        mean = s_delta[2, 1],
        max = s_delta[3, 1],
        stringsAsFactors = FALSE
      )
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        model = m,
        type = "vel",
        period = paste0(y1, "_", y2),
        file = out_vel,
        min = s_vel[1, 1],
        mean = s_vel[2, 1],
        max = s_vel[3, 1],
        stringsAsFactors = FALSE
      )
      
      message("  Guardados: ",
              basename(out_delta),
              " , ",
              basename(out_vel))
    }
  } else {
    message("  Solo 1 periodo en la colección; no se calculan deltas/vel.")
  }
  
  # --- ACELERACIÓN (segunda diferencia) si hay >=3 periodos ---
  if (length(ras_list) >= 3) {
    for (i in 2:(length(ras_list) - 1)) {
      y_prev <- midyears[i - 1]
      y_curr <- midyears[i]
      y_next <- midyears[i + 1]
      if (any(is.na(c(y_prev, y_curr, y_next)))) {
        message("  Saltando aceleración por año faltante en índices: ",
                i - 1,
                i,
                i + 1)
        next
      }
      dt1 <- y_curr - y_prev
      dt2 <- y_next - y_curr
      if (dt1 == 0 || dt2 == 0) {
        message("  Saltando aceleración por dt=0 en índices: ", i - 1, i, i + 1)
        next
      }
      r_prev <- ras_list[[i - 1]]
      r_curr <- ras_list[[i]]
      r_next <- ras_list[[i + 1]]
      dt_mean <- (dt1 + dt2) / 2
      
      accel <- (r_next - 2 * r_curr + r_prev) / (dt_mean^2)
      out_acc <- file.path(derived_dir,
                           paste0(safe_m, "_accel_", y_prev, "_", y_curr, "_", y_next, ".tif"))
      
      do.call(terra::writeRaster, c(list(accel, filename = out_acc), write_args))
      
      s_acc <- global(accel,
                      fun = c("min", "mean", "max"),
                      na.rm = TRUE)
      summary_rows[[length(summary_rows) + 1]] <- data.frame(
        model = m,
        type = "accel",
        period = paste0(y_prev, "_", y_curr, "_", y_next),
        file = out_acc,
        min = s_acc[1, 1],
        mean = s_acc[2, 1],
        max = s_acc[3, 1],
        stringsAsFactors = FALSE
      )
      
      message("  Guardada aceleración: ", basename(out_acc))
    }
  } else {
    message("  Menos de 3 periodos; no se calculan aceleraciones.")
  }
}

# --- Guardar resumen CSV ---
if (length(summary_rows) > 0) {
  summary_df <- bind_rows(summary_rows)
  summary_csv <- file.path(derived_dir, "delta_vel_accel_summary.csv")
  write.csv(summary_df, summary_csv, row.names = FALSE)
  message("Resumen guardado en: ", summary_csv)
} else {
  message("No se generaron capas derivadas (revisa colecciones).")
}


# 1) listar archivos generados
list.files("Output/cmip6_projected/derived_rasters", pattern = "\\.tif$", full.names = FALSE)

# 2) comprobar estadísticas de un archivo ejemplo
r <- rast(list.files("Output/cmip6_projected/derived_rasters", pattern = "delta", full.names = TRUE)[4])
global(r, fun = c("min","mean","max"), na.rm = TRUE)

# 3) vista rápida
plot(r)

# Ensemble collections + write velocity/accel to derived_rasters ----

# Ensemble collections + write velocity/accel to derived_rasters (USING terra::quantile FOR IQR)
library(terra)
library(dplyr)
library(fs)

# Ajusta esta ruta si tu estructura es distinta
collections_root <- "C:/R/Calotropis_model/Output/cmip6_projected/collections_by_model"

# Output collections
ens_p50_dir <- file.path(collections_root, "wc2_1_2_5m_bioc_ensamble_p50")
ens_iqr_dir <- file.path(collections_root, "wc2_1_2_5m_bioc_ensambleIQR")
dir_create(ens_p50_dir, recurse = TRUE)
dir_create(ens_iqr_dir, recurse = TRUE)

# Derived rasters directory (user requested)
project_root <- dirname(collections_root)   # .../Output/cmip6_projected
derived_out_dir <- file.path(project_root, "derived_rasters")
dir_create(derived_out_dir, recurse = TRUE)

# GDAL / write options
gdal_opts <- c("COMPRESS=LZW", "TILED=YES")
wopt <- list(datatype = "FLT4S", gdal = gdal_opts)

# Utilities
extract_period <- function(fn) {
  p <- regmatches(basename(fn), regexpr("[0-9]{4}_[0-9]{4}", basename(fn)))
  if (length(p) == 0 || p == "")
    return(NA_character_)
  p
}
get_midyear <- function(period_str) {
  yrs <- as.numeric(strsplit(period_str, "_")[[1]])
  mean(yrs, na.rm = TRUE)
}

# Detect periods (exclude any existing ensemble outputs)
all_files <- list.files(
  collections_root,
  pattern = "\\.tif$",
  full.names = TRUE,
  recursive = TRUE
)
all_files <- all_files[!grepl("wc2_1_2_5m_bioc_ensamble_p50|wc2_1_2_5m_bioc_ensambleIQR",
                              all_files)]
periods <- sort(unique(na.omit(sapply(
  all_files, extract_period
))))
if (length(periods) == 0)
  stop("No files with pattern YYYY_YYYY found under: ", collections_root)
message("Periods detected: ", paste(periods, collapse = ", "))

# Pre-build map period -> files (exclude ensemble outputs)
files_by_period <- lapply(periods, function(p) {
  fs <- list.files(
    collections_root,
    pattern = paste0("_", p, "\\.tif$"),
    full.names = TRUE,
    recursive = TRUE
  )
  fs[!grepl("wc2_1_2_5m_bioc_ensamble_p50|wc2_1_2_5m_bioc_ensambleIQR",
            fs)]
})
names(files_by_period) <- periods

# Helper: compute IQR via terra::quantile (returns Q75 - Q25)
compute_iqr_from_stack <- function(s, out_file, wopt) {
  # s: SpatRaster with layers = samples
  # out_file: destination path
  # compute Q25 and Q75 as two-layer raster (streamed)
  tmp_q <- tempfile(fileext = ".tif")
  q <- tryCatch({
    terra::quantile(
      s,
      probs = c(0.25, 0.75),
      na.rm = TRUE,
      filename = tmp_q,
      overwrite = TRUE,
      wopt = wopt
    )
  }, error = function(e) {
    message("  quantile() failed: ", e$message)
    NULL
  })
  if (is.null(q))
    return(FALSE)
  # q has two layers: q25, q75
  # compute difference q75 - q25 and write to out_file
  q75 <- q[[2]]
  q25 <- q[[1]]
  iqr_r <- q75 - q25
  terra::writeRaster(iqr_r, out_file, overwrite = TRUE, wopt = wopt)
  # cleanup tmp
  if (file.exists(tmp_q))
    file.remove(tmp_q)
  return(TRUE)
}

# 1) Per-period: median (p50) using terra::median and IQR using terra::quantile
for (pi in seq_along(periods)) {
  period <- periods[pi]
  message("\n=== Processing period: ",
          period,
          " (",
          pi,
          "/",
          length(periods),
          ") ===")
  files_p <- files_by_period[[period]]
  if (length(files_p) < 1) {
    message("  No model files for ", period)
    next
  }
  
  # read rasters and align to first valid
  ras_list <- lapply(files_p, function(f)
    tryCatch(
      rast(f),
      error = function(e) {
        message("  read error: ", f)
        NULL
      }
    ))
  valid_idx <- which(!sapply(ras_list, is.null))
  ras_list <- ras_list[valid_idx]
  files_p <- files_p[valid_idx]
  if (length(ras_list) == 0) {
    message("  No valid rasters for ", period)
    next
  }
  
  ref <- ras_list[[1]]
  if (is.na(crs(ref)) || crs(ref) == "")
    crs(ref) <- "EPSG:4326"
  
  aligned_paths <- character(0)
  tmp_created <- character(0)
  for (i in seq_along(files_p)) {
    f <- files_p[i]
    r <- ras_list[[i]]
    if (!compareGeom(ref, r, stopOnError = FALSE)) {
      tmp <- tempfile(fileext = ".tif")
      r2 <- resample(r, ref, method = "bilinear")
      terra::writeRaster(r2, tmp, overwrite = TRUE, wopt = wopt)
      aligned_paths <- c(aligned_paths, tmp)
      tmp_created <- c(tmp_created, tmp)
    } else {
      aligned_paths <- c(aligned_paths, f)
    }
  }
  
  # stack aligned rasters
  s <- tryCatch(
    rast(aligned_paths),
    error = function(e) {
      message("  Error stacking rasters: ", e$message)
      NULL
    }
  )
  if (is.null(s)) {
    if (length(tmp_created) > 0)
      file.remove(tmp_created)
    next
  }
  
  # median: use terra::median (native)
  out_med_file <- file.path(ens_p50_dir, paste0("p50_", period, ".tif"))
  message("  Writing median (terra::median) to: ", out_med_file)
  med_r <- tryCatch(
    terra::median(s, na.rm = TRUE),
    error = function(e) {
      message("  median() failed: ", e$message)
      NULL
    }
  )
  if (is.null(med_r)) {
    # fallback: compute median via quantile 0.5
    tmp_q <- tempfile(fileext = ".tif")
    terra::quantile(
      s,
      probs = 0.5,
      na.rm = TRUE,
      filename = tmp_q,
      overwrite = TRUE,
      wopt = wopt
    )
    q50 <- rast(tmp_q)
    terra::writeRaster(q50, out_med_file, overwrite = TRUE, wopt = wopt)
    if (file.exists(tmp_q))
      file.remove(tmp_q)
  } else {
    terra::writeRaster(med_r,
                       out_med_file,
                       overwrite = TRUE,
                       wopt = wopt)
  }
  
  # IQR: compute via quantile (Q75 - Q25)
  out_iqr_file <- file.path(ens_iqr_dir, paste0("IQR_", period, ".tif"))
  message("  Writing IQR (terra::quantile) to: ", out_iqr_file)
  ok <- compute_iqr_from_stack(s, out_iqr_file, wopt)
  if (!ok)
    message("  Warning: IQR computation failed for ", period)
  
  # cleanup temporals
  if (length(tmp_created) > 0)
    file.remove(tmp_created)
  message("  Saved ensemble median and IQR for ", period)
}

# 2) Derivatives: delta/velocity (signed) between consecutive periods and acceleration for triples
for (i in seq_len(length(periods) - 1)) {
  p1 <- periods[i]
  p2 <- periods[i + 1]
  y1 <- get_midyear(p1)
  y2 <- get_midyear(p2)
  dt <- y2 - y1
  if (is.na(dt) ||
      dt == 0) {
    message("Skipping pair with invalid dt: ", p1, " -> ", p2)
    next
  }
  message("\n--- Processing delta/velocity for: ",
          p1,
          " -> ",
          p2,
          " (dt=",
          dt,
          ") ---")
  
  files1 <- files_by_period[[p1]]
  files2 <- files_by_period[[p2]]
  model1 <- sapply(files1, function(f)
    basename(dirname(f)))
  model2 <- sapply(files2, function(f)
    basename(dirname(f)))
  common_models <- intersect(model1, model2)
  if (length(common_models) == 0) {
    message("  No common models for this pair")
    next
  }
  
  delta_tmp <- character(0)
  for (m in common_models) {
    f_curr <- files1[which(model1 == m)][1]
    f_next <- files2[which(model2 == m)][1]
    r_curr <- tryCatch(
      rast(f_curr),
      error = function(e)
        NULL
    )
    r_next <- tryCatch(
      rast(f_next),
      error = function(e)
        NULL
    )
    if (is.null(r_curr) || is.null(r_next))
      next
    if (!compareGeom(r_curr, r_next, stopOnError = FALSE))
      r_next <- resample(r_next, r_curr, method = "bilinear")
    tmpf <- tempfile(fileext = ".tif")
    delta_model <- (r_next - r_curr) / dt
    terra::writeRaster(delta_model, tmpf, overwrite = TRUE, wopt = wopt)
    delta_tmp <- c(delta_tmp, tmpf)
  }
  if (length(delta_tmp) == 0) {
    message("  No valid model deltas for this pair.")
    next
  }
  
  s_delta <- rast(delta_tmp)
  out_delta_med <- file.path(ens_p50_dir,
                             paste0("wc2_1_2_5m_bioc_p50_delta_", p1, "_to_", p2, ".tif"))
  out_delta_iqr <- file.path(ens_iqr_dir,
                             paste0("wc2_1_2_5m_bioc_IQR_delta_", p1, "_to_", p2, ".tif"))
  message("  Writing ensemble delta median to: ", out_delta_med)
  terra::writeRaster(
    terra::median(s_delta, na.rm = TRUE),
    out_delta_med,
    overwrite = TRUE,
    wopt = wopt
  )
  message("  Writing ensemble delta IQR to: ", out_delta_iqr)
  compute_iqr_from_stack(s_delta, out_delta_iqr, wopt)
  
  # write to derived_rasters (delta and velocity names)
  out_delta_med_derived <- file.path(
    derived_out_dir,
    paste0("wc2_1_2_5m_bioc_ensemble_delta_", p1, "_to_", p2, ".tif")
  )
  out_delta_iqr_derived <- file.path(
    derived_out_dir,
    paste0(
      "wc2_1_2_5m_bioc_ensemble_delta_IQR_",
      p1,
      "_to_",
      p2,
      ".tif"
    )
  )
  file.copy(out_delta_med, out_delta_med_derived, overwrite = TRUE)
  file.copy(out_delta_iqr, out_delta_iqr_derived, overwrite = TRUE)
  # velocity (signed delta) copies
  out_vel_med_derived <- file.path(
    derived_out_dir,
    paste0("wc2_1_2_5m_bioc_ensemble_velocity_", p1, "_to_", p2, ".tif")
  )
  out_vel_iqr_derived <- file.path(
    derived_out_dir,
    paste0(
      "wc2_1_2_5m_bioc_ensemble_velocity_IQR_",
      p1,
      "_to_",
      p2,
      ".tif"
    )
  )
  file.copy(out_delta_med, out_vel_med_derived, overwrite = TRUE)
  file.copy(out_delta_iqr, out_vel_iqr_derived, overwrite = TRUE)
  message("  Wrote delta/velocity to derived_rasters: ",
          basename(out_delta_med_derived))
  
  # cleanup temp delta model files
  if (length(delta_tmp) > 0)
    file.remove(delta_tmp)
}

# acceleration: triples (prev, curr, next)
if (length(periods) >= 3) {
  for (i in 2:(length(periods) - 1)) {
    p_prev <- periods[i - 1]
    p_curr <- periods[i]
    p_next <- periods[i + 1]
    y_prev <- get_midyear(p_prev)
    y_curr <- get_midyear(p_curr)
    y_next <- get_midyear(p_next)
    dt1 <- y_curr - y_prev
    dt2 <- y_next - y_curr
    if (any(is.na(c(dt1, dt2))) ||
        dt1 == 0 ||
        dt2 == 0) {
      message("Skipping accel for triple with invalid dt: ",
              p_prev,
              p_curr,
              p_next)
      next
    }
    dt_eff <- mean(c(dt1, dt2))
    message(
      "\n--- Processing acceleration for: ",
      p_prev,
      " , ",
      p_curr,
      " , ",
      p_next,
      " (dt_eff=",
      dt_eff,
      ") ---"
    )
    
    files_prev <- files_by_period[[p_prev]]
    files_curr <- files_by_period[[p_curr]]
    files_next <- files_by_period[[p_next]]
    model_prev <- sapply(files_prev, function(f)
      basename(dirname(f)))
    model_curr <- sapply(files_curr, function(f)
      basename(dirname(f)))
    model_next <- sapply(files_next, function(f)
      basename(dirname(f)))
    common_models <- Reduce(intersect, list(model_prev, model_curr, model_next))
    if (length(common_models) == 0) {
      message("  No common models for triple ", p_prev, p_curr, p_next)
      next
    }
    
    accel_tmp <- character(0)
    for (m in common_models) {
      f_prev <- files_prev[which(model_prev == m)][1]
      f_curr <- files_curr[which(model_curr == m)][1]
      f_next <- files_next[which(model_next == m)][1]
      r_prev <- tryCatch(
        rast(f_prev),
        error = function(e)
          NULL
      )
      r_curr <- tryCatch(
        rast(f_curr),
        error = function(e)
          NULL
      )
      r_next <- tryCatch(
        rast(f_next),
        error = function(e)
          NULL
      )
      if (is.null(r_prev) ||
          is.null(r_curr) || is.null(r_next))
        next
      if (!compareGeom(r_curr, r_prev, stopOnError = FALSE))
        r_prev <- resample(r_prev, r_curr, method = "bilinear")
      if (!compareGeom(r_curr, r_next, stopOnError = FALSE))
        r_next <- resample(r_next, r_curr, method = "bilinear")
      tmpf <- tempfile(fileext = ".tif")
      accel_model <- (r_next - 2 * r_curr + r_prev) / (dt_eff^2)
      terra::writeRaster(accel_model,
                         tmpf,
                         overwrite = TRUE,
                         wopt = wopt)
      accel_tmp <- c(accel_tmp, tmpf)
    }
    if (length(accel_tmp) == 0) {
      message("  No valid model accelerations for this triple.")
      next
    }
    
    s_accel <- rast(accel_tmp)
    out_acc_med <- file.path(
      ens_p50_dir,
      paste0(
        "wc2_1_2_5m_bioc_p50_accel_",
        p_prev,
        "_",
        p_curr,
        "_",
        p_next,
        ".tif"
      )
    )
    out_acc_iqr <- file.path(
      ens_iqr_dir,
      paste0(
        "wc2_1_2_5m_bioc_IQR_accel_",
        p_prev,
        "_",
        p_curr,
        "_",
        p_next,
        ".tif"
      )
    )
    message("  Writing ensemble accel median to: ", out_acc_med)
    terra::writeRaster(
      terra::median(s_accel, na.rm = TRUE),
      out_acc_med,
      overwrite = TRUE,
      wopt = wopt
    )
    message("  Writing ensemble accel IQR to: ", out_acc_iqr)
    compute_iqr_from_stack(s_accel, out_acc_iqr, wopt)
    
    # write to derived_rasters as well
    out_acc_med_derived <- file.path(
      derived_out_dir,
      paste0(
        "wc2_1_2_5m_bioc_ensemble_accel_",
        p_prev,
        "_",
        p_curr,
        "_",
        p_next,
        ".tif"
      )
    )
    out_acc_iqr_derived <- file.path(
      derived_out_dir,
      paste0(
        "wc2_1_2_5m_bioc_ensemble_accel_IQR_",
        p_prev,
        "_",
        p_curr,
        "_",
        p_next,
        ".tif"
      )
    )
    file.copy(out_acc_med, out_acc_med_derived, overwrite = TRUE)
    file.copy(out_acc_iqr, out_acc_iqr_derived, overwrite = TRUE)
    message("  Wrote accel to derived_rasters: ",
            basename(out_acc_med_derived))
    
    # cleanup temp accel model files
    if (length(accel_tmp) > 0)
      file.remove(accel_tmp)
  }
} else {
  message("Not enough periods to compute acceleration (need >= 3 periods).")
}

message("\nAll ensemble collections written to:")
message("  Medians (p50): ", ens_p50_dir)
message("  IQRs:          ", ens_iqr_dir)
message("  Derived rasters (delta/velocity/accel): ", derived_out_dir)


# # Visualización por modelo: láminas con paneles (predicciones, delta, vel, accel) ----


# Final visualization block adapted: velocity = signed delta; time-series with 95% CI
library(terra)
library(raster)
library(ggplot2)
library(viridis)
library(dplyr)
library(patchwork)
library(fs)
library(scales)

# Paths (adjust if needed)
collections_dir <- file.path("Output", "cmip6_projected", "collections_by_model")
derived_dir     <- file.path("Output", "cmip6_projected", "derived_rasters")
fig_dir         <- file.path("Output", "cmip6_projected", "figures")
dir_create(fig_dir, recurse = TRUE)

# Helpers
reduce_for_plot <- function(r, maxdim = 1200) {
  if (!inherits(r, "SpatRaster"))
    r <- rast(r)
  d <- dim(r)[1:2]
  if (max(d) > maxdim) {
    fact <- ceiling(max(d) / maxdim)
    return(aggregate(
      r,
      fact = fact,
      fun = mean,
      na.rm = TRUE
    ))
  }
  return(r)
}

raster_to_df <- function(r) {
  r_small <- reduce_for_plot(r)
  df <- as.data.frame(r_small, xy = TRUE)
  if (ncol(df) < 3)
    colnames(df) <- c("x", "y", "value")
  else
    colnames(df)[3] <- "value"
  df$value[is.infinite(df$value)] <- NA
  df
}

safe_sample_vals <- function(r, max_n = 20000) {
  v <- tryCatch({
    if (inherits(r, "SpatRaster")) {
      ncell_r <- terra::ncell(r)
      n <- min(max_n, max(1, ncell_r))
      samp <- tryCatch(
        terra::spatSample(
          r,
          size = n,
          method = "random",
          na.rm = TRUE
        ),
        error = function(e)
          NULL
      )
      if (!is.null(samp)) {
        if (is.matrix(samp))
          samp <- samp[, 1]
        samp <- samp[!is.na(samp)]
        return(as.numeric(samp))
      }
    }
    vals <- terra::values(r)
    if (is.matrix(vals))
      vals <- vals[, 1]
    vals <- vals[!is.na(vals)]
    if (length(vals) == 0)
      return(numeric(0))
    n <- min(max_n, length(vals))
    if (length(vals) > n)
      sample(vals, size = n)
    else
      vals
  }, error = function(e) {
    message("  warning: could not sample values: ", e$message)
    numeric(0)
  })
  return(v)
}

# Divergent map builder for signed fields (velocity/delta, accel)
make_map_divergent <- function(r,
                               title = "",
                               vlim = NULL,
                               palette_low = "#2b83ba",
                               palette_high = "#d7191c") {
  df <- raster_to_df(r)
  if (nrow(df) == 0)
    return(ggplot() + geom_blank() + labs(title = paste0(title, " (empty)")) + theme_minimal())
  if (is.null(vlim)) {
    # use symmetric quantiles around 0 based on absolute values to avoid palette skew
    vals <- df$value[is.finite(df$value)]
    if (length(vals) == 0)
      vlim <- c(-1e-6, 1e-6)
    else {
      q <- quantile(abs(vals),
                    probs = c(0.01, 0.99),
                    na.rm = TRUE)
      m <- max(q, na.rm = TRUE)
      vlim <- c(-m, m)
    }
  }
  ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_gradient2(
      low = palette_low,
      mid = "white",
      high = palette_high,
      midpoint = 0,
      limits = vlim,
      oob = squish,
      na.value = "grey95",
      guide = guide_colorbar(
        barwidth = 0.5,
        barheight = 5,
        title.position = "top"
      )
    ) +
    coord_quickmap() +
    labs(title = title, fill = "") +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(
        size = 11,
        face = "bold",
        hjust = 0.5
      ),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
}

# Sequential map builder for unsigned fields (predictions, IQR, etc.)
make_map_continuous <- function(r,
                                title = "",
                                vmin = NULL,
                                vmax = NULL,
                                palette = "C") {
  df <- raster_to_df(r)
  if (nrow(df) == 0)
    return(ggplot() + geom_blank() + labs(title = paste0(title, " (empty)")) + theme_minimal())
  if (is.null(vmin))
    vmin <- min(df$value, na.rm = TRUE)
  if (is.null(vmax))
    vmax <- max(df$value, na.rm = TRUE)
  if (!is.finite(vmin) || !is.finite(vmax) || vmin == vmax) {
    vmin <- min(df$value, na.rm = TRUE)
    vmax <- max(df$value, na.rm = TRUE)
    if (vmin == vmax) {
      vmin <- vmin - 1e-6
      vmax <- vmax + 1e-6
    }
  }
  ggplot(df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_viridis_c(
      option = palette,
      na.value = "grey95",
      limits = c(vmin, vmax),
      oob = squish,
      guide = guide_colorbar(
        barwidth = 0.5,
        barheight = 5,
        title.position = "top"
      )
    ) +
    coord_quickmap() +
    labs(title = title, fill = "") +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(
        size = 11,
        face = "bold",
        hjust = 0.5
      ),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "right",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
}

# compute per-period mean across models and 95% CI for the series
compute_period_means_with_ci <- function(pred_files) {
  rows <- list()
  for (f in pred_files) {
    # f is a model file for a given period; we need to compute spatial mean per model file
    r <- tryCatch(
      rast(f),
      error = function(e)
        NULL
    )
    if (is.null(r))
      next
    m <- tryCatch(
      global(r, fun = "mean", na.rm = TRUE)[1, 1],
      error = function(e)
        NA_real_
    )
    period <- regmatches(basename(f),
                         regexpr("[0-9]{4}_[0-9]{4}|[0-9]{4}-[0-9]{4}", basename(f)))
    if (length(period) == 0 ||
        period == "")
      period <- tools::file_path_sans_ext(basename(f))
    rows[[length(rows) + 1]] <- data.frame(
      file = f,
      period = gsub("-", "_", period),
      mean_model = m,
      stringsAsFactors = FALSE
    )
  }
  if (length(rows) == 0)
    return(
      data.frame(
        period = character(0),
        mean = numeric(0),
        ci_low = numeric(0),
        ci_high = numeric(0),
        year = numeric(0)
      )
    )
  df <- bind_rows(rows)
  # group by period: compute mean across models and 95% CI (treat models as independent samples)
  summary_df <- df %>%
    group_by(period) %>%
    summarize(
      n_models = sum(!is.na(mean_model)),
      mean = ifelse(n_models > 0, mean(mean_model, na.rm = TRUE), NA_real_),
      sd = ifelse(n_models > 1, sd(mean_model, na.rm = TRUE), NA_real_),
      year = as.numeric(gsub(".*_([0-9]{4})$", "\\1", first(period))),
      .groups = "drop"
    ) %>%
    mutate(
      se = ifelse(n_models > 1, sd / sqrt(n_models), NA_real_),
      ci_low = mean - 1.96 * se,
      ci_high = mean + 1.96 * se
    )
  # if only one model, set CI to NA
  summary_df$ci_low[summary_df$n_models <= 1] <- NA
  summary_df$ci_high[summary_df$n_models <= 1] <- NA
  summary_df <- arrange(summary_df, year)
  return(summary_df)
}

# Main loop: one legend per row, portrait canvas; velocity = signed delta
models <- list.dirs(collections_dir, full.names = FALSE, recursive = FALSE)
if (length(models) == 0)
  stop("No collections found in: ", collections_dir)

for (m in models) {
  message("Generating sheet for: ", m)
  col_dir <- file.path(collections_dir, m)
  pred_files <- list.files(col_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(pred_files) == 0) {
    message("  No predictions in: ", col_dir)
    next
  }
  
  extract_period <- function(fn) {
    p <- regmatches(basename(fn), regexpr("[0-9]{4}_[0-9]{4}", basename(fn)))
    if (length(p) == 0 ||
        p == "")
      p <- regmatches(basename(fn), regexpr("[0-9]{4}-[0-9]{4}", basename(fn)))
    if (length(p) == 0 || p == "") {
      yrs <- regmatches(basename(fn), gregexpr("[0-9]{4}", basename(fn)))[[1]]
      if (length(yrs) >= 2)
        p <- paste0(yrs[1], "_", yrs[2])
      else if (length(yrs) == 1)
        p <- paste0(yrs[1], "_", yrs[1])
      else
        p <- basename(fn)
    }
    gsub("-", "_", p)
  }
  periods <- sapply(pred_files, extract_period)
  midyears <- sapply(periods, function(p)
    mean(as.numeric(strsplit(p, "_")[[1]])))
  ord <- order(midyears, na.last = TRUE)
  pred_files <- pred_files[ord]
  periods <- periods[ord]
  midyears <- midyears[ord]
  
  preds <- lapply(pred_files, function(f)
    tryCatch(
      rast(f),
      error = function(e)
        NULL
    ))
  preds <- preds[!sapply(preds, is.null)]
  if (length(preds) == 0) {
    message("  Could not read predictions for: ", m)
    next
  }
  
  safe_m <- gsub("[^A-Za-z0-9_\\-]", "_", m)
  # delta files are used as velocity (signed)
  delta_files <- list.files(
    derived_dir,
    pattern = paste0("^", safe_m, ".*_delta_.*\\.tif$"),
    full.names = TRUE
  )
  accel_files <- list.files(
    derived_dir,
    pattern = paste0("^", safe_m, ".*_accel_.*\\.tif$"),
    full.names = TRUE
  )
  
  # Scales: sample safely
  all_pred_vals <- c()
  for (r in preds) {
    v <- safe_sample_vals(r, max_n = 20000)
    if (length(v) > 0)
      all_pred_vals <- c(all_pred_vals, v)
  }
  if (length(all_pred_vals) > 0) {
    pred_q <- quantile(all_pred_vals,
                       probs = c(0.025, 0.975),
                       na.rm = TRUE)
    pred_vmin <- pred_q[1]
    pred_vmax <- pred_q[2]
  } else {
    pred_vmin <- NA
    pred_vmax <- NA
  }
  
  all_delta_vals <- c()
  if (length(delta_files) > 0)
    for (f in delta_files) {
      r <- tryCatch(
        rast(f),
        error = function(e)
          NULL
      )
      if (!is.null(r)) {
        v <- safe_sample_vals(r, 20000)
        if (length(v) > 0)
          all_delta_vals <- c(all_delta_vals, v)
      }
    }
  if (length(all_delta_vals) > 0) {
    dq <- quantile(all_delta_vals,
                   probs = c(0.01, 0.99),
                   na.rm = TRUE)
    delta_vlim <- c(-max(abs(dq)), max(abs(dq)))
  } else {
    delta_vlim <- NULL
  }
  
  all_acc_vals <- c()
  if (length(accel_files) > 0)
    for (f in accel_files) {
      r <- tryCatch(
        rast(f),
        error = function(e)
          NULL
      )
      if (!is.null(r)) {
        v <- safe_sample_vals(r, 20000)
        if (length(v) > 0)
          all_acc_vals <- c(all_acc_vals, v)
      }
    }
  if (length(all_acc_vals) > 0) {
    aq <- quantile(all_acc_vals,
                   probs = c(0.01, 0.99),
                   na.rm = TRUE)
    acc_vlim <- c(-max(abs(aq)), max(abs(aq)))
  } else {
    acc_vlim <- NULL
  }
  
  # Build rows with one legend per row (collect guides per row)
  # Predictions row (continuous)
  pred_panels <- list()
  for (i in seq_along(preds)) {
    title <- paste0("S[n]: ", periods[i])
    pred_panels[[i]] <- make_map_continuous(preds[[i]],
                                            title = title,
                                            vmin = pred_vmin,
                                            vmax = pred_vmax)
  }
  pred_row <- (wrap_plots(pred_panels, nrow = 1) + plot_layout(guides = "collect")) &
    theme(legend.position = "right")
  
  # Velocity row (use delta files, signed)
  delta_row <- NULL
  if (length(delta_files) > 0) {
    extract_mid <- function(fn) {
      p <- regmatches(basename(fn), regexpr("[0-9]{4}_[0-9]{4}", basename(fn)))
      if (length(p) == 0 ||
          p == "")
        return(NA_real_)
      yrs <- as.numeric(strsplit(p, "_")[[1]])
      mean(yrs)
    }
    delta_files <- delta_files[order(sapply(delta_files, extract_mid), na.last = TRUE)]
    delta_panels <- list()
    for (f in delta_files) {
      r <- tryCatch(
        rast(f),
        error = function(e)
          NULL
      )
      if (is.null(r))
        next
      title <- paste0("V[n]: ",
                      regmatches(basename(f), regexpr("[0-9]{4}_[0-9]{4}", basename(f))))
      delta_panels[[length(delta_panels) + 1]] <- make_map_divergent(r, title = title, vlim = delta_vlim)
    }
    if (length(delta_panels) > 0)
      delta_row <- (wrap_plots(delta_panels, nrow = 1) + plot_layout(guides = "collect")) &
      theme(legend.position = "right")
  }
  
  # Acceleration row (central/backward depending on files available)
  accel_row <- NULL
  if (length(accel_files) > 0) {
    accel_files <- accel_files[order(sapply(accel_files, function(fn) {
      p <- regmatches(basename(fn),
                      regexpr("[0-9]{4}_[0-9]{4}_[0-9]{4}", basename(fn)))
      if (length(p) == 0)
        NA_character_
      else
        p
    }), na.last = TRUE)]
    accel_panels <- list()
    for (f in accel_files) {
      r <- tryCatch(
        rast(f),
        error = function(e)
          NULL
      )
      if (is.null(r))
        next
      title <- paste0("A[n]: ",
                      regmatches(
                        basename(f),
                        regexpr("[0-9]{4}_[0-9]{4}_[0-9]{4}", basename(f))
                      ))
      accel_panels[[length(accel_panels) + 1]] <- make_map_divergent(r, title = title, vlim = acc_vlim)
    }
    if (length(accel_panels) > 0)
      accel_row <- (wrap_plots(accel_panels, nrow = 1) + plot_layout(guides = "collect")) &
      theme(legend.position = "right")
  }
  
  # Time series row with 95% CI across models
  # For the series we need per-model spatial means grouped by period: find model files for this collection and compute
  # pred_files contains all model rasters for this model collection; compute summary across them
  means_summary <- compute_period_means_with_ci(pred_files)
  if (nrow(means_summary) > 0) {
    p_series <- ggplot(means_summary, aes(x = year, y = mean)) +
      geom_ribbon(
        aes(ymin = ci_low, ymax = ci_high),
        fill = "#9ecae1",
        alpha = 0.4,
        na.rm = TRUE
      ) +
      geom_line(color = "#2c7bb6") + geom_point(color = "#2c7bb6") + theme_minimal() +
      labs(title = "Mean suitability by period", x = "Mid-year", y = "Mean suitability") +
      theme(plot.title = element_text(
        size = 11,
        face = "bold",
        hjust = 0.5
      ))
  } else {
    p_series <- ggplot() + geom_blank() + labs(title = "No means available")
  }
  
  # Assemble rows (only include non-null rows)
  rows <- list(pred_row)
  if (!is.null(delta_row))
    rows <- c(rows, list(delta_row))
  if (!is.null(accel_row))
    rows <- c(rows, list(accel_row))
  # combine and add series at bottom
  main_block <- wrap_plots(rows, ncol = 1)
  final_plot <- (main_block / p_series) + plot_layout(heights = c(rep(1, length(rows)), 0.9))
  
  # Save portrait outputs
  out_base <- file.path(fig_dir, paste0("fig_", safe_m))
  tryCatch({
    ggsave(
      paste0(out_base, ".pdf"),
      final_plot,
      device = cairo_pdf,
      width = 20,
      height = 30,
      units = "cm"
    )
  }, error = function(e)
    message("  Error saving PDF: ", e$message))
  if (requireNamespace("ragg", quietly = TRUE)) {
    tryCatch({
      ragg::agg_png(
        paste0(out_base, ".png"),
        width = 20,
        height = 30,
        units = "cm",
        res = 300
      )
      print(final_plot)
      dev.off()
    }, error = function(e)
      message("  Error saving PNG (ragg): ", e$message))
  } else {
    tryCatch({
      ggsave(
        paste0(out_base, ".png"),
        final_plot,
        width = 20,
        height = 30,
        units = "cm",
        dpi = 300
      )
    }, error = function(e)
      message("  Error saving PNG: ", e$message))
  }
  tryCatch({
    ggsave(
      paste0(out_base, ".svg"),
      final_plot,
      width = 20,
      height = 30,
      units = "cm",
      device = "svg"
    )
  }, error = function(e)
    message("  Error saving SVG: ", e$message))
  
  message("Sheet saved: ", out_base, ".{pdf,png,svg}")
}



