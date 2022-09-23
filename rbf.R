library(getopt)
spec <- matrix(c(
  'help', 'h', 0, "logical",
  'input', 'i', 1, "character",
  'output', 'o', 1, "character",
  'layer', 'l', 2, "character",
  'step', 's', 2, "integer",
  'xmin', 'x', 2, "integer",
  'xmax', 'X', 2, "integer",
  'ymin', 'y', 2, "integer",
  'ymax', 'Y', 2, "integer"
), byrow=TRUE, ncol=4)

opt <- getopt(spec)

# return help if asked
if ( !is.null(opt$help) ) {
  cat(getopt(spec, usage=TRUE))
  q(status=1)
}

# set defaults
if ( is.null(opt$step) ) { opt$step = 250}
if ( is.null(opt$layer) ) {
  name <- rev(strsplit(opt$input, '\\', fixed=TRUE)[[1]])[1]
  opt$layer <- substr(name, 1, nchar(name) - nchar(".shp"))
}
print(opt)
#input_file = "C://temp//fnjwsqjzwa//HUMIDITY//eotanqsgrl//data_10_10_2021_00_00.shp"
#layer = "data_10_10_2021_00_00"
#step = 250
#xmin_ <- 5022899
#ymin_ <- 5005632
#xmax_ <- 6442384
#ymax_ <- 6477995

# print(args)

library(sf)
library(stars)
library(tmap)
library(raster)
library(plotly)
library(mapview)
library(tidyverse)
library(ggrepel)
#library(dismo) # библиотека species distribution modelling
library(akima) # библиотека для интерполяции на основе триангуляции
library(gstat) # библиотека для геостатистической интерполяции, построения трендов и IDW
library(deldir) # библиотека для построения триангуляции Делоне и диаграммы Вороного
library(fields) # радиальные базисные функции (сплайны минимальной кривизны)

# Убираем экспоненциальное представление больших чисел
options(scipen=999)
# Читаем слои картографической основы

humidity = st_read(opt$input, opt$layer) %>% # Города
  bind_cols(st_coordinates(.) %>% as_tibble())
humidity = drop_na(humidity)
humidity[140, "HUMIDITY"] <- NaN
humidity = drop_na(humidity)
# Триангуляция Делоне
edges = humidity %>%
  st_union() %>%
  st_triangulate()
# Координаты пригодятся нам в дальнейшем
coords = st_coordinates(humidity)
# ПОСТРОЕНИЕ СЕТКИ ДЛЯ ИНТЕРПОЛЯЦИИ
# получим ограничивающий прямоугольник вокруг точек:


box = st_bbox(humidity)
if ( !is.null(opt$xmin) ) {box[1] <- opt$xmin}
if ( !is.null(opt$xmax) ) {box[3] <- opt$xmax}
if ( !is.null(opt$ymin) ) {box[2] <- opt$ymin}
if ( !is.null(opt$ymax) ) {box[4] <- opt$ymax}

#envelope = box[c(1,3,2,4)]
px_grid = st_as_stars(box, dx = opt$step, dy = opt$step)
ggplot() +
  geom_sf(data = humidity, color = 'red') +
  geom_sf(data = st_as_sf(px_grid), size = 0.5, fill = NA)
# создадим детальную растровую сетку
#px_grid = st_as_stars(box, dx = 250, dy = 250)
# извлечем координаты точек в соответствующие столбцы, они нам пригодятся:
coords_grid = st_coordinates(px_grid)
# Цветовая шкала для осадков
rain_colors = colorRampPalette(c("white", "dodgerblue", "dodgerblue4"))
# Шкала количества осадков и соответствющее число цветов
#rain_levels = seq(50, 120, by=5)
#rain_ncolors = length(rain_levels)-1
# rain_legend = scale_fill_manual(name = '%',
#                                 values = rain_colors(rain_ncolors),
#                                 guide = guide_legend(label.vjust = -0.3, reverse = TRUE, title.position = "bottom"),
#                                 labels = rain_levels,
#                                 na.value = 'white',
#                                 drop = FALSE)
rain_mapping = aes(fill = cut(humidity.HUMIDITY, breaks = rain_levels))
# px_grid = px_grid %>%
#   mutate(z_linear = interpp(x = coords[,1],
#                             y = coords[,2],
#                             z = humidity$HUMIDITY,
#                             xo = coords_grid[,1],
#                             yo = coords_grid[,2],
#                             linear = TRUE)$z)
#
# cont_linear = st_contour(px_grid['z_linear'], breaks = rain_levels, contour_lines = TRUE)
# # Смотрим как выглядит результат
#
# ggplot() +
#   geom_stars(data = cut(px_grid['z_linear'], breaks = rain_levels)) +
#   rain_legend +
#   coord_sf(crs = st_crs(humidity)) +
#   geom_sf(data = cont_linear, color = 'black', size = 0.2) +
#   geom_sf(data = humidity, color = 'red', size = 0.5) +
#   geom_sf(data = edges, color = 'red', size = 0.1, fill = NA)
# РАДИАЛЬНЫЕ БАЗИСНЫЕ ФУНКЦИИ (RADIAL BASIS FUNCTIONS)
pred = Tps(coords, humidity$HUMIDITY, scale.type = 'unscaled')
# После этого можно интерполировать значения с помощью функции predict():
px_grid = px_grid %>%
  mutate(z_tps = predict(pred, coords_grid))
# Придется расширить шкалу, так как сплайновая поверхность выходит за пределы исходных значений:
tps_breaks = seq(50,110,by=5)
tps_ncolors = length(tps_breaks) - 1
cont_tps = st_contour(px_grid['z_tps'],
                      breaks = tps_breaks,
                      contour_lines = TRUE)

tiff(opt$output, compression = "zip", width = 480, height = 480)
# Виузализируем результат:
ggplot() +

  geom_stars(data = cut(px_grid['z_tps'], breaks = tps_breaks)) +
  scale_fill_manual(name = 'мм',
                   values = rain_colors(tps_ncolors),
                   labels = paste(tps_breaks[-tps_ncolors-1], '-', tps_breaks[-1])) +
  theme(legend.position = "none") +
  coord_sf(crs = st_crs(humidity)) +
  geom_sf(data = cont_tps, color = 'black', size = 0.2) +
  geom_sf(data = humidity, color = 'red', size = 0.5)
dev.off()
#rain_colors3d = colorRamp(c("white", "dodgerblue", "dodgerblue4"))
#x = coords_grid[,'x'] %>% unique() # Получим координаты столбцов
#y = coords_grid[,'y'] %>% unique() # Получим координаты строк

# p = plot_ly(x = x,
#             y = y,
#             z = px_grid$z_tps,
#             type = "surface",
#             colors = rain_colors3d)
# layout(p, scene = list(aspectratio =
#                          list(x = 1, y = 1, z = 0.3)
# ))

