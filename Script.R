# The following packages are necessary: rayshader, raster, osmdata, sf, dplyr, 
#ggplot2, RColorBrewer, paletteer, pacman

install.packages("devtools")
devtools::install_github("tylermorganwall/rayshader")
devtools::install_github ("ropensci/osmdata")
devtools::install_github("r-spatial/sf")
install.packages("paletteer")

#Load packages
pacman::p_load(rayshader, raster, osmdata, sf, dplyr, ggplot2, RColorBrewer, 
               paletteer)

#Load the digital levation model
localtif = raster::raster("Elevacion_cuenca.tif")

#Prepare the elevation matrix
elmat = raster_to_matrix(localtif)

#Resize the elevation matrix if necessary (e.g. memory issues)
elmat_small = resize_matrix(elmat, scale = 0.5)

#Load de shapefile
shapefile = st_read(dsn = "shapefile/biodiversity_measures_Mira_Mataje.shp")

#Generate the shaded-relief map
base_map = elmat %>% 
    height_shade() %>%
    add_overlay(sphere_shade(elmat, texture = "desert", colorintensity = 5), alphalayer=0.5) %>%
    add_shadow(lamb_shade(elmat), 0) %>%
    add_shadow(ambient_shade(elmat),0) %>% 
    add_shadow(texture_shade(elmat,detail=8/10,contrast=9,brightness = 11), 0.1)

plot_map(base_map)

#Testing if the shapefile is valid
st_is_valid(shapefile)

#Making the shapefile valid
shapefile_valid = st_make_valid(shapefile)

shapefile_valid$FF_ende = as.numeric(as.character(shapefile_valid$FF_ende))

#Confirm the shapefile is now valid
st_is_valid(shapefile_valid)

#Generate overlay
base_map %>% 
    add_overlay(generate_polygon_overlay(shapefile_valid, palette = rainbow, extent = attr(localtif, "extent"), heightmap = elmat)) %>% 
    plot_map()

#~~~~~~~Starting here, the maps for the diversity data are generated~~~~~~####

#Ecosystems map####

county_palette = c("1" = "darkgoldenrod1", "2" = "brown2", "3" = "darkorchid3", "4" = "darkseagreen1", "5" = "pink", "6" = "darkgoldenrod4", 
                   "7" = "darkolivegreen4", "8" = "darkkhaki", "9" = "darkgoldenrod", "10" = "cyan3", "11" = "#B1F04B", "12" = "chartreuse", 
                   "13" = "yellow", "14" = "darkcyan", "15" = "darkslategray1")

#THIS IS THE PLOT RENDERED    
base_map %>% 
  add_overlay(generate_polygon_overlay(shapefile_valid, palette = county_palette, linecolor="white", linewidth=0, 
                                       extent = attr(localtif, "extent"), heightmap = elmat_small), alphalayer = 0.7) %>% 
                                      plot_3d(elmat_small, zscale = 150, windowsize=c(1600,900), solid = TRUE, soil = TRUE, 
                                      background = "black", shadowdepth = -7000,)

render_scalebar(limits=c(0, 80), label_unit = "km", radius = 10, color_text ="white", y = -10, offset = 10, clear_scalebar = FALSE)
render_compass(compass_radius = 100, position_circular = FALSE, clear_compass = FALSE, altitude = 10, scale_distance = 0.7)

#Renderig the 3D shaded-relief map in an interactive HTML file
r <- rgl::rglwidget(width = 1100, height = 900)
r
widget_fn <- "ecosystems_final.html"
htmlwidgets::saveWidget(r, widget_fn, background = "black")

rgl::clear3d()
rgl::rgl.close()

#Number_of_records map####

color_sequence = c("0" = "#335390", "1" = "#5e6090", "277" = "#787690", "745" = "#908a94", "780" = "#a7a292", "878" = "#c4be8c", 
                   "1271" = "#dddc89", "1552" = "#faf983", "1665" = "#fbe578", "1804" = "#fccb6f", "1875" = "#fdb266", "4369" = "#f99859", 
                   "16472" = "#f98153", "25691" = "#f3694a", "190969" = "#ee4443")

#THIS IS THE PLOT RENDERED    
base_map %>% 
  add_overlay(generate_polygon_overlay(shapefile_valid, palette = color_sequence, linecolor="white", linewidth=0, 
                                       extent = attr(localtif, "extent"), heightmap = elmat_small, data_column_fill = "reg_total"), alphalayer = 0.7) %>% 
  plot_3d(elmat_small, zscale = 150, windowsize=c(1600,900), solid = TRUE, soil = TRUE, 
          background = "black", shadowdepth = -7000)

render_scalebar(limits=c(0, 80), label_unit = "km", radius = 10, color_text ="white", y = -10, offset = 10, clear_scalebar = FALSE)
render_compass(compass_radius = 100, position_circular = FALSE, clear_compass = FALSE, altitude = 10, scale_distance = 0.7)

#Renderig the 3D shaded-relief map in an interactive HTML file
r <- rgl::rglwidget(width = 1100, height = 900)
r
widget_fn <- "number_of_records.html"
htmlwidgets::saveWidget(r, widget_fn, background = "black")


rgl::clear3d()
rgl::rgl.close()

#Number_of_species map ####

color_sequence = c("0" = "#335390", "1" = "#5e6090", "189" = "#787690", "211" = "#908a94", "266" = "#a7a292", "432" = "#c4be8c", 
                   "473" = "#dddc89", "476" = "#faf983", "493" = "#fbe578", "581" = "#fccb6f", "798" = "#fdb266", "990" = "#f99859", 
                   "2911" = "#f98153", "2917" = "#f3694a", "4095" = "#ee4443")

#THIS IS THE PLOT RENDERED    
base_map %>% 
  add_overlay(generate_polygon_overlay(shapefile_valid, palette = color_sequence, linecolor="white", linewidth=0, 
                                       extent = attr(localtif, "extent"), heightmap = elmat_small, data_column_fill = "esp_total"), alphalayer = 0.7) %>% 
  plot_3d(elmat_small, zscale = 150, windowsize=c(1600,900), solid = TRUE, soil = TRUE, 
          background = "black", shadowdepth = -7000)

render_scalebar(limits=c(0, 80), label_unit = "km", radius = 10, color_text ="white", y = -10, offset = 10, clear_scalebar = FALSE)
render_compass(compass_radius = 100, position_circular = FALSE, clear_compass = FALSE, altitude = 10, scale_distance = 0.7)

#Renderig the 3D shaded-relief map in an interactive HTML file
r <- rgl::rglwidget(width = 1100, height = 900)
r
widget_fn <- "number_of_species.html"
htmlwidgets::saveWidget(r, widget_fn, background = "black")


rgl::clear3d()
rgl::rgl.close()

#Number_of_endemics map####

color_sequence = c("0" = "#335390", "0" = "#5e6090", "4" = "#787690", "7" = "#908a94", "13" = "#a7a292", "35" = "#c4be8c", 
                   "39" = "#dddc89", "44" = "#faf983", "49" = "#fbe578", "53" = "#fccb6f", "57" = "#fdb266", "103" = "#f99859", 
                   "307" = "#f98153", "321" = "#f3694a", "423" = "#ee4443")

#THIS IS THE PLOT RENDERED    
base_map %>% 
  add_overlay(generate_polygon_overlay(shapefile_valid, palette = color_sequence, linecolor="white", linewidth=0, 
                                       extent = attr(localtif, "extent"), heightmap = elmat_small, data_column_fill = "FF_ende"), alphalayer = 0.7) %>% 
  plot_3d(elmat_small, zscale = 150, windowsize=c(1600,900), solid = TRUE, soil = TRUE, 
          background = "black", shadowdepth = -7000)

render_scalebar(limits=c(0, 80), label_unit = "km", radius = 10, color_text ="white", y = -10, offset = 10, clear_scalebar = FALSE)
render_compass(compass_radius = 100, position_circular = FALSE, clear_compass = FALSE, altitude = 10, scale_distance = 0.7)

#Renderig the 3D shaded-relief map in an interactive HTML file
r <- rgl::rglwidget(width = 1100, height = 900)
r
widget_fn <- "number_of_endemic_species.html"
htmlwidgets::saveWidget(r, widget_fn, background = "black")


rgl::clear3d()
rgl::rgl.close()

#Number_of_threatened_species map####

color_sequence = c("0" = "#335390", "0" = "#5e6090", "7" = "#787690", "13" = "#908a94", "21" = "#a7a292", "27" = "#c4be8c", 
                   "39" = "#dddc89", "48" = "#faf983", "52" = "#fbe578", "58" = "#fccb6f", "66" = "#fdb266", "114" = "#f99859", 
                   "482" = "#f98153", "889" = "#f3694a", "1175" = "#ee4443")

#THIS IS THE PLOT RENDERED    
base_map %>% 
  add_overlay(generate_polygon_overlay(shapefile_valid, palette = color_sequence, linecolor="white", linewidth=0, 
                                       extent = attr(localtif, "extent"), heightmap = elmat_small, data_column_fill = "FF_ame"), alphalayer = 0.7) %>% 
  plot_3d(elmat_small, zscale = 150, windowsize=c(1600,900), solid = TRUE, soil = TRUE, 
          background = "black", shadowdepth = -7000)

render_scalebar(limits=c(0, 80), label_unit = "km", radius = 10, color_text ="white", y = -10, offset = 10, clear_scalebar = FALSE)
render_compass(compass_radius = 100, position_circular = FALSE, clear_compass = FALSE, altitude = 10, scale_distance = 0.7)

#Renderig the 3D shaded-relief map in an interactive HTML file
r <- rgl::rglwidget(width = 1100, height = 900)
r
widget_fn <- "number_of_threatened_species.html"
htmlwidgets::saveWidget(r, widget_fn, background = "black")


rgl::clear3d()
rgl::rgl.close()

#Making vectors in case they are needed
vector_EcosisI= mantel_valid$EcosisI
vector_reg_total = mantel_valid$reg_total
vector_esp_total = mantel_valid$esp_total
vector_FF_ende = mantel_valid$FF_ende
vector_FF_ame = mantel_valid$FF_ame

#setting up a control table
data = data.frame(vector_EcosisI, vector_reg_total, vector_esp_total, vector_FF_ende, vector_FF_ame)

#end of script

