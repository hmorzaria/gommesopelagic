#' @title Mapping Atlantis polygons
#' @description  Functions to plot shapefile
#' @details INPUT: 1) An Atlantis model polygon shape file
#' @details OUTPUT: 1) Map of Atlantis polygons
#' @details Used code from https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
#' @author Hem Nalini Morzaria-Luna, hmorzarialuna@gmail.com



make_map <- function(shape.file, file.name,scale.factor, bar.position) {
  

  model.shape <- readOGR(shape.file)
  
  # Reformat shape for mapping purposes
  model.shape.df <- broom::tidy(model.shape)
  
  #tm_shape(world) + tm_polygons()
  #tm_basemap(server = "OpenTopoMap")
  
  world <- ne_countries(scale = "large", returnclass = "sf")
  class(world)
  
  world_points<- st_centroid(world)
  world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))
  
  min.long <- min(model.shape.df$long)-scale.factor
  max.long <- max(model.shape.df$long)+scale.factor
  min.lat <- min(model.shape.df$lat)-scale.factor
  max.lat <- max(model.shape.df$lat)+scale.factor
  
  
  model.map <- ggplot(data = world) +
    geom_sf() +
    annotation_scale(location = bar.position, width_hint = 0.5) +
    annotation_north_arrow(location = "br", which_north = "true", 
                           pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                           style = north_arrow_fancy_orienteering) +
    #geom_text(data= world_points,aes(x=X, y=Y, label=name),
    #          color = "darkgrey", check_overlap = FALSE) +
    coord_sf(xlim = c(min.long, max.long), ylim = c(min.lat, max.lat), expand = FALSE)+
    geom_path(data = model.shape.df, aes(x = long, y = lat, group=group),
              colour = "gray45", size=0.5) +
    xlab("Lon")+
    ylab("Lat")+
    theme_bw()
  
  
  ggsave(file.name, model.map, scale = 2, dpi = 400)
  
  return(model.map)
  
}

