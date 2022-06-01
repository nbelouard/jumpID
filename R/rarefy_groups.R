#'Rarefy the jump dataset
#'
#'Build the rarefied dataset by keeping only the centroid of each cluster of jumps
#'
#'@export
#'
#'@param Jump_groups A dataset to be processed
#'
#'@return A table containing the rarefied dataset
#' 
#'@examples
#' new_dataset <- rarefy_groups(dataset)
#' 

rarefy_groups <- function(Jump_groups) {
  
  # Create a dataset with centroids for groups
  Jumpers_centroids <- Jump_groups %>% group_by(bio_year, Group) %>% 
    summarise(latitude_rounded = mean(latitude_rounded), longitude_rounded = mean(longitude_rounded)) %>% 
    ungroup()
  
  # Prep a column in Jump_groups to store the distance of each point to the centroid (and find the closest one)
  Jump_groups %>% add_column(DistToCentroid = NA)
  
  # Map centroids to see how they fit in the group
  # ggplot(data = states) +
  #   geom_point(data = Jump_groups,
  #              aes(x = longitude_rounded, y = latitude_rounded, col = as.factor(Group)), 
  #              size = 2) +
  #   geom_point(data = Jumpers_centroids %>% filter(Group %in% Jump_groups_cumul$Group), #here I filtered only groups that had N > 1 point
  #              aes(x = longitude_rounded, y = latitude_rounded)) +
  #   geom_text(data = states,
  #             aes(X, Y, label = code), size = 4) +
  #   labs(x = "Longitude", y = "Latitude")+
  #   geom_sf(data = states, alpha = 0) + 
  #   coord_sf(xlim = c(-82, -72), ylim = c(38, 43), expand = FALSE) + 
  #   theme(legend.position="right")
  
  
  ##### Find the point closest to that centroid. 
  # Create the shapefiles with jumpers with unique points
  # Object with all jumps: Jump_groups
  Jumps_layer <- st_as_sf(x = Jump_groups, coords = c("longitude_rounded", "latitude_rounded"), crs = 4326, remove = F)
  Jumpers_proj <- st_transform(Jumps_layer)
  
  # Object with centroids: Jumpers_centroids
  Centroids_layer <- st_as_sf(x = Jumpers_centroids, coords = c("longitude_rounded", "latitude_rounded"), crs = 4326, remove = F)
  Centroids_proj <- st_transform(Centroids_layer)
  
  # Calculate their distance to the centroid
  for (j in 1:length(Jump_groups$Group)){ 
    Jumper_group = Jumpers_proj$Group[j]
    Jumpers_proj$DistToCentroid[j] <- st_distance(x = Jumpers_proj[j,], y = Centroids_proj %>% filter(Group == Jumper_group))
  }
  
  # Keep the closest point
  st_geometry(Jumpers_proj) <- NULL
  Jumpers_unique <- Jumpers_proj %>% 
    group_by(Group) %>% 
    slice(which.min(DistToCentroid)) %>% 
    ungroup() %>% 
    select(-DistToCentroid)
  
  return(Jumpers_unique)
}