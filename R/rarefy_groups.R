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
#'\dontrun{
#' new_dataset <- rarefy_groups(dataset)
#'} 

rarefy_groups <- function(Jump_groups) {
  
  # Create a dataset with centroids for groups
  Jump_centroids <- Jump_groups %>% dplyr::group_by(year, Group) %>% 
    dplyr::summarise(latitude = mean(latitude), longitude = mean(longitude)) %>% 
    dplyr::ungroup() %>% 
    as.data.frame()
  
  # Calculate their distance to the centroid
  for (j in 1:length(Jump_groups$Group)){ 
    Jump_centroid <- Jump_centroids %>% dplyr::filter(Group == Jump_groups$Group[j]) 
    Jump_groups$DistToCentroid[j] <- geosphere::distGeo(Jump_groups[j,c('longitude', 'latitude')],
                                                       Jump_centroid[,c('longitude', 'latitude')]
                                                       )/1000
  }
  
  # Keep the closest point
  Jumps_unique <- Jump_groups %>% 
    dplyr::group_by(Group) %>% 
    dplyr::slice(which.min(DistToCentroid)) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-DistToCentroid)
  
  return(Jumps_unique)
}
