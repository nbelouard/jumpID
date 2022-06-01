#'Determine sectors for each point based on trigonometry
#'
#'A full circle is 2*pi, the sector of each point is determined 
#'by atan2(y,x), the angle of the point relative to the horizontal line 
#'of the introduction site
#'
#'@export
#'
#'@param dataset A dataset to be processed
#'@param nb_sectors Number of sectors to divide the space in (default: 8)
#'@param centroid Coordinates of the centroid to center the circle (lat, long; default: -75.675340, 40.415240)
#'@param rotation Number of rotations of the grid wanted (default: 1)
#'
#'@return The same table as \code{dataset} with an additional column named 
#'\code{sectors} and containing the sector number for each row 
#' 
#'@examples
#' new_dataset <- attribute_sectors(dataset)


attribute_sectors <- function(dataset, nb_sectors = 8, centroid = c(-75.675340, 40.415240), rotation = 1) {
  
  # Calculate theta, the angle between the point and the horizontal line, for each point
  x = NULL
  y = NULL
  # dataset$theta <- NA
  # grid_data <- dataset
  grid_data <- dataset %>% add_column(theta = NA,
                                      sectors = nb_sectors)
  
  for (i in 1:length(grid_data$longitude_rounded)) {
    x = grid_data$longitude_rounded[i] - centroid[1]
    y = grid_data$latitude_rounded[i] - centroid[2]
    grid_data$theta[i] = base::atan2(y,x) + pi
  }
  
  # Create a variables for the angle
  angle_sector = 2*pi/nb_sectors
  p = angle_sector/rotation
  # Attribute the right disk portion number
  for (i in 0:(rotation-1)){
    grid_data <- grid_data %>% mutate(thetanew = theta - p*i,
                                      thetanew = ifelse(thetanew < 0, thetanew + 2*pi, thetanew),
                                      sector = ceiling((thetanew)/angle_sector)) %>%
      dplyr::select(-thetanew)
    names(grid_data)[which(names(grid_data) == 'sector')] <- paste("rotation", i+1, sep = "")
  }
  
  return(grid_data)
}
