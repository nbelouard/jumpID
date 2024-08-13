#'Determine sectors for each occurrence point
#'
#'A full circle is 2*pi, the sector of each point is determined 
#'by atan2(y,x), the angle of the point relative to the horizontal line 
#'of the introduction site
#'
#'@export
#'
#'@param dataset A dataset to be processed
#'@param nb_sectors Number of sectors to divide the invasion space (default: 8)
#'@param centroid Coordinates of the centroid to center the circle (long, lat; 
#'default: -75.675340, 40.415240)
#'
#'@return The same table as \code{dataset} with additional columns: 
#'\code{sectors} containing the total number of sectors considered,  
#'\code{sectors_nb} containing the sector number attributed to this cell  
#' 
#'@examples
#'\dontrun{
#' new_dataset <- attribute_sectors(dataset)
#'}


attribute_sectors <- function(dataset, 
                              nb_sectors = 8, 
                              centroid = c(-75.675340, 40.415240)) {
  
  cat(paste(Sys.time(), "Start sector attribution... "))
  x = NULL
  y = NULL
  
  # create a column to remind of the total number of sectors considered
  grid_data <- dataset %>% dplyr::mutate(nb_sectors = nb_sectors) 
  
  # Create an object containing the angle of a sector
  # a full circle is 2pi, divide by nb_sectors to get the angle of one sector
  angle_sector = 2*pi/nb_sectors 
  
  # Calculate theta, the angle between the point and the horizontal of the 
  # centroid, for each point
  grid_data %<>% dplyr::mutate(theta = base::atan2(latitude - centroid[2],
                                           longitude - centroid[1]) + pi)

  # Attribute the sector number for each point
  grid_data %<>% dplyr::mutate(sectors_nb = ceiling(theta/angle_sector)) %>% 
    dplyr::select(-theta) # remove theta that will not be used anymore
  
  cat(paste("Sector attribution completed. \n"))
  
  return(grid_data)
}
