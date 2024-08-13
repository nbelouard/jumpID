#'Find dispersal jumps 
#'
#'Find dispersal jumps after having applied find_thresholds(). Prune points selected due to sector limits and secondary diffusion.

#'@export
#'
#'@param dataset A data frame of unique points to be processed, ideally the dataset_unique resulting from find_thresholds()
#'@param nb_sectors Number of sectors in which the invasion range is divided
#'@param centroid long,lat coordinates of the introduction point
#'@param gap_size Distance between the invasion front and the positive point
#'necessary for it to be considered a jump, in kilometers (default: 15)
#'@param negatives Should negative surveys be considered? (default: TRUE)
#'
#'@return Three data frames: \code{Jumps} containing all jumps, \code{Dist} containing the 
#'limits of the diffusive spread, and \code{secDiff} containing secondary diffusion cells
#'
#'@examples
#'\dontrun{
#' jumps <- find_jumps(dataset)
#' }


find_jumps_wrapper <- function(dataset = grid_data, 
                               nb_sectors = 12,
                               centroid = c(-75.675340, 40.415240),
                               gap_size = 15,
                               negatives = T){
  
                         
  #1 Attribute sectors
  grid_data_sectors <- slfjumps::attribute_sectors(dataset = grid_data, # dataset to be explored
                                                   nb_sectors = nb_sectors, # number of sectors to divide the invasion range
                                                   centroid = centroid) # vector containing the centroid coordinates as long/lat
  
  
    #2 Find thresholds
  Results_thresholds <- slfjumps::find_thresholds(dataset = grid_data_sectors, 
                                                  gap_size = gap_size, 
                                                  negatives = negatives)

  
  #3 Find jumps
  Results_jumps <- slfjumps::find_jumps(grid_data = grid_data, 
                                        potJumps = Results_thresholds$potJumps,
                                        gap_size = gap_size)
  
  
  #4 Find sec diff
  Results_secDiff <- slfjumps::find_secDiff(potDiffusion = Results_jumps$potDiffusion,
                                            Jumps = Results_jumps$Jumps,
                                            diffusers = Results_jumps$diffusers,
                                            Dist = Results_thresholds$preDist,
                                            gap_size = gap_size)


  # select the results to return
  results <- list("Dist" = Results_secDiff$Dist, 
                  "Jumps" = Results_jumps$Jumps, 
                  "secDiff" = Results_secDiff$secDiff)
  
  return(results)
} 
