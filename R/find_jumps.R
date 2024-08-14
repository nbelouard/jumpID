#'Find dispersal jumps 
#'
#'Find dispersal jumps after having applied find_thresholds(). Prune points selected due to sector limits and secondary diffusion.

#'@export
#'
#'@param grid_data A data frame of unique points to be processed, i.e., the initial grid_data table
#'@param potJumps A data frame of potential jumps to be refined, resulting from find_thresholds()
#'@param gap_size Distance between the invasion front and the positive point
#'necessary for it to be considered a jump, in kilometers (default: 15)
#'
#'@return Two data frames: \code{Jumps} containing the list of jumps, and \code{potSecDiff} containing
#'potential secondary diffusion to be processed using find_secDiff()
#' 
#'@examples
#'\dontrun{
#' jumps <- find_jumps(dataset)
#' }


find_jumps <- function(grid_data = grid_data, 
                       potJumps = potJumps,
                       gap_size = 15) {
  
  cat(paste(Sys.time(), "Start finding jumps... "))
  
  Jumps = NULL # create an empty object to store true jumps
  year <- unique(grid_data$year)

  for (y in year[1:length(year)]){ # for each year
    cat(paste("Year", y, "... "))
    
    jumps_year <- potJumps %>% dplyr::filter(year == y) %>% dplyr::select(-nb_sectors, -sectors_nb)  # select potential jumps for this year
    dataset_all <- grid_data %>% dplyr::filter(year %in% c(year[1]:y) & established == T) # select all points up to this year
    dataset_diffusers <- dplyr::setdiff(dataset_all, jumps_year) # select all points up to this year, except potential jumps this year (includes previous jumps)
    jumps_year %<>% dplyr::mutate(DistToSLF = NA) #Create an empty column for the distance to the nearest other point
    
    # Check if potential jumps are real jumps by checking whether they are more than 
    #  <gap size> away from any other previous point
    if (dim(dataset_all)[1] == 0 | dim(jumps_year)[1] == 0){ 
      next # if there are no potential jumps, go to the next year
    } else { # if there are potential jumps this year,

      #Calculate the pairwise distances between jumps from that year and all other points up to that year
      for (jump in 1:length(jumps_year$DistToSLF)){ # for each potential jump
        
        # select only points within the DistToIntro to explore, to reduce computation time
        dataset_diffusers_j <- dataset_diffusers %>% dplyr::filter(dplyr::between(DistToIntro,
                                                                      as.numeric(jumps_year[jump,'DistToIntro']) - gap_size, 
                                                                      as.numeric(jumps_year[jump,'DistToIntro'] + gap_size)))
        
        if (dim(dataset_diffusers_j)[1] > 0){
          pairwise_dist <- geosphere::distGeo(jumps_year[jump,c('longitude', 'latitude')], 
                                              dataset_diffusers_j[c('longitude', 'latitude')])/1000
          jumps_year$DistToSLF[jump] <- min(pairwise_dist) # add the minimal distance to the table
          } else {
            jumps_year$DistToSLF[jump] <- gap_size # if no point is close, indicate the gap size
        } 
        
     }
      
      # Select jumps at least <gap size> away from all other points, these are potential new jumps
      # sf::st_geometry(jumps_year_proj) <- NULL
      newjumps = jumps_year %>% dplyr::filter(DistToSLF >= gap_size) # real new jumps
      notajump = jumps_year %>% dplyr::filter(DistToSLF < gap_size) # not_a_jump are either close to the diffusion core or to a previous jump
    }
  

    # notajump points may advance the core invasion front, which may remove other potential jumps, 
    # so we need to reiterate the analysis with the new invasion front
    # until the dataset stabilises to a list of real jumps away from any other point
    
    if (dim(newjumps)[1] != 0){ # if there are still new jumps for this year, check them
      
      while (dim(notajump)[1] != 0){ # until we don't deny any more points as jumps
     
        #Calculate their pairwise distances
        for (jump in 1:length(newjumps$DistToSLF)){
          pairwise_dist <- geosphere::distGeo(newjumps[jump, c('longitude', 'latitude')], notajump[c('longitude', 'latitude')])/1000
          newjumps$DistToSLF[jump] <- min(pairwise_dist)
        }
        
        #Select those at least <gap size> away from all other points, these are potential new jumps
        # sf::st_geometry(newjumps_proj) <- NULL
        notajump = newjumps %>% dplyr::filter(DistToSLF < gap_size)
        newjumps = newjumps %>% dplyr::filter(DistToSLF >= gap_size)
        
      } 
    }
    
    Jumps = dplyr::bind_rows(Jumps, newjumps) #add the final list of jumpers for each year
  }

  
  # Select points that are potential thresholds or secondary diffusion
  # i.e. were potential jumps but finally not jumps
  potDiffusion = dplyr::setdiff(potJumps %>% dplyr::select(-nb_sectors, -sectors_nb), 
                                Jumps %>% dplyr::select(-DistToSLF))
  diffusers = dplyr::setdiff(grid_data %>% dplyr::filter(established == T), 
                             Jumps %>% dplyr::select(-DistToSLF)) %>% 
    dplyr::setdiff(potDiffusion)

  cat(paste("Jump analysis done.", dim(Jumps)[1], "jumps were identified.\n"))
  # select the results to return
  results <- list("Jumps" = Jumps, "potDiffusion" = potDiffusion, "diffusers" = diffusers)
  
  return(results)
} 
