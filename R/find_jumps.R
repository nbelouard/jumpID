#'Find jump dispersal events
#'
#'Find jump dispersal events after having applied attribute_sectors
#'
#'@export
#'
#'@param dataset A dataset to be processed
#'@param bio_year Years to be considered (default: 2014 to 2021)
#'@param gap_size Distance between the invasion front and the positive point
#'necessary for it to be considered a jump, in kilometers (default: 15)
#'
#'@return Two tables: one table \code{Thresholds} containing the threshold per year and sector,
#'one table \code{Jumps} containing the list of jumps
#' 
#'@examples
#' jumps <- find_jumps(dataset)


find_jumps <- function(dataset, 
                                        bio_year = c(2014:2021),
                                        gap_size = 15) {
  
  ##############################################################################################
  ## PHASE 1: IDENTIFY THE THREHOLDS OF DIFFUSIVE SPREAD, AND GET A LIST OF POTENTIAL JUMPS  ###
  ##############################################################################################
  #Initialize variables for the results
  Dist = NULL
  Jumps_alls = NULL
  Jumps_allr = NULL
  rotation = unique(sort(dataset$rotation_nb)) #Look for the number of rotations in the dataset
  sector = unique(sort(dataset$sectors_nb)) #Look for the number of portions in the dataset
  
  for (rot in rotation){
    dataset_rot <- dataset %>% filter(rotation_nb == rot) #Create a dataset for this rotation
    
    for (s in sector){
      dataset_n = NULL
      jumpers_sector = data.frame(DistToIntro = 0)
      for (y in bio_year){
        
        #Select the dataset. We assume that no population is going extinct over the years and cumulate datasets.
        dataset_n <- rbind(dataset_n, dataset_rot %>% filter(sectors_nb == s & bio_year == y & sp_established == T))
        if (dim(dataset_n)[1] == 0){ #If there is no point in the sector up to that year, go to the next sector
          next
        }
        
        # Initialize values
        i = 1
        distancei = 1
        j = 2
        distancej = 2
        
        # Order the variable by increasing order
        distance_sorted <- sort(dataset_n$DistToIntro) 
        
        # Loop until it finds the threshold or until the variable is finished
        while ( (distancei + gap_size > distancej) & (j <= length(distance_sorted)) ) { 
          distancei = distance_sorted[i]
          distancej = distance_sorted[j]
          i = i+1
          j = j+1
        }
        
        if (distancei + gap_size > distancej) { # there is no jump
          threshold = distance_sorted[i]
        } else { #a jump was found, take the previous iteration
          threshold = distance_sorted[i-1]
          # print(paste0("Jump in ", rot, ", portion ", p, " and year ", y, " after distance ", distancei))
          # We make sure that the gap is not due to an absence of surveys!
          dataset_total <- dataset_rot %>% filter(sectors_nb == s & bio_year %in% c(2014:y) & between(DistToIntro, threshold, threshold+15))
          dim(dataset_total)[1]
          if (dim(dataset_total)[1] == 0) { 
            print(paste0("Error: there is no survey in the gap in rotation ", rot, ", sector ", s, " and year ", y, " after distance ", threshold)) 
          }
        }
        
        
        
        #Find the threshold survey in the initial table (that is not ordered)
        rowNumber = which(grepl(threshold, dataset_n$DistToIntro))
        if (length(rowNumber > 1)) { rowNumber = tail(rowNumber, n = 1)} #If the threshold has been the same for several years, take the last year
        
        #Store results in objects
        threshold_survey = dataset_n[rowNumber,]
        
        # Make sure the threshold is associated to the correct year, even if the threshold is the same as the year before
        threshold_survey$bio_year <- y
        jump_survey = dataset_n %>% filter(DistToIntro > threshold & bio_year == y)
        
        # Add results at each iteration
        Dist = rbind(Dist, threshold_survey)
        jumpers_sector = dplyr::bind_rows(jumpers_sector, jump_survey)
      }
      Jumps_alls = rbind(Jumps_alls, jumpers_sector[-1,])
    }
    
    Jumps_allr = rbind(Jumps_allr, Jumps_alls)
  } 
  
  # Reduce the jump list and the dataset to unique points (without repetitions due to rotations)
  Jumps_all = Jumps_allr %>% dplyr::select(-c(sectors_nb, rotation_nb, sectors)) %>% unique()
  dataset_unique = dataset %>% dplyr::select(-c(sectors_nb, rotation_nb, sectors)) %>% unique()
  
  
  
  ###########################################################################
  ## PHASE 2: KEEP ONLY JUMPS AT LEAST <gap size> AWAY FROM OTHER POINTS  ###
  ###########################################################################
  # Are surveys in the list of jump surveys real new jumps or just diffusion of secondary introductions from the previous year?
  # i.e. are new jumpers less than 10 miles from a previous jump?
  # we run that second part on the whole dataset, not within disk portions, to avoid the occurrence of false-positive)
  
  dataset_nprev = NULL
  Jumps = NULL
  
  for (y in bio_year[1:length(bio_year)]){
    jumps_year <- Jumps_all %>% dplyr::filter(bio_year == y)  #select jumps for this year
    dataset_all <- dataset_unique %>% dplyr::filter(bio_year %in% c(bio_year[1]:y) & sp_established == T) #all points up to this year
    doubles <- bind_rows(jumps_year, dataset_all) #aggregate the datasets
    dataset_diffusers <- doubles %>% group_by_all() %>% filter(n() == 1) #remove duplicate points = keep only diffusers
    
    jumps_year <- jumps_year %>% add_column(DistTosp = NA) #Create a column for the dist to the nearest other point
    
    if (dim(dataset_all)[1] == 0 | dim(jumps_year)[1] == 0){
      next
    } else {
      # Create shapefiles with the two sets of points
      dataset_diffusers_layer <- st_as_sf(x = dataset_diffusers, coords = c("longitude_rounded", "latitude_rounded"), crs = 4326, remove = F)
      dataset_diffusers_proj <- st_transform(dataset_diffusers_layer, crs = 4326)
      jumps_year_layer <- st_as_sf(x = jumps_year, coords = c("longitude_rounded", "latitude_rounded"), crs = 4326, remove = F)
      jumps_year_proj <- st_transform(jumps_year_layer, crs = 4326)
      
      #Calculate their pairwise distances
      for (jump in 1:length(jumps_year_proj$DistTosp)){
        pairwise_dist <- st_distance(x = jumps_year_proj[jump,], y = dataset_diffusers_proj)
        jumps_year_proj$DistTosp[jump] <- min(pairwise_dist)
      }
      
      #Select those at least 10 miles away from the others
      st_geometry(jumps_year_proj) <- NULL
      not_a_jump = jumps_year_proj %>% filter(DistTosp < gap_size*1000) 
      newjumpers = jumps_year_proj %>% filter(DistTosp > gap_size*1000)
    }
    
    
    
    #################################################################
    ## PHASE 3: REITERATE PHASE 2 WITH POINTS DISCARDED AS JUMPS  ###
    #################################################################
    # Last precaution: re-iterate the analysis with points that were finally not jumps
    # until the dataset stabilises to a list of real jumps away from any other point
    
    if (dim(newjumpers)[1] != 0){ #if jumpers are identified this year, let's check if they are true
      
      while (dim(not_a_jump)[1] != 0){ # until we don't deny any more jumper
        # Create shapefiles with the two sets of points
        notajump_layer <- st_as_sf(x = not_a_jump, coords = c("longitude_rounded", "latitude_rounded"), crs = 4326, remove = F)
        notajump_proj <- st_transform(notajump_layer, crs = 4326)
        newjumpers_layer <- st_as_sf(x = newjumpers, coords = c("longitude_rounded", "latitude_rounded"), crs = 4326, remove = F)
        newjumpers_proj <- st_transform(newjumpers_layer, crs = 4326)
        
        #Calculate their pairwise distances
        for (jump in 1:length(newjumpers_proj$DistTosp)){
          pairwise_dist <- st_distance(x = newjumpers_proj[jump,], y = notajump_proj)
          newjumpers_proj$DistTosp[jump] <- min(pairwise_dist)
        }
        
        #Select those at least 10 miles away from the others
        st_geometry(newjumpers_proj) <- NULL
        not_a_jump = newjumpers_proj %>% filter(DistTosp < gap_size*1000)
        newjumpers = newjumpers_proj %>% filter(DistTosp > gap_size*1000)
      } 
    }
    
    Jumps = bind_rows(Jumps, newjumpers) #add the final list of jumpers for each year
  }
  
  results <- list("Dist" = Dist, "Jump" = Jumps)
  
  return(results)
} 