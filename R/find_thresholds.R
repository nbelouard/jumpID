#'Find thresholds of diffusive spread 
#'
#'Find thresholds of diffusive spread and get a list of potential jumps. 
#'This function identifies spatial discontinuities in the distribution of 
#'species’ occurrences between the putative limit of the continuous, diffusive 
#'spread (i.e., the invasion front) and outlying occurrences.
#'This function must be applied after attribute_sectors() and before find_jumps().
#'
#'@export
#'
#'@param dataset A dataset to be processed, output of attribute_sectors()
#'@param gap_size Distance between the invasion front and a positive point
#'necessary for it to be considered a jump, in kilometers (default: 15)
#'@param negatives Should the function verify that negative surveys were found 
#'in the spatial discontinuities? (default: TRUE)
#'
#'@return Two tables: one table \code{Thresholds} containing the threshold per year and sector,
#'one table \code{potJumps} containing the list of potential jumps for find_jumps()
#' 
#'@examples
#'\dontrun{
#' thresholds <- find_thresholds(dataset)
#' }


find_thresholds <- function(dataset, 
                       gap_size = 15,
                       negatives = TRUE) {
  
  cat(paste(Sys.time(), "Start finding thresholds... ")) 
            
  # Initialize variables to store the results
  sector = unique(sort(dataset$sectors_nb)) # Number of sectors used in the dataset
  year = unique(sort(dataset$year)) # Years considered
  
  Dist = NULL # Will store the thresholds of the diffusive spread
  jumps_sector = NULL # Will store potential jumps for each sector
  Jumps_alls = NULL # Will store potential jumps for all sectors 
  
  # calculate the distance at which points that have the same 'DistToIntro' may be distant of more than 'gap size'
  # Calculate the central angle in radians
  theta <- 2 * pi / max(sector)
  # circle radius (in km) at which 2 points start being distant of more than 'gap size'
  limitRadius <- gap_size / (2 * sin(theta / 2))



  for (s in sector){ # for each sector

    # send message to inform of analysis progression
    if (s == 1 & s == max(sector)) {
      cat(paste0("Sector ", as.numeric(s), "/", max(sector), "\n"))
    } else if(s == 1){
      cat(paste0("Sector ", as.numeric(s), "/", max(sector), "... "))
    } else if (s == max(sector)) {
      cat(paste0(" ", as.numeric(s), "/", max(sector), "\n"))
    } else {
      cat(paste0(" ", as.numeric(s), "/", max(sector), "... "))
    } 
    
    
    for (y in year){ # for each year in the dataset
      
      # Select the dataset to be examined, with the correct sector, and year
      # Additionally, we assume that no population is going extinct over the years 
      # and keep datasets from previous years. 
      dataset_s <- dataset %>% dplyr::filter(sectors_nb == s & 
                                            year %in% c(year[1]:y) & 
                                            established == T) %>%
        dplyr::arrange(DistToIntro, year) # Order rows to go through the distances
      
      if (dim(dataset_s)[1] == 0){ # If there are no points in the sector up to that year, 
        next # go to the next sector
      } else if (dim(dataset_s)[1] == 1) { # If there is a single point
        threshold = dataset_s[1,] # this point is the threshold
      } else { # If the dataset has more than one point, take a deeper look

      # Initialize iterations to identify spatial discontinuities in species presence
      i = 1 # initialize point 1
      j = 2 # initialize point 2

      # create a variable to stop the function when the threshold is found
      continue = T
      
      while (j <= length(dataset_s$DistToIntro) & continue) { # until the variable is finished or a jump is found
        distancei = dataset_s$DistToIntro[i]
        distancej = dataset_s$DistToIntro[j]


        
        # Case 1: j is a jump according to DistToIntro
        if (distancei + gap_size < distancej ) {
          threshold = dataset_s[i,] # the threshold of the diffusive spread is i
          continue = F # stop the while loop here

          if(negatives){ # if user indicated to look for negative surveys in the gap,
            # make sure that the gap is not due to an absence of surveys
            # select the dataset of negative surveys corresponding to the discontinuity
            dataset_negatives <- dataset %>% dplyr::filter(established == F &
                                                           sectors_nb == s &
                                                           year %in% c(year[1]:y) &
                                                           dplyr::between(DistToIntro, threshold$DistToIntro, threshold$DistToIntro + gap_size))
            if (dim(dataset_negatives)[1] == 0) { # if this dataset is empty, send a warning
              cat(paste0("Warning: no negative survey in the gap identified in sector ", s, " and year ", y, " after ", round(threshold$DistToIntro,0),
                           " km. The spatial discontinuity that is identified may be due to few surveys done in this area, or space divided into too many sectors. Consider decreasing the number of sectors \n"))
            }
          }



        # Case 2: no jump according to DistToIntro, but successive pairwise distances may be larger than
        # the difference in DistToIntro if DistToIntro is larger than limitRadius
        # } else if (distancej > limitRadius){
        } else {

            # take the dataset without this point, only for this year
            dataset_test = dataset_s[-j,]

            # select only points within the DistToIntro to explore, to reduce computation time
            dataset_test <- dataset_test %>% dplyr::filter(dplyr::between(DistToIntro,
                                                          as.numeric(dataset_s[j,'DistToIntro']) - gap_size, 
                                                          as.numeric(dataset_s[j,'DistToIntro'])))
        
            pairwise_dist <- geosphere::distGeo(dataset_test[c('longitude', 'latitude')], dataset_s[j,c('longitude', 'latitude')])/1000

            if (min(pairwise_dist) >= gap_size){ # j is far from any point, i is the threshold
              threshold = dataset_s[i,]
              continue = F

              if(negatives){ # if user indicated to look for negative surveys in the gap,
                # make sure that the gap is not due to an absence of surveys
                # select the dataset of negative surveys corresponding to the discontinuity
                dataset_negatives <- dataset %>% dplyr::filter(established == F &
                                                               sectors_nb == s &
                                                               year %in% c(year[1]:y) &
                                                               dplyr::between(DistToIntro, threshold$DistToIntro, threshold$DistToIntro + gap_size))

                pairwise_dist <- geosphere::distGeo(dataset_negatives[c('longitude', 'latitude')], dataset_s[j,c('longitude', 'latitude')])/1000

                if (min(pairwise_dist) >= gap_size) { # if no negative survey around j, send a warning
                  cat(paste0("Warning: no negative survey around point (",
                             round(as.numeric(dataset_s[j,'longitude']), 5), ",",
                             round(as.numeric(dataset_s[j,'latitude']),5),
                             "). The spatial discontinuity that is identified may be due to few surveys in this area, or space divided into too many sectors. Consider decreasing the number of sectors \n"))
                } 
              } # end of negatives loop
            }
          } # end of limitRadius loop

        i = i+1 # go to the next pair of points
        j = j+1 # go to the next pair of points
      }

      if(continue){threshold = dataset_s[i,]}

      }


      # Now, we know the threshold point for this sector,
      # Make sure the threshold is associated to the year we are looking at, even if the threshold is the same as the year before
      threshold$year <- y
      # all points after this threshold point are potential jumps and stored
      jump_survey = dataset_s %>% dplyr::filter(DistToIntro > threshold$DistToIntro & year == y)

      # Add results at each iteration (sector, year)
      Dist = rbind(Dist, threshold) # add this threshold to the list of thresholds
      jumps_sector = dplyr::bind_rows(jumps_sector, jump_survey) # add the potential jumps to the list of potential jumps
    }

      Jumps_alls = rbind(Jumps_alls, jumps_sector) # add potential jumps from this sector
  }

  # Reduce the lists to unique points (without repetitions due to multiple years)
  Jumps = Jumps_alls %>% unique()

  # This is a list of points that are away from the core invasion, i.e. potential jumps
  # It may still include points from the core invasion due to the separation in sectors.
  
  # Send an info message
  cat(paste("Threshold analysis done.", dim(Jumps)[1], "potential jumps were found. \n"))
  # select the results to return
  results <- list("preDist" = Dist, "potJumps" = Jumps)
  
  return(results)
} 
