#'Find secondary diffusion 
#'
#'Find secondary diffusion points after having applied find_jumps()
#'
#'@export
#'
#'@param potDiffusion object exported from find_jumps
#'@param Jumps object exported from find_jumps
#'@param diffusers object exported from find_jumps
#'@param Dist object exported from find_jumps
#'@param gap_size Distance necessary for a point to be considered a jump, 
#'in kilometers (default: 15)
#'
#'@return A data frame \code{secDiff} containing the secondary diffusion points, a data frame \code{Dist} containing the final thresholds of the diffusive spread.
#' 
#'@examples
#'\dontrun{
#' secDiff <- find_secDiff()
#' }


find_secDiff <- function(potDiffusion = potDiffusion,
                         Jumps = Jumps,
                         diffusers = diffusers, 
                         Dist = preDist, 
                       gap_size = 15) {
  
  cat(paste(Sys.time(), "Start finding secondary diffusion... "))
  
  potDiffusion %<>% dplyr::arrange(year) # sort potential secondary diffusion points by year
  yearsDiffusion = unique(potDiffusion$year) # years where potential secondary diffusion points are identified
  newDist = data.frame()

    # for each year, we will look for points coming from secondary diffusion
    # for each year where there is secDiff, we take the year before (because if no secDiff, no need to)
    for (i in 1:length(yearsDiffusion)){
      cat(paste("Year", yearsDiffusion[i], "..."))
      # select jumps up to that year
      jumps_i = Jumps %>% dplyr::filter(dplyr::between(year, yearsDiffusion[1]-1, 
                                         yearsDiffusion[i]-1))
  
      # if there are jumps in this selection, continue, else go to the next year
      if (dim(jumps_i)[1] > 0){
        # select potential secDiff points that year
        potDiffusion_i = potDiffusion %>% 
          dplyr::filter(year == yearsDiffusion[i]) %>% 
          dplyr::mutate(DistToSLF = NA)
        newThresholds_i = data.frame(1)
        
        while(dim(newThresholds_i)[1] > 0 & dim(potDiffusion_i)[1] > 0){

          # select diffusion points up to that year
          diffusers_i <- diffusers %>% dplyr::filter(dplyr::between(year, yearsDiffusion[1]-1, 
                                                            yearsDiffusion[i]))
          
        for (point in 1:length(potDiffusion_i$DistToIntro)){ # for each potential diffusion
          rangeDist <- potDiffusion_i[point,] %>% dplyr::pull(DistToIntro)
          diffusers_pointi <- diffusers_i %>% dplyr::filter(dplyr::between(DistToIntro, rangeDist - gap_size, rangeDist + gap_size))
          # from all diffusers, we subselect those that are less than a gap size away when looking at DistToIntro to accelerate the analysis
          
          if (dim(diffusers_pointi)[1] > 0){
            pairwise_dist <- geosphere::distGeo(potDiffusion_i[point,c('longitude', 'latitude')], 
                                                diffusers_pointi[,c('longitude', 'latitude')])/1000
            
            potDiffusion_i$DistToSLF[point] <- min(pairwise_dist) # add the minimal distance to the table
          } else {
            potDiffusion_i$DistToSLF[point] <- gap_size # if no point is close, indicate the gap size
          } 
        }
        
        # Select points less than <gap size> away from all other points, these are new thresholds
        newThresholds_i = potDiffusion_i %>% dplyr::filter(DistToSLF < gap_size) # potDiffusion that are close to Thresholds
        # add these points to the diffusers' dataset
        diffusers <- dplyr::bind_rows(diffusers, newThresholds_i)
        newDist <- dplyr::bind_rows(newDist, newThresholds_i)
        
        potDiffusion_i <- potDiffusion_i %>% dplyr::filter(DistToSLF >= gap_size) # potDiffusion pruned of newThresholds
  
        }
      } else { # if there are no jumps the year before, there cannot be any secondary diffusion, all points are diffusion
        newThresholds_i = potDiffusion %>% dplyr::filter(year == yearsDiffusion[i]) 
        diffusers <- dplyr::bind_rows(diffusers, newThresholds_i)
        newDist = dplyr::bind_rows(newDist, newThresholds_i)
        }
    }
  
    Dist = dplyr::bind_rows(Dist, newDist)
    secDiff = dplyr::setdiff(potDiffusion, newDist %>% dplyr::select(-DistToSLF))

    # select the results to return
    results <- list("secDiff" = secDiff, "Dist" = Dist)
    
    cat("Analysis of secondary diffusion done. \n")
    
    return(results)
}
