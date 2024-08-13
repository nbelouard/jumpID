#'Attribute groups to jumps
#'
#'Identify independent locations of jump dispersal
#'
#'@export
#'
#'@param jumps A dataset to be processed
#'@param gap_size Distance between the invasion front and the positive point
#'necessary for it to be considered a jump, in kilometers (default: 15)
#'
#'@return The same table as \code{dataset} with an additional column named 
#'\code{group} and containing the group number for each jump
#' 
#'@examples
#'\dontrun{
#' new_dataset <- group_jumps(dataset)
#' }

group_jumps <- function(jumps, gap_size = 15) {
  
  # 1. Populate the list of neighbors for each jump
  jumps %<>% dplyr::mutate(ID = seq(1:dim(jumps)[1]),
                        Neighbors = list(NULL))
  
  # Calculate all pairwise distances between points
  pairwise_dist <- geosphere::distm(jumps[,c('longitude', 'latitude')])/1000
  pairwise_dist <- as.data.frame(pairwise_dist)
  
  
  for (i in 1:dim(pairwise_dist)[1]) { #take each point of the table
    
    for (j in 1:dim(pairwise_dist)[2]) { # go through all the distances with other points
      if (pairwise_dist[i,j] < gap_size) { # if a point is closer than a gap size to the other point, but not the same point
        jumps$Neighbors[[i]] <- c(jumps$Neighbors[[i]], j) # Add it to the table of neighbors
      }
    }
  }
    
  
  # 2. Make a list of extended neighbors 
  # (merge the lists of neighbor points with common points to form groups)

  jumps %<>% dplyr::mutate(Neighbors_extended = Neighbors)
  
  for (i in 1:(length(jumps$ID)-1)){ 
    for (j in 2:length(jumps$ID)) { #consider the next point in the table
      if (is.null(jumps$Neighbors[[j]]) == F & #if the new point considered j has neighbors
          jumps$ID[[j]] %in% jumps$Neighbors_extended[[i]]) { # and if j is in the list of neighbors of i  
        
        # Add neighbors of j to the list of neighbors of i
        jumps$Neighbors_extended[[i]] <- sort(unique(c(jumps$Neighbors_extended[[i]], jumps$Neighbors_extended[[j]])))
        # Add neighbors of i to the list of neighbors of j
        jumps$Neighbors_extended[[j]] <- sort(unique(c(jumps$Neighbors_extended[[i]], jumps$Neighbors_extended[[j]])))
      }
    }
  }

  # 3. Attribute group names
  jumps %<>% dplyr::mutate(Group = NA) 
  groups = unique(jumps$Neighbors_extended)
  
  for (i in 1:length(groups)){ 
    jumps %<>% dplyr::mutate(Group = replace(Group, ID %in% groups[[i]], i)) 
  }
  
  jumps %<>% dplyr::select(-Neighbors, -Neighbors_extended, -ID)
  
  return(jumps)
  }
  