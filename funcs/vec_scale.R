# bring a single vector to the fence

vec_scale <- function(vec){
  vec <- as.numeric(vec)
  stats <- boxplot.stats(vec)$stats
  vec[vec < stats[1]] <- stats[1]
  vec[vec > stats[5]] <- stats[5]
  
  vec <- as.numeric(scale(vec))
  
  return(vec)
}