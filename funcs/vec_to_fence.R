# bring a single vector to the fence

vec_to_fence <- function(vec){
  stats <- boxplot.stats(vec)$stats
  vec[vec < stats[1]] <- stats[1]
  vec[vec > stats[5]] <- stats[5]
  return(vec)
}