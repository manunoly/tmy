rmsd <- function(error){
  sqrt(sum(error^2)/length(error))
}