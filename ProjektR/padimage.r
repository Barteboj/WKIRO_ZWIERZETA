library(matlab)

padimage <- function(i,amnt,method) {
  if (length(as.list(match.call())) - 1 < 3)
    method <- 'replicate'
  
  o <- array(0, c(size(i,1) + 2 * amnt, size(i,2) + 2* amnt, size(i,3)))
  for (n in 1 : size(i,3))
  {
    o[,,n] = padarray(i[,,n], c(amnt, amnt), method, "both")
  } 
  return (o)
}
