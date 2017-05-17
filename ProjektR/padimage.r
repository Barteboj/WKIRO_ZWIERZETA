library(matlab)

padimage <- function(i,amount,method) {
  
  if (length(as.list(match.call())) - 1 < 3)
    method = 'replicate'
  
  paddedImage = array(0, c(size(i,1) + 2 * amount, size(i,2) + 2* amount, size(i,3)))
  for (n in 1 : size(i,3))
  {
    paddedImage[,,n] = padarray(i[,,n], c(amount, amount), method, "both")
  } 
  return (paddedImage)
}
