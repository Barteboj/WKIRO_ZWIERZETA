sumfilter <- function(sourceImage, radius) {
  require('matlab')
  source('convulation.r')
  
  if (length(dim(sourceImage)) > 2) {
    stop('The image is not single channel, stopping execution.')
  }
  
  temporary <- matrix()
  returnMatrix <- matrix()
  
  if (length(radius) == 4) 
  {
    temporary = conv2vector(t(ones(1, radius[2]+radius[4] + 1)), ones(radius[1] + radius[3] + 1, 1), sourceImage)
    returnMatrix = temporary[seq(radius[4]+1, radius[4]+size(sourceImage,1)), seq(radius[3]+1,radius[3]+size(sourceImage,2))]
  }
  else if (length(radius) == 1) 
  {
    mask = ones(2*radius + 1,1)
    temporary = conv2vector(mask, mask, sourceImage)
    returnMatrix = temporary[(radius+1):(radius+size(sourceImage, 1)), (radius+1):(radius+size(sourceImage, 2))]
  }
  
  return(returnMatrix)
}
