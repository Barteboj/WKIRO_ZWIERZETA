init_gabor <- function(rot, RF_siz, Div) {
  #TODO docs
  #
  #
  
  require("matlab")
  c1OL <- 2;
  numFilterSizes <- length(RF_siz)
  numSimpleFilters <- length(rot);
  numFilters <- numFilterSizes*numSimpleFilters;
  fSiz <- zeros(numFilters,1);	# vector with filter sizes
  filters <- zeros(max(RF_siz)^2,numFilters);
  
  lambda = RF_siz*2/Div;
  sigma  = lambda*0.8;
  G      = 0.3;   # spatial aspect ratio: 0.23 < gamma < 0.92
  
  for (k in 1:numFilterSizes) {
    for (r in 1:numSimpleFilters) {
      theta     = rot[r]*pi/180;
      filtSize  = RF_siz[k];
      center    = ceil(filtSize/2);
      filtSizeL = center-1;
      filtSizeR = filtSize-filtSizeL-1;
      sigmaq    = sigma[k]^2;
      f <- array(dim = c(filtSize, filtSize))
      
      for (i in -filtSizeL:filtSizeR) {
        for (j in -filtSizeL:filtSizeR) {
          
          if ( sqrt(i^2+j^2)>filtSize/2 ) {
            E = 0
          } else {
            x = i*cos(theta) - j*sin(theta)
            y = i*sin(theta) + j*cos(theta)
            E = exp(-(x^2+G^2*y^2)/(2*sigmaq))*cos(2*pi*x/lambda[k])
          }
          f[j+center,i+center] = E
        }
      }
      f = f - mean(colMeans(f))
      f = f / sqrt(sum(sum(f^2)))
      p = numSimpleFilters*(k-1) + r
      filters[1:filtSize^2,p]=matlab::reshape(f,filtSize^2,1)
      fSiz[p]=filtSize;
    }
  }
  l <- list(fSiz, filters, c1OL, numSimpleFilters)
  return(l)
}
