WindowedPatchDistance <- function(Im, Patch) {
  
  DEBUG <- TRUE
  if (DEBUG) {
    debugSource('sumfilter.r')
    debugSource('conv2same.r')
  } else {
    source('sumfilter.r')
    source('conv2same.r')
  }
  
  dIm <- length(Im)
  dPatch <- dim(Patch)[3]
  if (dIm != dPatch) {
    print('The patch and image must be of the same number of layers')
  }
  
  #convert list to array
  d <- dim(Im[[1]])
  arrDims <- c(d, dIm) 
  im <- array(dim = arrDims)
  for(i in 1:dIm) {
    im[,,i] <- Im[[i]]
  }
  
  s = size(Patch)
  s[3] = dIm
  t <- Patch ^ 2
  Psqr <- sum(as.vector(t))
  Imsq <- im ^ 2
  ImSum <- matrix(data = 0, nrow = d[1], ncol = d[2])
  #sum over 3th dimension
  for (i in 1:dIm) {
    ImSum <- ImSum + Imsq[,,i]
  }
  
  sum_support <- c(ceil(s[2] / 2) - 1,
                   ceil(s[1] / 2) - 1,
                   floor(s[2] / 2),
                   floor(s[1] / 2))
  ImSum <- sumfilter(ImSum, sum_support)
  PI <- zeros(size(im)[-3])
  
  for(i in 1:dIm){
    t1 <- im[,,i]
    t2 <- Patch[,,i]
    t3 <- conv2same(t1, t2)
    PI <- PI + t3
  }
  
  D = ImSum - 2 * PI + Psqr + 10^-10
  return(D)
}