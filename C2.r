C2 <- function(stim,filters,fSiz,c1SpaceSS,c1ScaleSS,c1OL,s2Target,c1){
  
  DEBUG <- TRUE
  if (DEBUG) {
    debugSource('WindowedPatchDistance.r')
  } else {
    source('WindowedPatchDistance.r')
  }
  
  s1 <- list()
  
  nargin = nargin <- length(as.list(match.call())) - 1
  if (nargin < 8) {
    C1_output <- C1(stim,filters,fSiz,c1SpaceSS,c1ScaleSS,c1OL)
    c1 <- C1_output[[1]]
    s1 <- C1_output[[2]]
  }
  nbands <- length(c1)
  c1BandImage <- c1
  nfilts <- length(c1[[1]])
  n_rbf_centers <- size(s2Target)[2]
  L <- size(s2Target)[1] / nfilts;
  PatchSize <- c(L^.5,L^.5,nfilts)
  
  s2 <- cell(n_rbf_centers, 1)
  
  #Build s2:
  # for all prototypes in s2Target (RBF centers)
  # for all bands
  # calculate the image response
  for(iCenter in 1:n_rbf_centers) {
    t <- s2Target[,iCenter]
    Patch <- reshape(as.array(t), PatchSize)
    s2[[iCenter]] <- cell(nbands, 1)
    for(iBand in 1:nbands) {
      t <- WindowedPatchDistance(c1BandImage[[iBand]],Patch)
      s2[[iCenter]][[iBand]] <- t
    }
  }
  
  #Build c2:
  # calculate minimum distance (maximum stimulation) across position and scales
  #c2 = inf(n_rbf_centers, 1)
  c2 <- matrix(data = Inf, ncol = n_rbf_centers, nrow = 1)
  for (iCenter in 1:n_rbf_centers) {
    for (iBand in 1:nbands) {
      t <- s2[[iCenter]][[iBand]]
      m <- min(as.vector(t))
      m <- c(c2[iCenter], m)
      c2[iCenter] <- min(m)
    }
  }
  
  print('C2 done')
  
  output = list(c2, s2, c1, s1)
  
  return(output)
}