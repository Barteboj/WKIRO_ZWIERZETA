C2 = function(stim,filters,fSiz,c1SpaceSS,c1ScaleSS,c1OL,s2Target,c1){
  
  source('WindowedPatchDistance.r')
  
  s1 = list()
  
  nargin = length(as.list(match.call())) - 1
  if (nargin < 8) {
    C1Value = C1(stim,filters,fSiz,c1SpaceSS,c1ScaleSS,c1OL)
    c1 = C1Value[[1]]
    s1 = C1Value[[2]]
  }
  
  nbands = length(c1)
  c1BandImage = c1
  nfilts = length(c1[[1]])
  n_rbf_centers = size(s2Target)[2]
  L = size(s2Target)[1] / nfilts;
  PatchSize = c(L^.5,L^.5,nfilts)
  
  s2 = cell(n_rbf_centers, 1)
  
  #Build s2:
  # for all prototypes in s2Target (RBF centers)
  # for all bands
  # calculate the image response
  for(iCenter in 1:n_rbf_centers) {
    Patch = reshape(as.array(s2Target[,iCenter]), PatchSize)
    s2[[iCenter]] = cell(nbands, 1)
    for(iBand in 1:nbands) {
      s2[[iCenter]][[iBand]] = WindowedPatchDistance(c1BandImage[[iBand]],Patch)
    }
  }
  
  #Build c2:
  # calculate minimum distance (maximum stimulation) across position and scales
  c2 = matrix(data = Inf, ncol = n_rbf_centers, nrow = 1)
  for (iCenter in 1:n_rbf_centers) {
    for (iBand in 1:nbands) {
      c2[iCenter] = min(c2[iCenter],min(min(s2[[iCenter]][[iBand]])));
    }
  }
  
  return(list(c2, s2, c1, s1))
}