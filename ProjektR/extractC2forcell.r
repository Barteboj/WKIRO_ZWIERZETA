extractC2forcell <- function(filters,fSiz,c1SpaceSS,c1ScaleSS,c1OL,cPatches,cImages,numPatchSizes){
  
  require('matlab')
  
  DEBUG <- TRUE
  if (DEBUG) {
    debugSource('C2.r')
  } else {
    source('C2.r')
  }
  
  numPatchSizes = min(numPatchSizes,length(cPatches))
  print("Done extrC2_1")
  for (i in 1:numPatchSizes){
    siz <- size(cPatches[[i]])[1]
    numpatch <- size(cPatches[[i]])[2]
    siz <- sqrt(siz/4)
    
    for(j in 1:numpatch){
      tmp <- array(data = cPatches[[i]][,j], dim = c(siz, siz, 4))
      tmp <- tmp[siz:1,siz:1,]
      tmp <- as.vector(tmp)
      cPatches[[i]][,j] <- tmp
    }
  }
  
  mC2 <- NULL;
  
  for(i in 1:length(cImages)){
    print(i)
    stim <- cImages[[i]]
    img_siz <- size(stim)
    c1 <- NULL
    iC2 <- NULL
    for(j in 1:numPatchSizes){
      if(is.null(c1)){
        c2_output <- C2(stim,filters,fSiz,c1SpaceSS,c1ScaleSS,c1OL,cPatches[[j]])
        tmpC2 <- c2_output[[1]]
        tmp <- c2_output[[2]]
        c1 <- c2_output[[3]]
      } else {
        c2_output <- C2(stim,filters,fSiz,c1SpaceSS,c1ScaleSS,c1OL,cPatches[[j]],c1) 
        tmpC2 <- c2_output[[1]]
      }
      iC2 <- t(t(c(iC2, tmpC2)))
    }
    mC2 <- cbind(mC2, iC2)
  }
  
  return(mC2)
}