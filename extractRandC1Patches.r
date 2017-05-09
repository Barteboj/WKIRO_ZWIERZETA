extractRandC1Patches <- function(cItrainingOnly, numPatchSizes, numPatchesPerSize, patchSizes) {
  #extracts random prototypes as part of the training of the C2 classification 
  #system. 
  #Note: we extract only from BAND 2. Extracting from all bands might help
  #cPatches the returned prototypes
  #cItrainingOnly the training images
  #numPatchesPerSize is the number of sizes in which the prototypes come
  #numPatchesPerSize is the number of prototypes extracted for each size
  #patchSizes is the vector of the patche sizes
  
  DEBUG <- TRUE
  
  require('matlab')
  if (DEBUG) {
    debugSource('C1.r')
  } else {
    source('C1.r')
  }
  
  #TODO some default values
  #if nargin<2
  #numPatchSizes = 4;
  #numPatchesPerSize = 250;
  #patchSizes = 4:4:16;
  #end
  
  nImages <- length(cItrainingOnly)
  
  #----Settings for Training the random patches--------#
  rot <- c(90, -45, 0, 45)
  c1ScaleSS <- c(1, 3)
  RF_siz    <- c(11, 13)
  c1SpaceSS <- c(10)
  minFS     <- 11
  maxFS     <- 13
  div       <- seq(4, 3.2, -0.05)
  Div       <- div[3:4]
  #--- END Settings for Training the random patches--------#
  
  print('Initializing gabor filters -- full set...')
  init_gabor_ret_val  <- init_gabor(rot, RF_siz, Div)
  fSiz                <- init_gabor_ret_val[[1]]
  filters             <- init_gabor_ret_val[[2]]
  c1OL                <- init_gabor_ret_val[[3]]
  numSimpleFilters    <- init_gabor_ret_val[[4]]
  print('done')
  
  cPatches <- list()
  bsize <- c(0, 0)
  
  pind <- zeros(numPatchSizes,1)
  for (j in 1:numPatchSizes) {
    cPatches[j] <- list(zeros(patchSizes[j]^2*4,numPatchesPerSize))
  }
  
  for (i in 1:numPatchesPerSize) {
    if (DEBUG) {
      ii <- floor(0.5*nImages) + 1
    } else {
      ii <- floor(runif(1)*nImages) + 1
    }
    
    print(paste(as.character(i / numPatchesPerSize * 100.0), ' %'))
    stim <- cItrainingOnly[[ii]];
    img_siz <- size(stim);
    
    c1_reval <- C1(stim, filters, fSiz, c1SpaceSS, c1ScaleSS, c1OL)
    c1source <- c1_reval[[1]]
    #s1source <- c1_reval[[2]] #it seems that it is unused
    
    #new C1 interface
    d <- dim(c1source[[1]][[1]])
    l <- length(c1source[[1]])
    arrDims <- c(d, l) 
    b <- array(dim = arrDims)
    for(i in 1:l) {
      b[,,i] <- c1source[[1]][[i]]
    }
    bsize[1] <- dim(b)[1]
    bsize[2] <- dim(b)[2]
    
    for (j in 1:numPatchSizes) {
      if (DEBUG) {
        xy <- floor(c(0.1, 0.8)*(bsize - patchSizes[j])) + 1
      } else {
        xy <- floor(runif(2)*(bsize - patchSizes[j])) + 1
      }
      tmp <- b[seq(from=xy[1], length.out = patchSizes[j]), seq(from=xy[2], length.out = patchSizes[j]),]
      pind[j] <- pind[j] + 1
      cPatches[[j]][,pind[j]] <- reshape(tmp, size(tmp))
    }
  }
  
  return(cPatches)
}
