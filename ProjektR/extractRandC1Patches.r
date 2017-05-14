extractRandC1Patches = function(cItrainingOnly, numPatchSizes, numPatchesPerSize, patchSizes) {
  #extracts random prototypes as part of the training of the C2 classification 
  #system. 
  #Note: we extract only from BAND 2. Extracting from all bands might help
  #cPatches the returned prototypes
  #cItrainingOnly the training images
  #numPatchesPerSize is the number of sizes in which the prototypes come
  #numPatchesPerSize is the number of prototypes extracted for each size
  #patchSizes is the vector of the patche sizes
  
  library('matlab')
  source('C1.r')
  
  nImages = length(cItrainingOnly)
  
  #----Settings for Training the random patches--------#
  rot = c(90, -45, 0, 45)
  c1ScaleSS = c(1, 3)
  RF_siz    = c(11, 13)
  c1SpaceSS = c(10)
  minFS     = 11
  maxFS     = 13
  div       = seq(4, 3.2, -0.05)
  Div       = div[3:4]
  #--- END Settings for Training the random patches--------#
  
  print('Initializing gabor filters -- partial set...')
  initGaborResult  = init_gabor(rot, RF_siz, Div)
  fSiz                = initGaborResult[[1]]
  filters             = initGaborResult[[2]]
  c1OL                = initGaborResult[[3]]
  numSimpleFilters    = initGaborResult[[4]]
  print('done')
  
  cPatches = list()
  bsize = c(0, 0)
  
  pind = zeros(numPatchSizes,1)
  for (j in 1:numPatchSizes) {
    cPatches[j] = list(zeros(patchSizes[j]^2*4,numPatchesPerSize))
  }
  
  for (i in 1:numPatchesPerSize) {
    ii = floor(runif(1)*nImages) + 1
    print(paste(as.character(i), ' from ', as.character(numPatchesPerSize), ' done'))
    stim = cItrainingOnly[[ii]];
    img_siz = size(stim);
    
    c1Result = C1(stim, filters, fSiz, c1SpaceSS, c1ScaleSS, c1OL)
    c1source = c1Result[[1]]
    s1source = c1Result[[2]]
    dimensions = dim(c1source[[1]][[1]])
    len = length(c1source[[1]])
    arrayDimensions = c(dimensions, len) 
    b = array(dim = arrayDimensions) #new C1 interface
    for(i in 1:len) {
      b[,,i] = c1source[[1]][[i]]
    }
    bsize[1] = dim(b)[1]
    bsize[2] = dim(b)[2]
    for (j in 1:numPatchSizes) {
      xy = floor(runif(2)*(bsize - patchSizes[j])) + 1
      seq
      tmp = b[seq(from=xy[1], length.out = patchSizes[j]), seq(from=xy[2], length.out = patchSizes[j]),]
      pind[j] = pind[j] + 1
      cPatches[[j]][,pind[j]] = reshape(tmp, size(tmp))
    }
  }
  return(cPatches)
}
