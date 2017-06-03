#demoRelease.r
#demonstrates how to use C2 standard model features in a pattern classification framework

rm(list = ls()) #optional clear workspace

#dependencies:
source('readAllImages.r')
source('extractRandC1Patches.r')
source('init_gabor.r')
source('extractC2forcell.r')
source('SVMClassificate.r')
source('NNClassificate.r')

#TODO #put your own path to osusvm here

useSVM <- TRUE #if you do not have osusvm installed you can turn this
                #to 0, so that the classifier would be a NN classifier
                #note: NN is not a great classifier for these features

READPATCHESFROMFILE <- FALSE  #use patches that were already computed
                              #(e.g., from natural images)

patchSizes <- c(4, 8, 12, 16) #other sizes might be better, maybe not
                              #all sizes are required

numPatchSizes <- length(patchSizes)

#specify directories for training and testing images
train_pos <- 'images_to_use/in_use/train_pos'
train_neg <- 'images_to_use/in_use/train_neg'
test_pos  <- 'images_to_use/in_use/test_pos'
test_neg  <- 'images_to_use/in_use/test_neg'

cI <- readAllImages(train_pos, train_neg, test_pos, test_neg) #cI is a cell containing
                                                              #all training and testing images
if (length(cI$train_pos) == 0 | length(cI$train_neg) == 0) {
  stop('No training images were loaded -- did you remember to change the path names?')
}

if (length(cI$test_pos) == 0 | length(cI$test_neg) == 0) {
  stop('No testing images were loaded -- did you remember to change the path names?')
}

#below the c1 prototypes are extracted from the images/ read from file
if (!READPATCHESFROMFILE) {
  readPatchesStartTime <- Sys.time()
  numPatchesPerSize <- 10  #more will give better results, but will
                           #take more time to compute
  
  cPatches <- extractRandC1Patches(cI$train_pos, numPatchSizes, 
                                   numPatchesPerSize, patchSizes) #fix: extracting from positive only
  
  totaltimespentextractingPatches <- Sys.time() - readPatchesStartTime
  cat('Time spent extracting patches (seconds): ', totaltimespentextractingPatches, '\n')
} else {
  #TODO
  stop('Reading patches from file not implemented yet!')
}

#----Settings for Testing --------#
rot <- c(90, -45, 0, 45)
c1ScaleSS <- seq(1, 18, 2)
RF_siz    <- seq(7, 39, 2)
c1SpaceSS <- seq(8, 22, 2)
minFS     <- 7
maxFS     <- 39
div       <- seq(4, 3.2, -0.05)
Div       <- div
#--- END Settings for Testing --------#

print('Initializing gabor filters -- full set...')
#creates the gabor filters use to extract the S1 layer
result_init_gabor  <- init_gabor(rot, RF_siz, Div)
fSiz                <- result_init_gabor[[1]]
filters             <- result_init_gabor[[2]]
c1OL                <- result_init_gabor[[3]]
numSimpleFilters    <- result_init_gabor[[4]]
print('done')

#The actual C2 features are computed below for each one of the training/testing directories
C2res <- list()
c2StartTime <- Sys.time()
for (i in 1:4) {
  C2res[[i]] = extractC2forcell(filters,fSiz,c1SpaceSS,c1ScaleSS,c1OL,cPatches,cI[[i]],numPatchSizes)
  
  totaltimespentextractingC2 <- Sys.time() - c2StartTime
  cat('Time spent extracting C2[',i, '] :', as.numeric(totaltimespentextractingC2,units="secs"), ' \n')
}

#Classification
trainSetX = cbind(C2res[[1]], C2res[[2]])
testSetX =  cbind(C2res[[3]], C2res[[4]])
trainSetY = matrix(c(rep(1, dim(C2res[[1]])[2]), rep(-1, dim(C2res[[2]])[2])), ncol = 1)
testSetY = matrix(c(rep(1, dim(C2res[[3]])[2]), rep(-1, dim(C2res[[4]])[2])), ncol = 1)

ry = c()
if (useSVM) {
	ry = SVMClassificate(trainSetX, testSetX, trainSetY)
} else {
  ry = NNClassificate(trainSetX, testSetX, trainSetY)
}

successrate = mean(as.integer(testSetY == ry)) #a simple classification score
print(successrate)