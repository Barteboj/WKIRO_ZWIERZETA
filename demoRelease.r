#demoRelease.r
#demonstrates how to use C2 standard model features in a pattern classification framework

rm(list = ls()) #optional clear workspace

#dependencies:
source('readAllImages.r')
source('extractRandC1Patches.r')
source('init_gabor.r')
source('extractC2forcell.r')

#TODO #put your own path to osusvm here

useSVM <- FALSE #if you do not have osusvm installed you can turn this
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
  tic <- Sys.time()
  numPatchesPerSize <- 10  #more will give better results, but will
                           #take more time to compute
  
  cPatches <- extractRandC1Patches(cI$train_pos, numPatchSizes, 
                                   numPatchesPerSize, patchSizes) #fix: extracting from positive only
  
  totaltimespectextractingPatches <- Sys.time() - tic
  t_str <- as.numeric(totaltimespectextractingPatches, units = "secs")
  print(paste('extracting patches takes ', t_str, ' seconds'))
} else {
  stop('NOT IMPLEMENTED')
  #TODO
  #print('reading patches');
  #cPatches = load('PatchesFromNaturalImages250per4sizes','cPatches');
  #cPatches = cPatches.cPatches;
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
init_gabor_ret_val  <- init_gabor(rot, RF_siz, Div)
fSiz                <- init_gabor_ret_val[[1]]
filters             <- init_gabor_ret_val[[2]]
c1OL                <- init_gabor_ret_val[[3]]
numSimpleFilters    <- init_gabor_ret_val[[4]]
print('done')

#The actual C2 features are computed below for each one of the training/testing directories
totaltimespectextractingC2 <- 0
C2res <- list()
tic <- Sys.time()
for (i in 1:4) {
  C2res[[i]] = extractC2forcell(filters,
                                fSiz,
                                c1SpaceSS,
                                c1ScaleSS,
                                c1OL,
                                cPatches,
                                cI[[i]],
                                numPatchSizes)
  extractC2forcellTime <- Sys.time() - tic
  t_str <- as.numeric(extractC2forcellTime, units = "secs")
  totaltimespectextractingC2 <- t_str + totaltimespectextractingC2
  print(paste('extracting C2 takes ', t_str, ' seconds'))
}

#%Simple classification code
#XTrain = [C2res{1} C2res{2}]; %training examples as columns 
#XTest =  [C2res{3},C2res{4}]; %the labels of the training set
#ytrain = [ones(size(C2res{1},2),1);-ones(size(C2res{2},2),1)];%testing examples as columns
#ytest = [ones(size(C2res{3},2),1);-ones(size(C2res{4},2),1)]; %the true labels of the test set
#if useSVM
#Model = CLSosusvm(XTrain,ytrain);  %training
#[ry,rw] = CLSosusvmC(XTest,Model); %predicting new labels
#else %use a Nearest Neighbor classifier
#Model = CLSnn(XTrain, ytrain); %training
#[ry,rw] = CLSnnC(XTest,Model); %predicting new labels
#end  
#successrate = mean(ytest==ry) %a simple classification score
