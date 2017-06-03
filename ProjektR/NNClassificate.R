NNClassificate = function(trainSetX, testSetX, trainSetY) {
  library('class')
  knnResult = knn(t(trainSetX), t(testSetX), factor(trainSetY))
  return(as.integer(as.vector(knnResult)))
}