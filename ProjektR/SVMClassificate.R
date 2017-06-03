SVMClassificate = function(trainSetX, testSetX, trainSetY) {
  library('e1071')
  svmResult = svm(t(trainSetX), factor(trainSetY))
  prediction = predict(svmResult, t(testSetX))
  return(as.integer(as.vector(prediction)))
}